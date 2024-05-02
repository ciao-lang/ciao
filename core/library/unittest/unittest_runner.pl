:- module(_, [], [assertions, regtypes, isomodes, nativeprops, dcg, fsyntax, hiord, datafacts, define_flag]).

%! \title Unittest runner (client)
%
%  \module Client for the unittest runner (sameproc and separate process).

% TODO: add option to keep the runner alive between modules; merge
%   with ciaopp_batch, active modules, etc.

:- use_module(engine(stream_basic)).
:- use_module(engine(messages_basic), [message/2]).
:- use_module(library(process), [process_join/1, process_kill/1]).
:- use_module(ciaobld(cpx_process), [cpx_process_call/3]).
:- use_module(library(stream_wait), [input_wait/2, input_set_unbuf/1]).
:- use_module(engine(system_info), [get_arch/1]).
:- use_module(library(lists), [member/2]).

:- use_module(library(unittest/unittest_db), [read_data/2]).
:- use_module(library(unittest/unittest_runner_common), [
    unittest_runner_main/2, % (for same process)
    runner_recover_aborted/5,
    runner_test_option/1
]).

:- data runner_cont/1.
:- data runner_pending/2.

:- export(unittest_runner/2).
:- meta_predicate unittest_runner(?, pred(2)).
:- pred unittest_runner(RunnerOpts, TreatRes) : list(runner_test_option) * term.

unittest_runner(RunnerOpts, TreatRes) :-
    unittest_runner_(first, RunnerOpts, on_runner_msg(TreatRes)).

:- meta_predicate unittest_runner_(?, ?, pred(1)).
unittest_runner_(end, _, _) :- !.
unittest_runner_(Cont0, Opts, RecvData) :-
    invoke_runner(Cont0, Opts, RecvData, Cont),
    unittest_runner_(Cont, Opts, RecvData).

:- meta_predicate invoke_runner(?, ?, pred(1), ?).
invoke_runner(comp_error, _, _, Cont) :- !,
    % Note: compilation errors of tested modules should be detected
    % earlier (see load_tests/2).
    % TODO: In some cases (e.g., undefined predicates) we still capture this event.
    message(error, ['Compilation failed. Please make sure all relevant predicates',
                    ' and properties for testing are exported.']),
    Cont = end.
invoke_runner(unknown_timeout, _, _, Cont) :- !,
    % (This should not happen)
    message(error, ['A timeout occurred while not testing predicates!']),
    Cont = end.
invoke_runner(recover(TestId, Result), Opts, RecvData, Cont) :- !,
    % Recover data from test, resume after it
    test_recover(TestId, Result, Opts, RecvData),
    Cont = resume_after(TestId).
invoke_runner(Cont0, Opts, RecvData, Cont) :-
    ( Cont0 = resume_after(ContIdx) -> RunnerOpts = [resume_after(ContIdx)|Opts]
    ; Cont0 = first -> RunnerOpts = Opts
    ; throw(bug_wrong_cont(Cont0))
    ),
    retractall_fact(runner_cont(_)),
    retractall_fact(runner_pending(_,_)),
    % TODO: to engine stack limits
    ( use_sameproc(Opts) -> % Run in same process
        unittest_runner_main(RunnerOpts, RecvData)
    ; unittest_runner_proc(RunnerOpts, RecvData)
    ),
    ( retract_fact(runner_cont(Cont1)) -> Cont = Cont1 % use runner_cont
    ; retract_fact(runner_pending(TestId,_)) -> 
        % message(error0, ['log: recovering aborted ', ''(TestId)]),
        Cont = recover(TestId, aborted) % unfinished
    ; Cont = end % end
    ).

use_sameproc(_Opts) :- get_arch(wasm32), !. % by default when separate processes are not available 
use_sameproc(Opts) :- member(sameproc, Opts).

:- use_module(ciaobld(config_common), [cmd_path/4]).
ciaosh_exec := ~cmd_path(core, plexe, 'ciaosh').

:- meta_predicate unittest_runner_proc(?, pred(1)).
unittest_runner_proc(Opts, RecvData) :-
    % Note: we cannot use invoke_ciaosh_batch/2 since it captures stdin
    absolute_file_name(library(unittest/unittest_runner_common), Runner),
    cpx_process_call(~ciaosh_exec, ['-q', '-f', '-u', Runner, '-e', 'unittest_runner_batch'], [
        stdin(terms([runner_opts(Opts)])), % TODO: change if we modify read_data/2
        stdout(stream(MsgS)),
        status(_), % (ignore status)
        background(P)
    ]),
    % Note: use of select() requires unbuffered input, do it before any input is performed!
    % TODO: performance is not an issue at the moment, but consider other alternatives
    input_set_unbuf(MsgS),
    %
    loop_runtest_msgs(MsgS, RecvData, P),
    ( process_join(P) -> true ; true ), % TODO: fail on errors, modify process_join/1 to throw exceptions? do something?
    close(MsgS).

:- meta_predicate loop_runtest_msgs(?, pred(1), ?).
loop_runtest_msgs(MsgS, RecvData, P) :-
    repeat,
    % if there is a pending test, use its timeout
    ( runner_pending(_, Timeout) -> true ; Timeout = 0 ),
    % message(error0, ['log: waiting with timeout ', ''(Timeout)]),
    ( maybe_input_wait(MsgS, Timeout) -> % wait until we have data
        ( catch(read_data(MsgS, Term), E, read_data_err(E)) ->
            % message(error0, ['log: received ', ''(Term)]),
            RecvData(Term),
            fail % (loop)
        ; ! % (end loop)
        )
    ; !, % (end loop)
      % timeout without data, send kill signal, treat as timeout later
      ( retract_fact(runner_pending(TestId,_)) ->
          set_fact(runner_cont(recover(TestId, timeout)))
      ; set_fact(runner_cont(unknown_timeout))
      ),
      process_kill(P)
    ).

% TODO: parsing error, better message?
read_data_err(E) :- message(error0, [bug_read_data(E)]), fail.

maybe_input_wait(_Stream, 0) :- !.
maybe_input_wait(Stream, MSecs) :-
    USecs is (MSecs+500)*1000, % Note: give 0.5s extra
    input_wait(Stream, USecs).

:- meta_predicate test_recover(?, ?, ?, pred(1)).
test_recover(TestId, Result, Opts, RecvData) :-
    ( member(dir(TestRunDir), Opts) -> true
    ; throw(bug_no_dir)
    ),
    runner_recover_aborted(TestRunDir, Opts, unknown, Result, TRes),
    RecvData(runner_output(TestId, TRes)).

:- meta_predicate on_runner_msg(pred(2), ?).
on_runner_msg(TreatRes, X) :-
    ( X = resume_after(TestId) ->
        set_fact(runner_cont(resume_after(TestId)))
    ; X = comp_error ->
        set_fact(runner_cont(comp_error))
    ; X = runner_begin_test(TestId,Timeout) -> % working on TestId
        % other results for the same TestId)
        ( runner_pending(TestId0,_) -> message(error0, [bug_pending(TestId0)]) ; true ),
        set_fact(runner_pending(TestId,Timeout))
        % message(error0, [runner_begin_test(TestId,Timeout)])
    ; X = runner_end_test(TestId) -> % no longer working on TestId
        ( runner_pending(TestId0,_), TestId \== TestId0 -> message(error0, [bug_pending(TestId,TestId0)]) ; true ),
        retractall_fact(runner_pending(_,_))
        % message(error0, [runner_end_test(TestId)])
    ; X = runner_output(TestId, TRes) ->
        TreatRes(TestId, TRes)
    ; message(error0, [unknown_msg(X)])
    ).
