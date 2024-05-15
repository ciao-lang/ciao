:- module(_, [], [assertions, regtypes, fsyntax, hiord, datafacts]).

%! \title Unittest runner (common)
%
%  \module Actual implementation of the unittest runner (usable as a
%  module in the same process).

:- use_module(library(compiler), [use_module/2]).
:- use_module(library(lists), [member/2]).
:- use_module(library(streams), [open/3, close/1, flush_output/1]).
:- use_module(library(assertions/assrt_lib), [assertion_body/7]).

:- use_module(library(unittest/unittest_db), [
    load_runtest_input/1,
    runtest_db/4
]).

% ---------------------------------------------------------------------------

:- doc(test_option/1,"A global option that controls the
    testing system. The current set of options is:

    @begin{itemize}

    @item @tt{stdout(dump)}: Save and show stdout as test results (default)
    @item @tt{stdout(save)}: Save stdout as test output, do not show
    @item @tt{stdout(null)}: Ignore stdout
    @item @tt{stderr(Opt)}: (same as above)
    @item @tt{stderr(stdout)}: redirect stderr to stdout

    @item @tt{nostats}: Do not show summary in tests results
    @item @tt{onlystats}: Only show summary in tests results

    @item @tt{saved_vers}: Use saved (regression) version in show summary

    @item @tt{rtc_entry}: Force run-time checking of at least exported
      assertions even if the @tt{runtime_checks} flag has not been
      activated in the tested module. This is useful for testing
      modules without runtime checks enabled.

    @item @tt{dir_rec}: Run tests in a specified directory
      recursively. You can indicate that the modules in a
      sub-directory should not be tested by placing an empty
      @tt{NOTEST} file in that sub-directory.  Also, if a
      @tt{NOTESTFILES} file is present, containing patterns for
      modules, those modules will not be tested.

    @item @tt{sameproc}: Use same process to run the tests (note: use
      with care, aborted tests may interrupt the whole process).

    @end{itemize}").

:- export(test_option/1).
:- regtype test_option(Opt) # "@var{Opt} is a testing option.".

test_option := stdout(~test_redirect_opt).
test_option := stderr(~test_redirect_opt).
test_option := nostats | onlystats.
test_option := saved_vers.
test_option := rtc_entry.
test_option := dir_rec.
test_option := sameproc.

% stdout/stderr redirection
test_redirect_opt := save % save (for regression). Default
                   | dump % save then show
                   | null % throw away
                   | stdout. % redirect to stdout (only for stderr)

:- export(runner_test_option/1).
:- regtype runner_test_option(Opt) # "@var{Opt} is a testing option.".
% Expected options in GOpts:
%   wrpmods(Mods): List of Wrappers for modules under test.
%   dir(TestRunDir): Temporary directory for test files.
%   timeout(Timeout): Default timeout for tests.
%   resume_after(TestId): Optional argument to skip tests until test with id TestId.
%   suff(Suff): Optional argument to set optional suffix (internals:opt_suff/1) in runner
%   stdout(Mode): stdout redirection mode
%   stderr(Mode): stderr redirection mode
%   other unittest driver options (unittest:test_option/1) might be passed down to the runner aside from stdout and stderr options. They are ignored.

runner_test_option := ~test_option.
runner_test_option := dir(~atm). % TestRunDir for aux data
runner_test_option := suff(~atm). % opt_suff
runner_test_option := wrpmods(_). % wrapper modules
runner_test_option := timeout(_). % default timeout

% ---------------------------------------------------------------------------

:- data compilation_error/0.
:- data skip_tests_before/1.
:- data default_timeout/1.

cleanup_state :-
    retractall_fact(compilation_error),
    retractall_fact(skip_tests_before(_)),
    retractall_fact(default_timeout(_)).

:- export(unittest_runner_main/2).
:- meta_predicate unittest_runner_main(?, pred(1)).
:- pred unittest_runner_main(RunnerOpts, SendData) : list(runner_test_option) * term.
% Predicate for using the runner as a module in the same process.
unittest_runner_main(GOpts, SendData) :-
    cleanup_state,
    ( % (failure-driven loop)
      member(Opt, GOpts),
        runner_opt(Opt),
        fail
    ; true
    ),
    ( member(wrpmods(WrpModules), GOpts) -> true ; throw(error_no_wrpmods) ),
    ( member(dir(TestRunDir), GOpts) -> true ; throw(error_no_dir) ),
    import_modules(WrpModules),
    ( retract_fact(compilation_error) ->
        SendData(comp_error) % error in import modules
    ; load_runtest_input(TestRunDir), % asserts test inputs as runtest_db/4
      runtests(TestRunDir, GOpts, SendData)
    ).

runner_opt(resume_after(TestId)) :- set_fact(skip_tests_before(TestId)).
runner_opt(timeout(Timeout)) :- set_fact(default_timeout(Timeout)).
runner_opt(suff(Suff)) :- opt_suffix(_,Suff). % TODO: save and restore old suffix? (at least for sameproc)

% TODO: distinguish which is the module that does not compile?
import_modules(_) :- compilation_error, !. % (stop here)
import_modules([]).
import_modules([M|Ms]) :-
    OutChn = null, % (redirect, imports may run initialization code) % TODO: show later?
    ErrChn = none,
    intercept(
        call_with_std_redirect(use_module(M,[]), OutChn, ErrChn), % we only care about multifiles test_check_pred/3 and test_entry/3
        compilation_error,
        assertz_fact(compilation_error)
    ),
    import_modules(Ms).

% TODO: multifile or bypass the module system?
:- multifile test_entry/3.
:- multifile test_check_pred/3.

:- meta_predicate runtests(?, ?, pred(1)).
runtests(TestRunDir, GOpts, SendData) :-
    ( % (failure-driven loop)
      get_active_test(TestId, Module, Options, Body),
      assertion_body(Pred,_,_,_,_,_,Body),
        run_one_test(TestId, TestRunDir, GOpts,
                     test_entry(Module,TestId,Pred), % calls field of Pred
                     test_check_pred(Module,TestId,Pred), % rtcheck version of Pred
                     Options, SendData),
        ( timed_out -> !, % (end loop)
            SendData(resume_after(TestId))
        ; fail % (loop)
        )
    ; true
    ).

% ---------------------------------------------------------------------------

% TODO: postcondition failure treating?
% TODO: how rtchecks deals with exceptions in pre/postconditins
% TODO: move (parts of) this lib into rtchecks?

% ----------------------------------------------------------------------

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(pathnames),  [path_concat/3]).
:- use_module(library(io_port_reify), [call_with_std_redirect/3]).
:- use_module(library(between), [between/3]).
:- use_module(library(assertions/assrt_lib), [assertion_body/7]).
:- use_module(library(bundle/bundle_paths), [bundle_shorten_path/2]).
:- use_module(library(compiler/c_itf), [opt_suffix/2]).

% ----------------------------------------------------------------------

% TODO: simplify
:- data active_test/0.

get_active_test(TestId, Mod, Options, Body) :-
    ( \+ skip_tests_before(_) ->
        % enum over tests
        runtest_db(TestId,Mod,Options,Body)
    ; % skip test and enum over the rest of tests 
      retractall_fact(active_test),
      runtest_db(TestId0,Mod0,Options0,Body0),
      ( skip_tests_before(TestId0) ->
          assertz_fact(active_test), fail
      ; true
      ),
      active_test,
      TestId = TestId0,
      Mod = Mod0,
      Options = Options0,
      Body = Body0
    ).

% ----------------------------------------------------------------------
% Test case execution (timeout, call cases, try sols)

:- data timed_out/0.
:- data rtcheck_db/1.
:- data signals_db/1.

% TODO: document: time limit includes test cases and try_sols

:- meta_predicate run_one_test(?, ?, ?, goal, goal, ?, pred(1)).
run_one_test(TestId, TestRunDir, GOpts, Precond, Pred, Options, SendData) :-
    get_option(timeout,Options,Timeout),
    get_option(generate_from_calls_n,Options,NCases),
    get_option(try_sols,Options,NSols),
    set(maxcase, NCases),
    set(maxsol, NSols),
    set(ncase, 0),
    set(nsol, 0), % (reset just in case of timeouts in generation)
    %
    SendData(runner_begin_test(TestId,Timeout)),
    call_with_timeout(Timeout, run_one_test_(TestId, TestRunDir, GOpts, Precond, Pred, Options, SendData)),
    SendData(runner_end_test(TestId)).

:- meta_predicate run_one_test_(?, ?, ?, goal, goal, ?, pred(1)).
run_one_test_(TestId, TestRunDir, GOpts, Precond, Pred, Options, SendData) :-
    ( % (failure-driven loop)
      run_one_test__(TestId, TestRunDir, GOpts, Precond, Pred, Options, TRes),
        SendData(runner_output(TestId, TRes)),
        ( timed_out -> ! % (end loop)
        ; fail % (loop)
        )
    ; true
    ).

:- meta_predicate run_one_test__(?, ?, ?, goal, goal, ?, ?).
run_one_test__(TestId, TestRunDir, GOpts, Precond, Pred, Options, TRes) :-
    retractall_fact(rtcheck_db(_)),
    retractall_fact(signals_db(_)),
    intercept(
        gen_test_case_and_run(TestId, TestRunDir, GOpts, Precond, Pred, Options, Result0, Stdout, Stderr),
        E,
        handle_signal(E)
    ),
    findall(E0, retract_fact(signals_db(E0)), Signals),
    findall(RTCError, retract_fact(rtcheck_db(RTCError)), RTCErrors),
    result_id(ResultId),
    test_result(Result0, TestId, Result),
    TRes = t_res(ResultId, RTCErrors, Signals, Result, Stdout, Stderr),
    ( Result = timeout -> set_fact(timed_out) ; true ).

handle_signal(control_c) :- !, % used for timeouts
    send_signal(control_c).
handle_signal(RTError0) :-
    RTError0 = rtcheck(Type, Pred, Dict, Prop, Valid, Poss0), !,
    short_paths(Poss0,Poss),
    RTError = rtcheck(Type, Pred, Dict, Prop, Valid, Poss),
    assertz_fact(rtcheck_db(RTError)),
    throw(rtcheck(RTError)).
% Flag for other possible behaviours after rtcheck instead of throw? (e.g., fail or continue)
handle_signal(E) :-
    assertz_fact(signals_db(E)).

short_paths([],[]).
short_paths([Loc0|Locs0],[Loc|Locs]) :-
    short_path(Loc0,Loc),
    short_paths(Locs0,Locs).

short_path(predloc(Pred, Loc0), predloc(Pred, Loc)) :- !, short_loc(Loc0,Loc).
short_path(callloc(Pred, Loc0), callloc(Pred, Loc)) :- !, short_loc(Loc0,Loc).
short_path(litloc(Lit, Loc0-(Pred)), litloc(Lit, Loc-(Pred))) :- !, short_loc(Loc0,Loc).
short_path(asrloc(Loc0), asrloc(Loc)) :- !, short_loc(Loc0,Loc).
short_path(pploc(Loc0), pploc(Loc)) :- !, short_loc(Loc0,Loc).
short_path(P,P). % needed?
% TODO: do this in rtchecks, error detection works already with short paths

short_loc(loc(Src0, LB, LE), loc(Src, LB, LE)) :- bundle_shorten_path(Src0,Src).

% Capture unexpected failure or exceptions, turn status to 'true' if
% we specify a failure or exception behaviour.
test_result(fail(predicate), TestId, true) :-
    runtest_db(TestId,_,_,Body),
    assertion_body(_,_,_,_,Comp,_,Body),
    member(C,Comp),failure_comp(C), !.
test_result(exception(predicate,_), TestId, true) :-
    runtest_db(TestId,_,_,Body),
    assertion_body(_,_,_,_,Comp,_,Body),
    member(C,Comp),exception_comp(C), !.
test_result(Status, _, Status).

failure_comp(fails(_)).
failure_comp(possibly_fails(_)).
exception_comp(exception(_)).
exception_comp(exception(_,_)).
exception_comp(possible_exceptions(_,_)).

% (Handle test cases, setup, and call the actual test with stdout/stderr redirection) (nondet)
:- meta_predicate gen_test_case_and_run(?, ?, ?, goal, goal, ?, ?, ?, ?).
gen_test_case_and_run(TestId, TestRunDir,GOpts,Precond,Pred,Options,Result,Stdout,Stderr) :-
    test_redirect_chns(TestRunDir, GOpts, OutChn, ErrChn),
    cnt(maxcase, NCases),
    cnt(maxsol, NSols),
    gen_test_case(NCases,Precond,Result),
    inc(ncase),
    set(nsol, 0),
    ( nonvar(Result) -> Stdout=[], Stderr=[] % some error in generation, returned as Result
    ; run_test_custom(setup,TestId,Options,Result),
      ( nonvar(Result) -> Stdout=[], Stderr=[] % some error in setup
      ; call_with_std_redirect(run_test(NSols,Pred,Result), OutChn, ErrChn),
        test_redirect_contents(OutChn, ErrChn, Stdout, Stderr)
      ),
      run_test_custom(cleanup,TestId,Options,ResultCleanup), % TODO: must be done once, outside
      ( nonvar(Result) -> true % some error in run_test
      ; ( nonvar(ResultCleanup) -> Result = ResultCleanup % some error cleanup
        ; Result = true % OK!
        )
      )
    ),
    inc(nsol).

% TODO: output and statistics for generate_from_calls_n(N)?
% TODO: deprecate for users, reuse for better integration with assertion-based testing?
:- meta_predicate gen_test_case(?,goal,?).
gen_test_case(NCases,Precond,Result) :-
    catch(
        backtrack_n_times(Precond,NCases,not_n_cases_reached(Result)),
        PrecEx,
        gen_test_case_exception(PrecEx, Result)
    ).

not_n_cases_reached(Result,0) :- !, Result = fail(precondition).

% gen_test_case_exception(time_limit_exceeded, timeout).
gen_test_case_exception(PrecEx, exception(precondition, PrecEx)).

% (Handle number of solutions)
:- meta_predicate run_test(?,goal,?).
run_test(NSols,Pred,Result) :-
    catch(
        backtrack_n_times(Pred,NSols,not_n_sols_reached(Result)),
        Ex,
        run_test_exception(Ex,Result)
    ).
% TODO: coverage warnings when there are more than NSols solutions?

not_n_sols_reached(Result, 0) :- !, Result = fail(predicate).

% rtchecks/1 exceptions, thrown by rtcheck/6 signal handler
run_test_exception(rtcheck(_RtcError), rtcheck_error) :- !. % _RtcError saved elsewhere
run_test_exception(postcondition(rtcheck(_RtcError)), rtcheck_error) :- !. % can this actually happen?
% time_limit_exceeded exceptions,
run_test_exception(postcondition(time_limit_exceeded),timeout) :- !. % TODO: distinguish timeout(postcondition)?
run_test_exception(time_limit_exceeded,timeout) :- !.
% predicate exceptions
run_test_exception(postcondition(PostEx),exception(postcondition,PostEx)) :- !.
run_test_exception(Ex,exception(predicate,Ex)).
% TODO: do we really need that much to distinguish between an
% exception in the postcondition and a normal exception?

run_test_custom(What, TestId, Options, Result):-
    get_option(What, Options, G),
    ( G = true -> true
    ; runtest_db(TestId, Mod, _, _),
      qualify_goal(Mod, G, MG),
      catch(call(MG), E, exception_custom(What, E, Result)) -> true
    ; Result = fail(What)
    ).

exception_custom(What, E, exception(What, E)).

:- use_module(engine(system_info), [get_arch/1]).

get_option(Opt,Options,Value) :-
    functor(Option,Opt,2),
    member(Option,Options), !,
    arg(2,Option,Value).
get_option(times,_,1).
get_option(try_sols,_,2). % enough to capture determinism
get_option(timeout,_,Timeout) :- get_arch(wasm32), !, % TODO: fix timeout for this arch
    Timeout = 0.
get_option(timeout,_,Timeout) :-
    default_timeout(Timeout).
get_option(generate_from_calls_n,_,1).
%
get_option(setup, _, true). % (default)
get_option(cleanup, _, true). % (default)

% ---------------------------------------------------------------------------
%! ## Auxiliary

% TODO: move somewhere else
%
:- use_module(engine(internals),['$setarg'/4]).

% Limit backtracking of Goal to N solutions, and optionally call
% Handler if there are less than N solutions.
:- meta_predicate backtrack_n_times(goal,?,pred(1)).
backtrack_n_times(Goal,N,Handler) :-
    N > 0,
    NSol = nsol(0),
    backtrack_n_times_(NSol,N,Goal,Handler).

:- meta_predicate backtrack_n_times(?, ?, goal, goal).
backtrack_n_times_(NSol,NSought,Goal,_) :-
    call(Goal),
    NSol=nsol(N),
    N1 is N+1,
    '$setarg'(1,NSol,N1,true),
    (NSol=nsol(NSought), ! ; true).
backtrack_n_times_(nsol(NFound),_,_,Handler) :-
    Handler(NFound).

:- compilation_fact(use_soft_timeout). % TODO: use only one?

:- meta_predicate call_with_timeout(?,goal).
:- if(defined(use_soft_timeout)).
% NOTE: This is a "soft" timeout (that may not stop the execution in
%   all the cases). See maybe_input_wait/2 for the more robust
%   mechanism (when running as a separate process).
:- use_module(library(timeout), [call_with_time_limit/3]).
call_with_timeout(0, G) :- !, call(G).
call_with_timeout(T, G) :-
    % (time_limit_exception is caught and handled in an inner catch/3)
    call_with_time_limit(T, G, _).
:- else.
call_with_timeout(_, G) :- call(G).
:- endif.

:- use_module(engine(internals), [module_concat/3]).
% TODO: this should not be needed once unittest handles module-resolved assertions 
qualify_goal(Mod, Goal, MGoal) :-
    Goal =.. [N|As],
    module_concat(Mod, N, ModN),
    MGoal0 =.. [ModN|As],
    MGoal = '$:'(MGoal0).

% ---------------------------------------------------------------------------
% Test result counters

:- data cnt/2.

result_id(result_id(Case,MaxCase,Sol,MaxSol)) :-
    cnt(ncase,Case),
    cnt(maxcase,MaxCase),
    cnt(nsol,Sol),
    cnt(maxsol,MaxSol).

inc(V) :- cnt(V,N), N1 is N+1, set(V, N1).

set(V, N) :- retractall_fact(cnt(V,_)), assertz_fact(cnt(V,N)).

:- export(next_result_id/2). % (for amend)
next_result_id(result_id(Case,MaxCase,Sol,MaxSol),
               result_id(Case2,MaxCase,Sol2,MaxSol)) :-
    ( Sol < MaxSol -> Sol2 is Sol + 1, Case2 = Case
    ; Sol2 = 1, Case2 is Case + 1
    ).

% ---------------------------------------------------------------------------

:- use_module(library(unittest/unittest_db), [file_runtest_redirect/3]).
:- use_module(library(stream_utils), [file_to_string/2]).

% Test redirection channels (for call_with_std_redirect/3)
test_redirect_chns(TestRunDir, GOpts, OutChn, ErrChn) :-
    test_redirect_chn(TestRunDir, GOpts, stdout, OutChn),
    test_redirect_chn(TestRunDir, GOpts, stderr, ErrChn).

test_redirect_chn(TestRunDir, GOpts, Std, Chn) :-
    ( redirect_opt(Std, Opt, Mode0), member(Opt, GOpts) -> Mode = Mode0
    ; Mode = dump % (default)
    ),
    test_redirect_chn_(Mode, Std, TestRunDir, Chn).

redirect_opt(stdout, stdout(Mode), Mode).
redirect_opt(stderr, stderr(Mode), Mode).

test_redirect_chn_(dump, Std, TestRunDir, Chn) :-
    test_redirect_chn_(save, Std, TestRunDir, Chn).
test_redirect_chn_(save, Std, TestRunDir, file(File)) :-
    file_runtest_redirect(Std, TestRunDir, File).
test_redirect_chn_(null, _, _, null).
test_redirect_chn_(stdout, _, _, stdout).

% Read contents of redirection
test_redirect_contents(OutChn, ErrChn, Out, Err) :-
    test_redirect_contents_(OutChn, Out),
    test_redirect_contents_(ErrChn, Err).

test_redirect_contents_(file(File), Str) :- !,
    file_to_string(File, Str).
test_redirect_contents_(_, []).

% ---------------------------------------------------------------------------

:- export(runner_recover_aborted/5).
runner_recover_aborted(TestRunDir, Options, ResultId, Result, TRes) :-
    % recover and possibly dump stdout,stderr until crash
    test_redirect_chns(TestRunDir, Options, OutChn, ErrChn),
    test_redirect_contents(OutChn, ErrChn, StdoutStr, StderrStr),
    % mark the test as aborted
    fill_aborted_test(ResultId, Result, StdoutStr, StderrStr, TRes).

:- export(fill_aborted_test/5).
fill_aborted_test(ResultId, Result, StdoutStr, StderrStr, TRes) :-
    TRes = t_res(ResultId, [], [], Result, StdoutStr, StderrStr).

% ---------------------------------------------------------------------------
%! # Entry for calling the runner as a separate process

:- use_module(library(unittest/unittest_db), [
    read_data/2,
    write_data/2
]).

:- export(unittest_runner_batch/0).
:- pred unittest_runner_batch # "Main predicate for using the runner
   as a separate process (see unittest_runner). Do not use
   directly.".

unittest_runner_batch :-
    read_data(user_input, runner_opts(Opts)),
    unittest_runner_main(Opts, stdout_send_data).

% Use stdout for sending messages to the unittest driver
stdout_send_data(Data) :-
    write_data(user_output, Data),
    flush_output(user_output).
