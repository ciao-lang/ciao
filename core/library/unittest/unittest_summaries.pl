:- module(_, [], [assertions, nativeprops, datafacts]).

:- doc(title, "Testing summaries").
% (see authors in unittest.pl)

:- doc(summary, "This module implements predicates for printing the
results of the tests.").

:- doc(module, "This module implements predicates for printing the
results of the tests. Each test has a result for each solution
generated for the predicate under test. The results can be one of the
followings:

@begin{itemize}
@item Test passed succesfully
@item Test failed
@item Test passed with warnings
  @begin{itemize}
  @item Predicate under test failed but that was allowed by the test assertions
  @item Predicate under test throwed an exception but that was allowed
  by the test assertions
  @item No test instance was generated from the calls field of the assertion
  @end{itemize}
@item Test aborted
  @begin{itemize}
  @item Test aborted for unmonitored reasons
  @item Test timed out
  @item Generation from calls field threw an exception
  @item Postcondition-checking threw an exception
  @end{itemize}
@end{itemize}

For the detailed output of each solution, the result, runtime-checks
and signals intercepted are printed.
").

% TODO: old behaviour, now we are showing one result per execution:
%
%   The output for the test is divided in a one-line summary of the test
%   results plus an optional detailed output for each test solution. In
%   the presence of multiple solutions, multiple tests instances from the
%   calls field of the test assertion, or running the test multiple times,
%   the one-line summary of the test will be the result with the higher
%   priority, according to this list: first, test aborted; second, test
%   failed; third, test passed with warnings, and finally, test passed
%   successfully.

:- use_module(library(llists), [flatten/2]).
:- use_module(library(hiordlib), [foldl/4]).
:- use_module(library(lists), [append/3, member/2, reverse/2]).
:- use_module(library(terms), [atom_concat/2]).
%
:- use_module(library(rtchecks/rtchecks_pretty), [
    pretty_prop/3,
    rtcheck_to_message/3
]).

% --------------------------------------------------------------

:- prop test_summary(X).

test_summary(TestIn-TestResults) :-
    test_in(TestIn),
    list(test_result, TestResults).

test_in(test_in(_Module, _F, _A, _Dict, _Comment, _Source, _LB, _LE)).

% The results of a test, written in an output file by the test runner,
% are a term of the form t_res(Id,RtcErrors,Signals,Result,Stdout,Stderr)
% for each solution of the predicate, where:
%
% - Id is result_id(Case,MaxCase,Sol,MaxSol), and specifies that
%   the following args refer to:
%    - the Case-th case generated from the precondition
%    - the Time-th time the test has been executed
%    - the Sol-th solution
%
% - RtcErrors is the list of runtime-check errors intercepted for that
%   solution
%
% - Signals is the list of other signals intercepted for that solution
%
% - Status is one of the following:
%
%   - aborted: The test aborted for some reason. This result is
%     added manually by unittest.pl when it finds no test results (so
%     it can not be a result of a second solution, and in particular
%     currently we don't know if a test aborted after the first
%     solution)
%
%   - fail(precondition): The precondition of the test assertion had
%     no solutions when being run, and therefore no actual goal to be
%     run for testing was generated.
%
%   - exception(Where,E): There was an exception at step Where of the
%     test. Where in {precondition, postcondition, predicate}
%
%   - fail(predicate): The predicate did not succeed for the given
%     test case
%
%   - true: The predicate succeeded for the given test case

test_result(t_res(ResultId, RtcErrors, Signal, Result, Stdout, Stderr)) :-
    result_id(ResultId),
    list(RtcErrors), % list(rtc_error,RtcErrors), rtc_error should be defined somewhere
    list(Signal),
    result(Result),
    string(Stdout),
    string(Stderr).

result_id(result_id(Case,MaxCase,Sol,MaxSol)) :-
    int(Case),
    int(MaxCase),
    int(Sol),
    int(MaxSol).

result(true).
result(fail(predicate)).
result(exception(predicate,_)).
result(fail(precondition)).
result(exception(precondition,_)).
result(exception(postcondition,_)).
result(timeout).
result(aborted).

% ---------------------------------------------------------------------------

:- export(show_test_result/4).
show_test_result(TestIn, TRes, ShowOut, ShowErr) :-
    TRes = t_res(ResultId,RTCErrors,Signals0,Result0,Stdout,Stderr),
    TestIn = test_in(_Module, _F, _A, Dict, _Comment, _Source, _LB, _LE),
    show_test_header(TestIn, ResultId, Result0),
    % TODO: show details only if verbose
    pretty_prop(Signals0, Dict, Signals),
    signals_text(Signals, SignalsMsg, []),
    ( SignalsMsg = [] -> true ; message(error0, SignalsMsg) ),
    ( % (failure-driven loop),
      member(RTCError, RTCErrors),
        rtcheck_to_message(RTCError, RTCErrorMsg, []),
        message(error0, RTCErrorMsg),
        fail
    ; true
    ),
    show_test_stdouterr(Stdout, Stderr, ShowOut, ShowErr).

show_test_header(TestIn, ResultId, Result) :-
    ( result_msg(Result, Status, Desc) -> true
    ; throw(unknown_result_bug(Result))
    ),
    TestIn = test_in(Module, F, A, _Dict, Comment, Source, LB, LE),
    module_text(Source,Module,ModuleMsg),
    descriptor_text(Comment, CommentMsg),
    Msg = [ModuleMsg|Msg0],
    ( ResultId = result_id(CaseN,MaxCase,SolN,MaxSol) -> true
    ; CaseN=1, MaxCase=1, SolN=1, MaxSol=1 % (unknown)
    ),
    ( SolN > 1 ->
        ( SolN < MaxSol -> Msg0 = ['├── (solution ', SolN, ')'|Desc]
        ; Msg0 = ['└── (solution ', SolN, ') (try_sol limit)'|Desc]
        )
    ; Msg0 = [F, '/', A, [](CommentMsg)|Msg1],
      ( MaxCase > 1 -> Msg1 = [' (case ', CaseN, ')', '.'|Desc]
      ; Msg1 = ['.'|Desc]
      )
    ),
    put_src_if_needed(Status, Source),
    message_lns(Status,LB,LE,Msg).

result_msg(aborted, aborted, [' Predicate under test aborted.']).
result_msg(fail(precondition), warning, [' Nothing tested because generation from calls field failed.']).
result_msg(exception(precondition,_), aborted, [' Nothing tested because generation from calls field aborted.']).
result_msg(exception(postcondition,_), aborted, [' Exception thrown while checking test success field.']).
result_msg(timeout, aborted, [' Time limit for the test exceeded.']).
% TODO: was TestResult = t_res(_,[_|_],_,_,_,_), OK?
result_msg(rtcheck_error, failed, []).
% TODO: do not print this if the test did specify exceptions behaviour. Include that information in TestIn
result_msg(exception(predicate, _), warning, [' There were exceptions, but test does not specify exceptions behavior.']).
% TODO: do not print this if the test did specify failure behaviour. Include that information in TestIn
result_msg(fail(predicate), warning, [' Goal tested failed, but test does not specify failure behavior.']).
result_msg(true, passed, []).

% TODO: useful?
module_text(Source,Module,'') :-
    atom_concat([_,'/',Module,'.pl'],Source), !.
module_text(_Source,Module,Text) :- % TODO: ignore it anyway?
    atom_concat(Module,':',Text).

descriptor_text("",      '') :- !.
descriptor_text(Comment, [' "', $$(Comment), '"']).

signals_text([], Msg, Msg) :- !.
signals_text(Signals, [' Signals thrown: ', ~~(Signals) | Msg], Msg).

% TODO: unify with unittest_statistics

% ---------------------------------------------------------------------------

show_test_stdouterr(Stdout, Stderr, ShowOut, ShowErr) :-
    ( % (failure-driven) loop
        % as opposed to empty, 'dumped', or 'ignored'. % TODO: messages for those
        ( ShowOut = yes, Stdout = [_|_] ->
            write_std('stdout in test', Stdout)
        ; true
        ),
        ( ShowErr = yes, Stderr = [_|_] ->
            write_std('stderr in test', Stderr)
        ; true
        ),
        fail
    ; true
    ).

:- use_module(library(stream_utils), [write_string/2]).
:- use_module(library(streams), [nl/1]).

write_std(Msg, Str) :-
    message(note, [Msg]),
    write_string(user_error, Str),
    % add newline if missing (just formatting)
    ( append(_, "\n", Str) -> true ; nl(user_error) ).

% TODO: exceptions and rtchecks intercepted by test runner are not
% further printed to error, as it would happen in a normal execution
% of the predicate

% TODO: allow showing output and error together

% ---------------------------------------------------------------------------
%! # Messages

% TODO: remove mesages_basic:messages/2

% ---------------------------------------------------------------------------
% (use c_itf_messages)

:- use_module(engine(messages_basic), [message/2, message_lns/4, message_type_visible/1]).

compiling_src(_) :- fail. % (avoid put_src if same)
doing_verbose(off).
:- include(library(compiler/c_itf_messages)).
% provides: put_src_if_needed/2, end_brace_if_needed/0

:- export(begin_messages/0).
% Prepare a message context
begin_messages :-
    retractall_fact(last_error_in_src(_)).

:- export(end_messages/0).
% End a message context
end_messages :-
    end_brace_if_needed.


