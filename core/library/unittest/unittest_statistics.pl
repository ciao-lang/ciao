:- module(unittest_statistics,
        [
            get_statistical_summary/2,
            print_statistical_summary/1,
            statistical_summary/1
        ],
        [assertions]).

:- use_module(engine(messages_basic), [display_list/1]).
:- use_module(engine(messages_basic), [message/2]).
:- use_module(library(lists),  [length/2]).
:- use_module(library(llists), [flatten/2]).
:- use_module(library(format), [sformat/3]).

:- doc(title,  "Testing statistics").
:- doc(author, "Alvaro Sevilla San Mateo").
:- doc(author, "Jos@'{e} Luis Bueno").

:- doc(stability,beta).

:- doc(module, "This module implements predicates for generating
       statistical summaries for the testing processes.").

:- pred statistical_summary(IdxTestSummaries) : list
# "Makes the statistic summary with the results of the
   tests. @var{IdxTestSummaries} contains a list of terms with the
   results of the tests.".
% (see unittest_summaries for documentation)

statistical_summary(IdxTestSummaries0) :-
    flatten(IdxTestSummaries0, IdxTestSummaries),
    get_statistical_summary(IdxTestSummaries, Stats),
    Stats = stats(NTotal,_,_,_,_,_,_),
    NTotal > 0, !,
    print_statistical_summary(Stats).
statistical_summary(_). % reached if NTotal=0. Print something?

% ---------------------------------------------------------------------------

:- pred get_statistical_summary(IdxTestSummaries, Stats)
%% Stats0 = stats(NTotal0, NSuccess0, NFail0, NFailPre0, NAborted0, NTimeout0, NErrors0)
%% Stats  = stats(NTotal, NSuccess,  NFail,  NFailPre,  NAborted,  NTimeout,  NRTCErrors)

# "Narrow the information of the tests and generate the statistical
   information structure needed to perform the statistical summary.
   @var{IdxTestSummaries} contains a list of terms with the results of
   tests. ".

% TODO: define type stats(NSuccess, NFail, NFailPre, NAborted, NTimeout, NErrors), all ints

get_statistical_summary(IdxTestSummaries0, Stats) :-
    flatten(IdxTestSummaries0, IdxTestSummaries),
    statistical_filter(IdxTestSummaries, stats(0,0,0,0,0,0,0), Stats).

statistical_filter([], Stats0, Stats) :-
    update_total(Stats0, Stats).
statistical_filter([_-TestSummary|TSs], Stats0, Stats) :-
    inc_stat(total,Stats0,Stats1),
    update_summary(TestSummary, Stats1, Stats2),
    statistical_filter(TSs, Stats2, Stats).

update_total(stats(_NTotal, NSuccess,NFail,NFailPre,NAborted,NTimeout,NRTCErrors),
             stats(NTotal, NSuccess,NFail,NFailPre,NAborted,NTimeout,NRTCErrors)) :-
    NTotal is NSuccess+NFail+NFailPre+NAborted+NTimeout.

% NOTE: The order here is important. Statistics aggregate information
%   for each test, not for each test result.
update_summary(Summ, Stats0, Stats) :- contains_status(aborted, Summ), !,
    inc_stat(aborted,Stats0,Stats).
update_summary(Summ, Stats0, Stats) :- contains_status(fail(precondition), Summ), !,
    inc_stat(fail_pre,Stats0,Stats).
update_summary(Summ, Stats0, Stats) :- contains_status(exception(precondition,_), Summ), !,
    inc_stat(aborted,Stats0,Stats). % or increase_fail_precondition?
update_summary(Summ, Stats0, Stats) :- contains_status(exception(postcondition,_), Summ), !,
    inc_stat(aborted,Stats0,Stats).
update_summary(Summ, Stats0, Stats) :- contains_status(timeout, Summ), !,
    inc_stat(timeout,Stats0,Stats). 
% At least one runtime-check error occurred during testing
update_summary(Summ, Stats0, Stats) :- count_rtc_errors(Summ,N), N > 0, !,
    inc_stat(failed,Stats0,Stats1),
    inc_stat_n(rtchecks,Stats1,N,Stats).
% setup/cleanup
update_summary(Summ, Stats0, Stats) :- contains_status(exception(setup,_), Summ), !,
    inc_stat(aborted,Stats0,Stats).
update_summary(Summ, Stats0, Stats) :- contains_status(fail(setup), Summ), !,
    inc_stat(aborted,Stats0,Stats).
update_summary(Summ, Stats0, Stats) :- contains_status(exception(cleanup,_), Summ), !,
    inc_stat(aborted,Stats0,Stats).
update_summary(Summ, Stats0, Stats) :- contains_status(fail(cleanup), Summ), !,
    inc_stat(aborted,Stats0,Stats).
% Anything else is a success
update_summary(_, Stats0, Stats) :-
    inc_stat(success,Stats0,Stats).

contains_status(Status, Summ) :-
    member(t_res(_, _, _, Status,_,_), Summ), !.

count_rtc_errors(Summ,N) :-
    count_rtc_errors_(Summ,0,N).

count_rtc_errors_([],N,N).
count_rtc_errors_([t_res(_, RTCErrors, _, _, _, _)|Summ],Acc0,N) :-
    length(RTCErrors,K),
    Acc is Acc0 + K,
    count_rtc_errors_(Summ,Acc,N).

inc_stat(Stat, Stats0, NewStats) :-
    inc_stat_n(Stat, Stats0, 1, NewStats).


inc_stat_n(total, stats(NTotal0, NSuccess,NFail,NFailPre,NAborted,NTimeout,NRTCErrors), N,
           stats(NTotal, NSuccess,NFail,NFailPre,NAborted,NTimeout,NRTCErrors)) :-
    NTotal is NTotal0+N.
inc_stat_n(success, stats(NTotal, NSuccess0,NFail,NFailPre,NAborted,NTimeout,NRTCErrors), N,
           stats(NTotal, NSuccess,NFail,NFailPre,NAborted,NTimeout,NRTCErrors)) :-
    NSuccess is NSuccess0+N.
inc_stat_n(failed, stats(NTotal, NSuccess,NFail0,NFailPre,NAborted,NTimeout,NRTCErrors), N,
           stats(NTotal, NSuccess,NFail,NFailPre,NAborted,NTimeout,NRTCErrors)) :-
    NFail is NFail0+N.
inc_stat_n(fail_pre, stats(NTotal, NSuccess,NFail,NFailPre0,NAborted,NTimeout,NRTCErrors), N,
           stats(NTotal, NSuccess,NFail,NFailPre,NAborted,NTimeout,NRTCErrors)) :-
    NFailPre is NFailPre0+N.
inc_stat_n(aborted, stats(NTotal, NSuccess,NFail,NFailPre,NAborted0,NTimeout,NRTCErrors), N,
           stats(NTotal, NSuccess,NFail,NFailPre,NAborted,NTimeout,NRTCErrors)) :-
    NAborted is NAborted0+N.
inc_stat_n(timeout, stats(NTotal, NSuccess,NFail,NFailPre,NAborted,NTimeout0,NRTCErrors), N,
           stats(NTotal, NSuccess,NFail,NFailPre,NAborted,NTimeout,NRTCErrors)) :-
    NTimeout is NTimeout0+N.
inc_stat_n(rtchecks, stats(NTotal, NSuccess,NFail,NFailPre,NAborted,NTimeout,NRTCErrors0), N,
           stats(NTotal, NSuccess,NFail,NFailPre,NAborted,NTimeout,NRTCErrors)) :-
    NRTCErrors is NRTCErrors0+N.

% ----------------------------------------------------------------------------------------

:- pred print_statistical_summary(Stats) # "Prints the statistical
summary of the test, previously computed and given as input
@var{Stats}. ".

print_statistical_summary(Stats) :-
    Stats = stats(NTotal, NSuccess,NFail,NFailPre,NAborted,NTimeout,NRTCErrors),
    sformat(S, "Passed: ~w (~2f\%) Failed: ~w (~2f\%) " ||
        "Precond Failed: ~w (~2f\%) Aborted: ~w (~2f\%) " ||
        "Timeouts: ~w (~2f\%) " ||
        "Total: ~w Run-Time Errors: ~w~n}",
        [
            NSuccess, 100*NSuccess/NTotal,
            NFail, 100*NFail/NTotal,
            NFailPre, 100*NFailPre/NTotal,
            NAborted, 100*NAborted/NTotal,
            NTimeout, 100*NTimeout/NTotal,
            NTotal,
            NRTCErrors
        ]),
    message(note, ['{Total:\n',$$(S)]).
print_statistical_summary(_). % reached if NTotal=0. Print something?

% -------------------------------------------------------------------------

% TODO: IC: I have kept the original stats, but we should redesign
% which ones to output (e.g., precondition_failed probably is not that
% relevant, and should not even occur, better to just warn when it
% happens).
%
% MH: Actually, it indeed is something that the user does
% not intend but if we do not detect it and report it it is hard to
% spot these errors: they show up as tests that succeed even if they
% have not even run.
