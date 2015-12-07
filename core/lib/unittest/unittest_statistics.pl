:- module(unittest_statistics,
	    [
		statistical_summary/2
		% statistical_filter/11
	    ],
	    [assertions]).

:- use_module(library(lists),  [length/2]).
:- use_module(library(llists), [flatten/2]).
:- use_module(library(format), [sformat/3]).

:- doc(title,  "Testing statistics").
:- doc(author, "Alvaro Sevilla San Mateo").

:- doc(module, "This module implements predicates for generating
       statistical summaries for the testing processes.").

:- pred statistical_summary(Tag, IdxTestSummaries) : atm * list

# "Makes the statistic summary with the results of the
   tests. @var{IdxTestSummaries} contains a list of terms with the
   results of the tests.".

statistical_summary(Tag, IdxTestSummaries0) :-
	flatten(IdxTestSummaries0, IdxTestSummaries),
	statistical_filter(IdxTestSummaries, 0, 0, 0, 0, 0,
	    NSuccess, NFail, NFailPre, NAborted, NRTCErrors),
	NTotal is NSuccess+NFail+NFailPre+NAborted,
	NTotal > 0
    ->
	sformat(S, "Passed: ~w (~2f\%) Failed: ~w (~2f\%) " ||
	    "Precond Failed: ~w (~2f\%) Aborted: ~w (~2f\%) " ||
	    "Total: ~w Run-Time Errors: ~w~n}~n",
	    [
		NSuccess, 100*NSuccess/NTotal,
		NFail, 100*NFail/NTotal,
		NFailPre, 100*NFailPre/NTotal,
		NAborted, 100*NAborted/NTotal,
		NTotal,
		NRTCErrors
	    ]),
	display_list(Tag),
	message(note, [$$(S)])
    ;
	true.

:- pred statistical_filter(IdxTestSummaries, NSuccess0, NFail0,
	    NFailPre0, NAborted0, NErrors0, NSuccess, NFail,
	    NFailPre, NAborted, NRTCErrors)
: list * int * int * int * int * int * int * int * int * int * int

# "Narrow the information of the tests and generate the statistical
   information structure needed to perform the statistical summary.
   @var{IdxTestSummaries} contains a list of terms with the results of
   tests. ".

statistical_filter([],                  NSuccess,  NFail,  NFailPre,
	    NAborted,  NRTCErrors,  NSuccess, NFail, NFailPre, NAborted,
	    NRTCErrors).
statistical_filter([_-TestSummary|TSs], NSuccess0, NFail0, NFailPre0,
	    NAborted0, NRTCErrors0, NSuccess, NFail, NFailPre, NAborted,
	    NRTCErrors) :-
	update_summary(TestSummary, NSuccess0, NFail0, NFailPre0, NAborted0,
	    NRTCErrors0, NSuccess1, NFail1, NFailPre1, NAborted1, NRTCErrors1),
	statistical_filter(TSs, NSuccess1, NFail1, NFailPre1, NAborted1,
	    NRTCErrors1, NSuccess, NFail, NFailPre, NAborted, NRTCErrors).

update_summary_each(st(_, _, aborted(_, _)), NSuccess, NFail, NFailPre,
	    NAborted0, NRTCErrors, NSuccess, NFail, NFailPre, NAborted,
	    NRTCErrors) :- !,
	NAborted is NAborted0 + 1.
update_summary_each(st(_, _, fail(precondition)), NSuccess, NFail,
	    NFailPre0, NAborted, NRTCErrors, NSuccess, NFail,
	    NFailPre, NAborted, NRTCErrors) :- !,
	NFailPre is NFailPre0 + 1.
update_summary_each(st(RTCErrors, _, _), NSuccess, NFail0, NFailPre, NAborted,
	    NRTCErrors0, NSuccess, NFail, NFailPre, NAborted, NRTCErrors) :-
	length(RTCErrors, N),
	N > 0,
	!,
	NFail is NFail0 + 1,
	NRTCErrors is NRTCErrors0 + N.
update_summary_each(_, NSuccess0, NFail, NFailPre, NAborted, NRTCErrors,
	    NSuccess, NFail, NFailPre, NAborted, NRTCErrors) :-
	NSuccess is NSuccess0 + 1.

update_summary([], NSuccess, NFail, NFailPre, NAborted, NRTCErrors, NSuccess,
	    NFail, NFailPre, NAborted, NRTCErrors).
update_summary([count(ErrorStatus, _)|TestSummary], NSuccess0, NFail0,
	    NFailPre0, NAborted0, NRTCErrors0, NSuccess, NFail, NFailPre,
	    NAborted, NRTCErrors) :-
	update_summary_each(ErrorStatus, NSuccess0, NFail0, NFailPre0,
	    NAborted0, NRTCErrors0, NSuccess1, NFail1, NFailPre1, NAborted1,
	    NRTCErrors1),
	update_summary(TestSummary, NSuccess1, NFail1, NFailPre1, NAborted1,
	    NRTCErrors1, NSuccess, NFail, NFailPre, NAborted, NRTCErrors).

