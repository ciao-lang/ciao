:- module(rtchecks_utils,
        [
            handle_rtcheck/1,
            call_rtc/1,
            save_rtchecks/1,
            load_rtchecks/1,
            rtcheck_error/1
        ],
        [assertions, regtypes]).

:- use_module(library(write), []).
:- use_module(library(debugger/debugger_lib), [tracertc/0]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(rtchecks/rtchecks_pretty),
        [
            rtcheck_to_messages/3,
            pretty_messages/1
        ]).

:- doc(author, "Edison Mera").

:- doc(module, "This module contains some useful predicates to
	facilitate work with run-time checks.").

:- regtype rtcheck_error/1 #
	"Specifies the format of a run-time check exception.".

rtcheck_error(rtcheck(Type, _Pred, Dict, _Prop, _Valid, Locs)) :-
	rtcheck_type(Type),
	list(Dict),
	list(Locs).

:- regtype rtcheck_type/1 # "Specifies the type of run-time errors.".

rtcheck_type(comp).
rtcheck_type(pp_check).
rtcheck_type(success).
rtcheck_type(compat).
rtcheck_type(compatpos).
rtcheck_type(calls).

% ----------------------------------------------------------------------

:- doc(handle_rtcheck/1, "Predicate that processes an rtcheck exception.").

:- pred handle_rtcheck/1 : rtcheck_error.

handle_rtcheck(RTCheck) :-
	rtcheck_to_messages(RTCheck, Messages, []),
	pretty_messages(Messages).

:- meta_predicate call_rtc(goal).

:- pred call_rtc/1 : callable # "This predicate calls a goal and if an
	rtcheck signal is intercepted, an error message is shown and
	the execution continues. Alternatively, it is re-raised as an
	exception depending on the flag rtchecks_abort_on_error
	value.".

call_rtc(Goal) :-
	Error = rtcheck(_Type, _Pred, _Dict, _Prop, _Valid, _Poss),
	( current_prolog_flag(rtchecks_abort_on_error, yes) ->
	    intercept(Goal, Error, throw(Error)) % rethrow signal as exception
	; intercept(Goal, Error, (handle_rtcheck(Error), tracertc))
	).

% ----------------------------------------------------------------------

:- data rtcheck_db/1.

:- meta_predicate save_rtchecks(goal).

:- pred save_rtchecks/1 : callable # "Asserts in rtcheck_db/1 all the
	run-time check exceptions thrown by the goal.".

save_rtchecks(Goal) :-
	retractall_fact(rtcheck_db(_)),
	RTError = rtcheck(_Type, _Pred, _Dict, _Prop, _Valid, _Poss),
	intercept(Goal, RTError, assertz_fact(rtcheck_db(RTError))).

:- pred load_rtchecks/1 => list(rtcheck_error) # "retract the
	rtcheck_db/1 facts and return them in a list.".

load_rtchecks(RTChecks) :-
	findall(RTCheck, retract_fact(rtcheck_db(RTCheck)), RTChecks).
