:- module(_, _, [hiord]).

:- use_package(inliner).
:- use_package(expander).

:- use_module(library(hrtime)).
:- use_module(library(lists)).

:- use_module(library(compiler), [use_module/1]).

:- inline measure/2.

:- meta_predicate measure(goal, ?).
measure(Goal, Time) :-
	hrtime(Time0),
	Goal,
	hrtime(Time1),
	Time is Time1 - Time0.

test1(Time) :-
	measure(use_module(ciaopp(ciaopp)), Time).

test2(Time) :-
	measure(use_module(ciaopp(plai/domains/fr_sets)), Time).
