:- module(qsort_test, [t0/0, t1/0], [assertions]).

:- use_module(library(write), [write/1]).
:- use_module(library(profiler/profiler_auto_conf)).
:- use_module(library(profiler/profiler_utils)).

:- use_module(.(qsort)).

t0 :-
	cc_auto_conf(ticks, qsort([5,6,2,3,4,1,7], _), 2, Goals, Tree),
	write(Goals),
	nl,
	write(Tree),
	nl.

t1 :-
	profile_reset,
	profile(qsort([5,6,2,3,4,1,7], _)),
	profile_dump.
