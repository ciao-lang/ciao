:- module(mqu_test, [t0/0, t1/0], [assertions]).

:- use_module(library(write), [write/1]).
:- use_module(library(profiler/profiler_utils)).
:- use_module(library(profiler/profiler_auto_conf)).

:- use_module(.(mqu_ov_test)).

t0 :-
	cc_auto_conf(ticks, queens(s(s(s(s(0))))), 2, Goals, Tree),
	write(Goals),
	nl,
	write(Tree),
	nl.

t1 :-
	profile_reset,
	profile(queens(s(s(s(s(0)))))),
	profile_dump.
