:- module(bignums_test, [t0/0, t1/0], [assertions]).

:- use_module(library(write), [write/1]).
:- use_module(library(profiler/profiler_auto_conf)).
:- use_module(library(profiler/profiler_utils)).

:- use_module(.(bignums)).

t0 :-
	cc_auto_conf(ticks, do_bignums, 2, Goals, Tree),
	write(Goals),
	nl,
	write(Tree),
	nl.

t1 :-
	profile_reset,
	profile(do_bignums),
	profile_dump.
