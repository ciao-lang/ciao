:- module(zebra_argnames_test, [t0/0, t1/0], [assertions]).

:- use_module(library(write), [write/1]).
:- use_module(library(profiler/profiler_auto_conf)).
:- use_module(library(profiler/profiler_utils)).

:- use_module(.(zebra_argnames)).

t0 :-
	cc_auto_conf(ticks, zebra(_, _, _), 2, Goals, Tree),
	write(Goals),
	nl,
	write(Tree),
	nl.

t1 :-
	profile_reset,
	profile(zebra(_,_,_)),
	profile_dump.
