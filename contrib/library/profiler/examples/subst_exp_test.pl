:- module(subst_exp_test, [t0/0, t1/0], [assertions]).

:- use_module(library(write), [write/1]).
:- use_module(library(profiler/profiler_auto_conf)).
:- use_module(library(profiler/profiler_utils)).

:- use_module(.(subst_exp)).

t0 :-
	cc_auto_conf(ticks, substitute((a + (b * c) + (c * d)) - e, [a = 2, b = 3, c = 4, d = 5, e = 6], _), 2, Goals, Tree),
	write(Goals),
	nl,
	write(Tree),
	nl.

t1 :-
	profile_reset,
	profile(substitute((a + (b * c) + (c * d)) - e, [a = 2, b = 3, c = 4, d = 5, e = 6], _)),
	profile_dump.
