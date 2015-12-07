:- module(hanoi_test, [t0/0, t1/0], [assertions]).

:- use_module(library(write), [write/1]).
:- use_module(library(profiler/profiler_auto_conf)).
:- use_module(library(profiler/profiler_utils)).

:- use_module(.(hanoi)).

t0 :-
	cc_auto_conf(ticks, hanoi(5, a, b, c, _), 2, Goals, Tree),
	write(Goals),
	nl,
	write(Tree),        
	nl, 
        tree_to_tex(ticks, Tree).

t1 :-
	profile_reset,
	profile(hanoi(5, a, b, c, _)),
	profile_dump.
