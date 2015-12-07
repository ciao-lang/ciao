:- module(color_map_test, [t0/0, t1/0], [assertions]).

:- use_module(library(write), [write/1]).
:- use_module(library(profiler/profiler_auto_conf)).
:- use_module(library(profiler/profiler_utils)).

:- use_module(.(color_map)).

t0 :-
	cc_auto_conf(ticks, color_map(_, _, _, _, _), 2, Goals, Tree),
	write(Goals),
	nl,
	write(Tree),
	nl,
        tree_to_tex(time(_), Tree).

t1 :-
	profile_reset,
	profile(color_map(_, _, _, _, _)),
	profile_dump.
