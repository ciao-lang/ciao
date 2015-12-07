:- module(wumpus_test, [t0/0, t1/0], [assertions]).

:- use_module(library(write), [write/1]).
:- use_module(library(profiler/profiler_auto_conf)).
:- use_module(library(profiler/profiler_utils)).

:- use_module(.(wumpus)).

t0 :-
	cc_auto_conf(ticks, w, 3, Goals, Tree),
	write(Goals),
	nl,
	write(Tree),
	nl,
        tree_to_tex(ticks,Tree).

t1 :-
	profile_reset,
	profile(w),
	profile_dump.
