:- module(sudoku_test, [t0/0, t1/0], [assertions]).

:- use_module(library(write), [write/1]).
:- use_module(library(profiler/profiler_auto_conf)).
:- use_module(library(profiler/profiler_utils)).

:- use_module(.(sudoku)).


%You can also test tes2, test3 and test4
t0 :-
	cc_auto_conf(ticks, test1, 2, Goals, Tree),
	write(Goals),
	nl,
	write(Tree),
	nl.

t1 :-
	profile_reset,
	profile(test1),
	profile_dump.