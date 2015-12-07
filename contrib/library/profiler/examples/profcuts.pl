:- module(profcuts, _, [assertions, profiler]).

:- use_module(library(profiler/profiler_utils)).

:- cost_center p2/0.

p1 :-
	member(a, [a, b, a, b, a, b, a]).

p2 :-
	p3.

p3 :-
	p1,
	p1,
	p1,
	p1,
	!,
	p1,
	p1,
	!,
	!,
	!,
	!.
p3.

t0 :-
	profile_reset,
	profile(p2),
	profile_dump.
