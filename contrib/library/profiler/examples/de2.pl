:- module(_, _, [profiler]).

:- cost_center p1/0, p2/0, p3/0, q/1.

p2 :-
	q(_A),
	fail.
p2.
p2 :-
	q(f).

p3 :- q(e).

q(a).
q(b).

main2 :-
	p2,
	p3.

:- use_module(library(profiler/profiler_utils)).
t0 :-
	profile_reset,
	profile(main2);
	profile_dump.
