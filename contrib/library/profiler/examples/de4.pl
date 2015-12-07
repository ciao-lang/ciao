:- module(_, _, [profiler]).

:- cost_center [p3/0, q/1, main2/0].

p2 :-
	q(_A),
	fail.
p2.
p2 :-
	q(f).

p3 :- q(e).

q(a).
q(b).
q(c) :- r(_, d).


r(_, a).
r(_, b).
r(_, c).

main2 :-
	cost_center(first_p2, p2),
	p2,
	p3.

:- use_module(library(profiler/profiler_utils)).
t0 :-
	profile_reset,
	profile(main2);
	profile_dump,
	profile_info(_A).
