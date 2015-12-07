:- module(_, _, [profiler]).

%:- cost_center p2/0, p3/0, q/1.

p2(A) :-
	q(A),
	fail.
p2(_).
p2(f) :-
	q(f).

p3 :- q(e).

q(a).
q(b).
q(c) :- r(_, d).


r(_, a).
r(_, b).
r(_, c).

main2(A) :-
	p2(A),
	p3.

:- use_module(library(profiler/profiler_utils)).
t0 :-
	profile_reset,
	profile(main2(_X));
	profile_dump,
	profile_info(_A).
