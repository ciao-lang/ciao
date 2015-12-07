:- module(_, [t0/0, t1/0], [profiler]).

:- use_module(library(profiler/profiler_utils)).

:- cost_center p/1, q/1, r/1, s/1, t/1.

p(A) :-
	q(A).
q(A) :-
	r(A).

r(a).
r(b).

s(A) :-
	t(A).

t(c).
t(d).

t0 :-
	profile(p(_A)),
	fail
 ;
	s(_B),
	profile_dump.

t1 :-
	profile(t0),
	profile_dump.
