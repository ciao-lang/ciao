:- module(_, _, [profiler]).

:- cost_center p1/0, p2/0, p3/0, q/1, r1/0, r2/0.

p1 :-
	q(a),
	q(b),
	q(c),
	q(d),
	r1,
	r2.

r1.
r2.

p2 :-
	q(a),
	q(b).

p3 :-
	q(c),
	q(d).

main1 :-
	q(a),
	p1,
	p2.
% 	p3.

rrr(A) :-
	q(A).

qqq :-
	rrr(_A),
	fail.
qqq.

q(a) :- display(a), nl.
q(b) :- display(b), nl.
q(c) :- display(c), nl.
q(d) :- display(d), nl.

list1([],     _).
list1([X|Xs], Y) :-
	p(X, Y),
	list1(Xs, X).

p(X, Y) :- X = Y.

main2 :-
	p2.
