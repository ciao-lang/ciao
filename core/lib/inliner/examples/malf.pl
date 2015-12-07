:- module(_, _, [inliner, expander]).

:- inline q1/1.

q1(X) :- a(X), !, b.

q2(X) :- a(X), !, b.

p1(X) :-
	q1(X) ; q1(X) ;  X = c.

p2(X) :-
	q2(X) ; q2(X) ; X = c.

p3(X) :-
	q1(X) -> true ; q1(X).

p4(X) :- q1(X).


% p(X) :-
% 	(a(X) -> b) -> true ; q(X) ; X=c.

% p2(X) :- ( a(X) -> true ; q(X) ) -> true .

a(a).
a(b).

b.
