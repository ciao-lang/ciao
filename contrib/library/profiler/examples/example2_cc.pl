:- module(_, _, [profiler]).

list1(P, [_|Xs]) :-
	list1(P, Xs).
list1(_, []).

main21 :-
	list1(a, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
	fail.
main21.


list2([_|Xs], P) :-
	list2(Xs, P).
list2([], _).

main22 :-
	list2([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], a),
	fail.
main22.

main :-
	main21.

ppp.

p :-
	q.

q.
q.

r :-
	p,
	fail.
r.

:- no_cost_center pp0/0, qq/2.

pp :- pp0.

pp0 :- qq(_, a).

qq(_, b).
qq(_, c).
qq(_, d).
qq(_, e).
qq(_, f).
qq(_, a).

:- no_cost_center aa0/0, aa/1, bb/1, cc0/1, cc1/1, cc2/1, cc3/1.

aaa :- aa0.

aa0 :- aa(d).

aa(A) :- bb(A).
aa(A) :- bb(A).
aa(A) :- bb(A).
aa(A) :- bb(A).

bb(A) :- cc0(A).
bb(A) :- cc1(A).
bb(A) :- cc2(A).
bb(A) :- cc3(A).

cc0(a).

cc1(b).

cc2(c).

cc3(d).

