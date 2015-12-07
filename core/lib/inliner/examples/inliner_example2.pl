:- module(_, [testdisplay2/1, patatin/0, testdisplay/0, mydisplay_list/1,
		testppp1/2, mylist/2, testdisplay3/2], [hiord, inliner]).

:- use_package(expander).

:- unfold_meta.

:- inline testpas2/2.

testpas2(A, B) :-
	display(A),
	display(B).

:- export(testpas1/2).
testpas1(A, B) :-
	testpas2(A, B).

:- inline ppp1/2.

ppp1(A, A).

patatin :- patatin2.

:- inline ppp2/2.

ppp2([C], C).

patatin2.

testppp1(A, C) :-
	ppp1([A], C),
	ppp2([C], C).

:- inline mylist/2.
:- unfold mylist(yes, yes).
:- meta_predicate mylist(?, pred(1)).

mylist([],    _).
mylist([E|L], T) :-
	T(E),
	mylist(L, T).

:- meta_predicate testdisplay3(?, pred(1)).

testdisplay3(A, T) :-
	mylist([b|A], T).

:- inline mydisplay_list/1.
:- unfold mydisplay_list(yes).

mydisplay_list([]).
mydisplay_list([L|Ls]) :-
	display(L),
	mydisplay_list(Ls).

testdisplay :-
	mydisplay_list([a, b, c]).

testdisplay2(A) :-
	mylist([a, b|A], display).
