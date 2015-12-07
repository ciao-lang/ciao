:- module(_, _, [hiord]).

:- use_package(inliner).
:- use_package(expander).

:- inline_module(inliner_example_inc).

test2(A, B) :-
	mymap(A, atom_codes, B).

:- meta_predicate ppp1(pred(1)).
:- meta_predicate ppp2(pred(1)).
:- meta_predicate ppp3(pred(1)).

ppp1(A) :-
	ppp2(A).

ppp2(A) :-
	ppp3(A).

ppp3(A) :-
	A(2).

testppp :-
	ppp1(int).

:- data prueba_db/2.

test4(A, B) :-
	prueba_db(A, B).

:- meta_predicate mylist(?, pred(1)).

mylist([],    _).
mylist([E|L], T) :-
	T(E),
	mylist(L, T).

test(A) :-
	mylist(A, int),
	mylist(A, nnegint).


:- meta_predicate call2(goal).

call2(Goal) :-
	Goal.

display_2(A, B) :-
	display(A),
	nl,
	display(B),
	nl.

test3(A) :-
	call2(display_2('hola', A)).

test5 :-
	call2(display_2('hola', 'mundo')).

test6(A) :-
	call2(display_2(A, 'mundo')).

pepito(A) :-
	juanito(A).

juanito(_).


:- inline p/1.
p(f(a)).

test_ :-
	var(A),
	p(A),
	display(A).
