:- module(mqu, [queens_n/2, queens/1], []).

:- use_module(library(write),      [write/1]).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(between)).

queens_n(N, A) :-
	between(1, N, _),
	queens(A),
	fail
    ;
	true.

queens(A) :-
	queens_(A, A, A, B),
	displ(B).

zero([]).
zero([0|A]) :-
	zero(A).

adddig([A|B], C, [[A]|D]) :-
	add(B, C, D).

place(0, A) :-
	zero(A).
place(s(A), [q|B]) :-
	place(A, B).
place(s(A), [0|B]) :-
	place(s(A), B).

app([],    A, A).
app([A|B], C, [A|D]) :-
	app(B, C, D).

seed(0,    []).
seed(s(A), [[]|B]) :-
	seed(A, B).

displ([]).
displ([_A|B]) :-
	displ(B).

board(0, s(A), [], [], B, C, C) :-
	seed(s(A), B),
	seed(A,    C).
board(s(A), B, C, [D|E], F, G, H) :-
	board(A, B, I, E, J, K, L),
	new(B, D),
	app(D, I, C),
	add(D, J, F),
	adddig(D, K, G),
	rev(D, [], M),
	adddig(M, L, H).

atmost1([]).
atmost1([q|A]) :-
	zero(A).
atmost1([0|A]) :-
	atmost1(A).

add([],    A,     A).
add([A|B], [C|D], [[A|C]|E]) :-
	add(B, D, E).

check([]).
check([A|B]) :-
	atmost1(A),
	check(B).

rev([],    A, A).
rev([A|B], C, D) :-
	rev(B, [A|C], D).

new(0,    []).
new(s(A), [_B|C]) :-
	new(A, C).


queens_(A, B, C, D) :-
	board(B, C, E, F, G, H, I),
	place(A, E),
	check(F),
	check(G),
	check(H),
	check(I),
	D=F.

%%%%%%%%%%%%%%%%%%%%%
:- export(ourmain/0).
ourmain:-
	statistics(runtime, _),
	ourdo,
	statistics(runtime, [_, T1]),
	write(T1).

:- export(ourdo/0).
ourdo:-
	queens(s(s(s(s(s(s(s(s(0))))))))),
	fail.
ourdo.
