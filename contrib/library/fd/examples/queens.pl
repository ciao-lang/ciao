:- use_package(fd).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format)).
:- use_module(library(aggregates)).
:- use_module(library(lists), [length/2]).


all_queens(N, Qs) :-
	queens(N, Qs),
	display(Qs), nl,
	fail.
all_queens(_,_).

allq(N,L) :- findall(_X,do_queens(N,_X),_S), length(_S,L).

queens(N, Qs) :-
	statistics(runtime,_),
	do_queens(N, Qs),
	statistics(runtime,[_, Time]),
	format("Used ~d milliseconds~n", Time).

do_queens(N, Qs):- 
	constrain_values(N, N, Qs),
	all_different(Qs),!,
	labeling(Qs).

constrain_values(0, _N, []).
constrain_values(N, Range, [X|Xs]):-
        N > 0, 
        X in 1 .. Range,
        N1 is N - 1,
        constrain_values(N1, Range, Xs),
        no_attack(Xs, X, 1).

no_attack([], _Queen, _Nb).
no_attack([Y|Ys], Queen, Nb):-
	Nb1 is Nb + 1,
	no_attack(Ys, Queen, Nb1),
	Queen .<>. Y + Nb,
	Queen .<>. Y - Nb.	
