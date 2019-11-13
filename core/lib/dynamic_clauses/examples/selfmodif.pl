:- module(_,[test/1],[dynamic_clauses]).

% A simple (and very artificial) example of self-modifying code.

:- dynamic loop/1.

test(Xs) :-
    clause(initial_loop(Xs), Body),
    retractall(loop(_)),
    assertz((loop(Xs) :- Body)),
    loop(Xs).

:- dynamic initial_loop/1. % (otherwise we do not get the clause)
initial_loop([100|Xs]) :-
    mutate,
    loop(Xs).

mutate :-
    clause(loop([N|Xs]), Body),
    retractall(loop(_)),
    ( N = 0 ->
        assertz(loop([]))
    ; N1 is N - 1,
      assertz((loop([N1|Xs]) :- Body))
    ).
