:- module(tk_test_aux, [
        hello/0,
        factorial/2,
        show/1,
        quit/0],
        [objects]).


hello :-
        display('Hello !!!!'),
        nl.

show(X) :-
        display(X),
        nl.


factorial(N2,X) :-
        factorial_aux(N2,X).
 %%         display(X),
 %%         nl.

factorial_aux(0,1).

factorial_aux(N,X1) :-
        N > 0,
        N1 is N - 1,
        factorial_aux(N1,X2),
        X1 is X2 * N.

quit.
