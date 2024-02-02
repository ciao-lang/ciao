:- module(primes, [test_primes/2], [lazy]).

:- use_module(library(lazy/lazy_lib), [nums_from/2, take/3]).

:- lazy cut/2.
cut([], []).
cut([H | T], R) :-
    cut_(T, H, S),
    !,
    cut(S, T2),
    R = [H | T2].

:- lazy cut_/3.
cut_([], _, []) :- !.
cut_([H2 | T], H1, R) :-
    cut_(T, H1, T2),
    ( H2 mod H1 > 0 -> R = [H2 | T2]
    ; R = T2
    ).

:- lazy primes/1.
primes(X) :-
    nums_from(2, Z),
    cut(Z, X).

test_primes(N, R) :-
    primes(X),
    take(N, X, R).
