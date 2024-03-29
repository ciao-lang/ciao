:- module(primes_function, [test_primes/2], [fsyntax, lazy]).

:- use_module(library(lazy/lazy_lib), [take/3, nums_from/2]).

:- lazy fun_eval cut/1.
cut([])      := [].
cut([H | T]) := [H | ~cut(~cut_(T, H))].

:- lazy fun_eval cut_/2.
cut_([], _)        := [].
%cut_([H2 | T], H1) := ( H2 mod H1 > 0 ? [H2 | T2] | T2 ) :- cut_(T, H1, T2).
cut_([H2 | T], H1) := R :-
    R = ( H2 mod H1 > 0 ? [H2 | ~cut_(T, H1)] | ~cut_(T, H1) ).

:- lazy fun_eval primes/0.
primes := ~cut(~nums_from(2)).

test_primes(N) := ~take(N, ~primes).
