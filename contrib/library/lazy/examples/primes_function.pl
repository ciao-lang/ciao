:- module(primes_function, [test_primes/2], [fsyntax, lazy]).

:- use_module(library(lazy/lazy_lib), [take/3, nums_from/2]).

:- lazy fun_eval cut/1.
cut([])      := [].
cut([H | T]) := [H | T2] :- cut_(H, T, S), !, cut(S, T2).

:- lazy fun_eval cut_/2.
cut_(_, [])        := [].
cut_(H1, [H2 | T]) := [H2 | T2] :- cut_(H1, T, T2), H2 mod H1  >  0.
cut_(H1, [H2 | T]) := L         :- cut_(H1, T, L),  H2 mod H1 =:= 0.

% :- lazy fun_eval primes/0.
% primes := ~cut(~nums_from(2)).

test_primes(N) := ~take(N, ~cut(~nums_from(2))).
