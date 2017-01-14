:- module(fib_function, _, [fsyntax, lazy]).

:- use_module(library(lists), [nth/3]).
:- use_module(library(lazy/lazy_lib), [tail/2, zipWith/4]).
:- use_module(library(arithpreds)).


:- lazy fun_eval fib1/0.
fib1 := [0, 1 | ~zipWith(+, L, ~tail(L))]
    :- fib1(L).

:- lazy fun_eval fib2(~).
fib2 := [0, 1 | ~zipWith(+, L, ~tail(L))]
    :- fib2(L).

:- lazy fun_return fib3(~).
fib3 := [0, 1 | ~zipWith(+, L, ~tail(L))]
    :- fib3(L).

:- fun_eval main1/1.
main1(Element) := ~nth(Element, fib1).

:- fun_eval main2/1.
main2(Element) := ~nth(Element, fib2).

:- fun_eval main3/1.
main3(Element) := ~nth(Element, fib3).


