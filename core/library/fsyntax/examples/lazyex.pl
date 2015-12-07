:- module(_,_,[fsyntax,lazy]).

:- use_module(library(lazy/lazy_lib), [tail/2, zipWith/4]).
:- use_module(library(arithpreds)).

:- lazy fun_eval fiblist/0.

fiblist := [0, 1 | ~zipWith(+, FibL, ~tail(FibL))]
        :- FibL = fiblist.
