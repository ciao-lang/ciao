:- module(module1, [test/1], [fsyntax, lazy, hiord]).

:- use_module(library(lazy/lazy_lib), [nums_from/2, takeWhile/3]).
:- use_module(module2, [squares/2]).
:- use_module(library(arithpreds)).

:- fun_eval test/0.
test := ~takeWhile((''(X) :- X < 10000), ~squares(~nums_from(1))).

