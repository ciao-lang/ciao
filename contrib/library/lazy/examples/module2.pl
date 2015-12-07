:- module(module2, [squares/2], [fsyntax, lazy]).

:- use_module(library(lazy/lazy_lib), [lazy_map/3]).

:- fun_eval arith(true).

:- lazy fun_eval squares(_,~).
squares(List) := ~lazy_map(List, (''(X, Y) :- Y is X * X)).

