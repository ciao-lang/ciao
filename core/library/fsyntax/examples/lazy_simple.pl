:- module(_,_,[functional,lazy]).
:- use_module(library(lazy/lazy_lib), [take/3]).

nums(N) := ~take(N,nums_from(0)).

:- lazy fun_eval nums_from/1.

nums_from(X) := [X | nums_from(X+1)].
