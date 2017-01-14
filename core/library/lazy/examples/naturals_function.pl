:- module(naturals_function, _, [fsyntax, lazy]).

:- use_module(library(lists), [nth/3]).

:- fun_eval arith(true).

:- lazy fun_eval generate_naturals_list(_,~).
generate_naturals_list(X) := [X | generate_naturals_list(X + 1)].

test(Position) := ~nth(Position, generate_naturals_list(1)).
