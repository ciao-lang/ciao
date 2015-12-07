:- package(dynamic_clauses).
:- new_declaration(dynamic_handling/0).
:- use_module(library(dynamic_rt)).

:- multifile '\3\mfclause'/2.
:- data '\3\mfclause'/2.

:- redefining(clause/2).

:- data '\3\clause'/2.

:- export(clause/2).
clause(H, B) :- '\3\clause'(H,B).

