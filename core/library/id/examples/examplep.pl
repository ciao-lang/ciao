:- module(examplep,[p/1],[id]).

:- use_module(exampleq).

:- iterative(p/1,0,(_(X,Y):- Y is X + 1)).

p(X) :- q(X).
p(a).
