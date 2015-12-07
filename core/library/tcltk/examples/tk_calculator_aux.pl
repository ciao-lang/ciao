
:- module(tk_calculator_aux,[]).

:- export(plus/3).
:- export(sub/3).
:- export(prod/3).
:- export(div/3).


plus(X,Y,Z) :- number(X),number(Y), Z is X+Y.

plus(X,Y,Z) :- number(X),number(Z), Y is Z-X.

plus(X,Y,Z) :- number(Y),number(Z), X is Z-Y.

sub(X,Y,Z) :- number(X),number(Y), Z is X-Y.

sub(X,Y,Z) :- number(X),number(Z), Y is X-Z.

sub(X,Y,Z) :- number(Y),number(Z), X is Z+Y.

prod(X,Y,Z) :- number(X),number(Y), Z is X*Y.

prod(X,Y,Z) :- number(X),number(Z), Y is Z/X.

prod(X,Y,Z) :- number(Y),number(Z), X is Z/Y.

div(X,Y,Z) :- number(X),number(Y), Z is X/Y.

div(X,Y,Z) :- number(X),number(Z), Y is X/Z.

div(X,Y,Z) :- number(Y),number(Z), X is Z*Y.
