:- module(exampleq,[q/1],[id]).

q(X):- r(X).
q(b).

r(X):- s(X).
r(c).

s(d).
