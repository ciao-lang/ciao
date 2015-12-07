:- module(_,_,[id]).

% All goals deeper than 2 will fail
:- iterative(p/1,0,(_(X,Z) :- Z is X + 1),2).

% Change the solutions' order to goal p(X). 
%:- iterative(p/1,1,(_(X,Z) :- Z is X + 3)).

p(X) :- q(X).
p(a).

q(X) :- r(X).
q(b).

r(X) :- s(X).
r(c).

s(d).
