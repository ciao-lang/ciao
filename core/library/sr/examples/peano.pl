:- module(_,_,[sr/bfall]).

% In pure LP, define the naturals, arithmetic 
% operations on them, and the squares of the 
% naturals that are smaller than five.

nat(0).
nat(s(X)) :- nat(X).

le(0,X) :- nat(X).
le(s(X),s(Y)) :- le(X,Y).

add(0,Y,Y) :- nat(Y).
add(s(X),Y,s(Z)) :- add(X,Y,Z).

mult(0,Y,0) :- nat(Y).
mult(s(X),Y,Z) :- add(W,Y,Z), mult(X,Y,W).

nat_square(X,Y) :- nat(X), nat(Y), mult(X,X,Y).

sol(X) :- nat(Y), le(Y,s(s(s(s(s(0)))))), nat_square(Y,X).

%  Some examples of queries. Copy on the right, hit 
%  ENTER to execute, and then ; for other solutions.
%  Some may not terminate (this is OK). 
%
%  ?- nat(s(0)).
%  ?- nat(X).
%  ?- add(s(0),s(s(0)),X).
%  ?- add(s(0),X,s(s(s(0)))).
%  ?- add(X,Y,s(0)).
%  ?- nat_square(s(s(0)), X).
%  ?- nat_square(X,s(s(s(s(0))))).
%  ?- nat_square(X,Y).
%  ?- solution(X).

%% :- op(500,fy,s).
