:- module(example_id, _,[id]).

test(id) :- 
	idchain(a,d).
test(df) :- 
	chain(a,d).   % loops!

:- iterative(idchain/2, 3, ( _(X,Z) :- Z is X + 1) ).

idchain(X,X).
idchain(X,Y) :- 
	arc(X,Z), 
	idchain(Z,Y).

chain(X,X).
chain(X,Y) :- 
	arc(X,Z), 
	chain(Z,Y).

arc(a,b).
arc(a,d).
arc(b,c).
arc(c,a).
