:- module(chain_bfall, _, ['bf/bfall']).
%:- module(chain_bfall, _, ['bf/afall']).
%:- module(chain_bfall, _, [bf]).

test :- chain(a,d).

chain(X,X).
chain(X,Y) :- arc(X,Z), chain(Z,Y).

arc(a,b).
arc(a,d).
arc(b,c).
arc(c,a).
