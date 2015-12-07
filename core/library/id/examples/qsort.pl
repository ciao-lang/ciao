:- module(qsort,[qsort/2],[id]).

:- iterative(qsort/2,1,(_(X,Y) :- Y is X + 1)).

qsort(X,Y) :- 
	qsortd(X,Y-[]).


qsortd([],L-L).
qsortd([X|Y],L-R):-
	split(X,Y,I,D),
	qsortd(I,L-[X|S]),
	qsortd(D,S-R).


split(_,[],[],[]).
split(X,[H|B],[H|R],D):- 
	H < X,!,
	split(X,B,R,D).
split(X,[H|B],I,[H|D]) :-
	split(X,B,I,D).
