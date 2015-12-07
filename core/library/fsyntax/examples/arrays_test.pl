:- module(_,_).

:- use_package(library(fsyntax/examples/arrays)).

main(M) :-
	V1 = a(1,3,4,5),
	V2 = a(5,4,3,1),
	I = 1,
	display(V2@[I+1]),
	M = V1 <*> ( V2 <+> V1 ).
