:- module(_,_,[]).

:- use_package(library(fsyntax/examples/arrays)).
:- use_module(engine(io_basic)).

main(M) :-
    V1 = a(1,3,4,5),
    V2 = a(5,4,3,1),
    I = 1,
    display(V2[I+1]), nl,
    M = V1 <*> ( V2 <+> V1 ).

foo(M) :-
    M = a(a(_,_),a(_,_)),
    M[1,2] = a,
    M[2,1] = a.
