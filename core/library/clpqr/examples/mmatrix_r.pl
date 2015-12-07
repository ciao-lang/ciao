:- use_package(clpr).
:- use_module(library(write)).

mmultiply([],_,[]).
mmultiply([V0|Rest], V1, [Result|Others]):-  
            mmultiply(Rest, V1, Others),
    	    multiply(V1,V0,Result).

multiply([],_,[]).
multiply([V0|Rest], V1, [Result|Others]):-  
            multiply(Rest, V1, Others),
    	    vmul(V0,V1,Result).

vmul([],[],0).
vmul([H1|T1], [H2|T2], Result):- 
	vmul(T1,T2, Newresult), 
	Result .=. H1*H2+Newresult.

matrix(1,[[1,2,3,4,5],[4,0,-1,5,6],[7,1,-2,8,9],[-1,0,1,3,2],[1,5,-3,2,4]]).
matrix(2,[[3,2,1,0,-1],[-2,1,3,0,2],[1,2,0,-1,5],[1,3,2,4,5],[-5,1,4,2,2]]).

%% Call with: ?- go(M).

go(M):-
        matrix(1,M1),
        matrix(2,M2), 
        mmultiply(M1, M, M2).
