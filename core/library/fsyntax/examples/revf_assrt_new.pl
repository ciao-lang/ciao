:- module(_, [nrev/2], [assertions,fsyntax,nativeprops]).

:- entry nrev/2 : {list, ground} * var.

:- pred nrev(A,B) : list(A) => list(B) 
   + ( not_fails, is_det, steps_o( exp(length(A),2) ) ).

nrev( [] )    := [].
nrev( [H|L] ) := ~conc( ~nrev(L),[H] ).


:- pred conc(A,_,_) + ( terminates, is_det, steps_o(length(A)) ).

conc( [],    L ) := L.
conc( [H|L], K ) := [ H | ~conc(L,K) ]. 
