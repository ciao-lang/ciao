:- module(_, [nrev/2], [assertions,functional,regtypes,nativeprops]).

:- entry nrev/2 : {list, ground} * var.


:- check pred nrev(A,B)  : list(A) => num(B).  
:- check comp nrev(_,_)  + ( not_fails, is_det, sideff(free) ).
:- check comp nrev(A,_)  + steps_o( length(A) ).

nrev( [] )    := [].
nrev( [H|L] ) := ~conc( nrev(L),[H] ).


:- check comp conc(_,_,_) + ( terminates, non_det ).
:- check comp conc(A,_,_) + steps_o(length(A)).

conc( [],    L ) := L.
conc( [H|L], K ) := [ H | conc(L,K) ]. 
