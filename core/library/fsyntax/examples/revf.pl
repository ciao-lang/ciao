:- module(_, [nrev/2], [functional]).

nrev( [] )    := [].
nrev( [H|T] ) := ~conc( nrev(T),[H] ).

conc( [],    L ) := L.
conc( [H|T], K ) := [ H | conc(T,K) ]. 
