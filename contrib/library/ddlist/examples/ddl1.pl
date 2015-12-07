:- module( ddl1 , _ , [] ).

:- use_module(library(ddlist)).

main(A,B):-
	% L = []
	null_ddlist( L ),
	% L = [1]
	insert_after( L  , 1 , L1 ),
	% L = [1,2]
	insert_after( L1 , 2 , L2 ),
	% L = [1,3,2]
	insert_after( L2 , 3 , L3 ),
	% L = [1,3,2] => A = [1]
	top( L3 , A ),
	% L = [3,2]
	next( L3 , PL3 ),
	% L = [3,2] => A = [3]
	top( PL3 , B ).
