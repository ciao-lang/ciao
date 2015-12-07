:- module( ddl2 , _ , [] ).

:- use_module(library(ddlist)).

main(A,B):-
	% L = []
	null_ddlist( L ),
	% L = [1]
	insert_after( L  , 1 , L1 ),
	% L = [1,2]
	insert_after( L1 , 2 , L2 ),
	% L = [1,2]
	insert( L2 , 3 , L3 ),
	% L = [3,1,2]
	prev( L3 , PL3 ),
	% L = [],
	forward( PL3 , FOR ),
	% L = [2]
	prev( FOR , FOR1 ),
	% L = [2] => A = 2
	top( FOR1 , A ),
	% L = [1,2]
	prev( FOR1 , FOR2 ),
	% L = [2]
	delete_after( FOR2 , FOR3 ),
	% L = [3,2]
	prev( FOR3, FOR4 ),
	% L = [3,2] => B = 3
	top( FOR4 , B ).
