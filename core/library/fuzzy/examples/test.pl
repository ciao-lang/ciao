:- module(test,[main/0],[fuzzy]).

:- use_module(example7, [good_player1/2, good_player2/2]).

main:- 
	display('testing fuzzy library .....'),nl,
	bottle_filled(90,X),
	(
	    X .=. 0.5,true ->
	    display('bottle_filled -> ok'),nl
	;
	    display('bottle_filled -> error'),nl
	),
	bottle_not_empty(10,X1),
	(
	    X1 .=. 0.5,true ->
	    display('bottle_not_empty -> ok'),nl
	;
	    display('bottle_not_empty -> error'),nl
	),
	good_player1(john,X3),
	(
	    X3 .=. 0.75,true ->
	    display('testing user aggr -> ok'),nl
	;
	    display('testing user aggr -> error'),nl
	).
	
	
	

bottle_filled  :# fuzzy_predicate([(100,1),(95,1),(85,0),(0,0)]).

bottle_empty :# fuzzy_predicate([(0,1),(5,1),(15,0),(100,0)]).

bottle_not_filled :# fnot bottle_filled/2.

bottle_not_empty :# fnot bottle_empty/2.

bottle_half_filled(X,M) :~ luka bottle_not_filled(X,_),bottle_not_empty(X,_).


many_glasses :# fuzzy_predicate([(10,1),(8,1),(6,0),(0,0)]).

few_glasses :# fuzzy_predicate([(0,1),(2,1),(4,0),(10,0)]).

not_many_glasses :# fnot many_glasses/2.

not_few_glasses :# fnot few_glasses/2.

some_glasses(X,M) :~ not_many_glasses(X,_),not_few_glasses(X,_).




indefined_glasses(X,M)  :~ max  
	not_many_glasses(X,_M1),
	not_few_glasses(X,_M2).



rule1(X,Y,M) :~ '=>'(min,bottle_filled(X,A),many_glasses(Y,B),M).
rule2(X,Y,M) :~ '=>'(min,bottle_not_empty(X,A),few_glasses(Y,B),M).



result(X,Y,M):~ dprod 
	rule1(X,Y,_),
	rule2(X,Y,_).

amount_of_glasses(Y,M) := bottle_filled(X,M1),'=>'(min,bottle_filled(X,Mb1),many_glasses(Y,Mv1),M2).
