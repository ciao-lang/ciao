:- module(example7,_,[fuzzy]).

:- use_module(library(lists)).

:- aggr average ## sum #> average_final. % arithmetic average 

sum(X,Y,Z) :- 
	Z .=. (X + Y).

average_final(ListTemp,ValorTemp,ValorFinal) :- 
	length(ListTemp,L),
	ValorFinal .=. ValorTemp /L.



:- aggr wa <# wa_initial ## sum. % weigthed average 

wa_initial(ListI,ListTemp):- weights(W), merge(ListI,W,prod,ListTemp).

weights([0.3,0.8]).


% program 


tall(john,0.8):~ .
fast(john,0.7):~ . 

good_player1(X,_):~ average 
	tall(X,_),
        fast(X,_).


good_player2(X,_):~ wa 
	tall(X,_),
        fast(X,_).
