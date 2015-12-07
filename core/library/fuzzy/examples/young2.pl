:- module(young2,_,[fuzzy]).

young_couple(X,Y,Mu) :~ min
	age(X,X1),
	age(Y,Y1),
	young(X1,MuX),
	young(Y1,MuY).

age(john,37).
age(rose,39).

young :# fuzzy_predicate([(0,1),(35,1),(45,0),(120,0)]).
