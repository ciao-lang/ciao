:- module(example4,_,[fuzzy]).



young_couple(X,Y,Mu) :~ prod
	age(X,X1),
	age(Y,Y1),
	young(X1,MuX),
	young(Y1,MuY).

age(john,35).
age(rose,46).


young :# fuzzy_predicate 
[(0,0),(10,0),(15,0.2),(20,1),(25,1),
 (35,0.8),(40,0.6),(45,0.4),(50,0),(120,0)].
