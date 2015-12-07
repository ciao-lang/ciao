:- module(fril,_,[fuzzy]).

fluent_speaker(john,english,1)  :~ .
fluent_speaker(john,spanish,M)  :~ {M .>=. 0.8, M .=<. 1} . 

likes(john,maria,0.8) :~ .
likes(maria,john,M) :~ {M .>=. 0.6, M .=<. 0.9} .

friends(X,Y,M) :~ 
 	likes(X,Y,M1),
 	likes(Y,X,M2).

age_john :# fuzzy_predicate([(30,0),(34,1),(37,1),(39,0)]).
about_thirty :# fuzzy_predicate([(28,0),(29,1),(31,1),(32,0)]).

age(john,X,M) :~ age_john(X,M).  
age(maria,X,M) :~ about_thirty(X,M).
