:- module(example5,_,[fuzzy]).



agile(X,Y,Mu):~ young(X,Muj),sportsman(Y,Mux).

awkward(X,Y,Mu):~ old(X,Mux), not_sportsman(Y,Mud).


young :# fuzzy_predicate([(0,0),(10,0),(15,0.2),(20,1),(25,1),(35,0.8),(40,0.6),(45,0.4),(50,0),(120,0)]).

old :# fnot young/2.

not_sportsman :# fnot  sportsman/2.

sportsman :# fuzzy_predicate([(7,1),(5,0.8),(1,0.2),(0,0)]).

perfomance(X,Y,M) :~ dprod  
	agile(X,Y,_),
	awkward(X,Y,_).


young_imp_sportman(X,Y,M):~ '=>'(min,young(X,_),sportsman(Y,_),M).
