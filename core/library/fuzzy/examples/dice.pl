:- module(dice,_,[fuzzy]).


small :# fuzzy_predicate([(1,1),(2,1),(3,0.7),(4,0.3),(5,0),(6,0)]).
large :# fuzzy_predicate([(1,0),(2,0),(3,0.3),(4,0.7),(5,1),(6,1)]).

die1(X,M) :~
	small(X,M).

die2(X,M) :~
	large(X,M).


two_dice(X,Y,M):~ luka
	die1(X,M1),
	die2(Y,M2).

sum(2,M) :~  
	two_dice(1,1,M1).


sum(3,M) :~ dluka 
	two_dice(1,2,M1),
	two_dice(2,1,M2).

sum(4,M) :~ dluka
	two_dice(1,3,M1),
	two_dice(3,1,M2),
	two_dice(2,2,M3).



sum(5,M) :~ dluka
	two_dice(4,1,M1),
	two_dice(1,4,M2),
	two_dice(3,2,M3),
	two_dice(2,3,M4).

sum(6,M) :~ dluka
	two_dice(1,5,M1),
	two_dice(5,1,M2),
	two_dice(2,4,M3),
	two_dice(4,2,M4),
	two_dice(3,3,M5).

sum(7,M) :~ dluka
	two_dice(1,6,M1),
	two_dice(6,1,M2),
	two_dice(2,5,M3),
	two_dice(5,2,M4),
	two_dice(3,4,M5),
	two_dice(4,3,M6).

sum(8,M) :~ dluka
	two_dice(2,6,M1),
	two_dice(6,2,M2),
	two_dice(3,5,M3),
	two_dice(5,3,M4),
	two_dice(4,4,M5).

sum(9,M) :~ dluka
	two_dice(3,6,M1),
	two_dice(6,3,M2),
	two_dice(4,5,M3),
	two_dice(5,4,M4).


sum(10,M) :~ dluka
	two_dice(4,6,M1),
	two_dice(6,4,M2),
	two_dice(5,5,M3).

sum(11,M) :~ dluka
	two_dice(5,6,M1),
	two_dice(6,5,M2).

sum(12,M) :~ dluka
	two_dice(6,6,M1).
