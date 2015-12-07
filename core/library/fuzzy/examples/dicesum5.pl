:- module(dicesum5,_,[fuzzy]).

% this example tries to measure which is the possibility
% that a couple of values, obtained throwing two loaded dice, sum 5. Let
% us suppose we only know that one die is loaded to obtain a small value
% and the other is loaded to obtain a large value. 
%
% the query is  ? sum(5,M)
%

small :# fuzzy_predicate([(1,1),(2,1),(3,0.7),(4,0.3),(5,0),(6,0)]).
large :# fuzzy_predicate([(1,0),(2,0),(3,0.3),(4,0.7),(5,1),(6,1)]).

die1(X,M) :~
	small(X,M).

die2(X,M) :~
	large(X,M).


two_dice(X,Y,M):~ prod
	die1(X,M1),
	die2(Y,M2).

sum(2,M) :~  
	two_dice(1,1,M1).


sum(5,M) :~ dprod
	two_dice(4,1,M1),
	two_dice(1,4,M2),
	two_dice(3,2,M3),
	two_dice(2,3,M4).
