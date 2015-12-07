%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% jugs.pl -- The jugs problem coded in prolog
 %% AFSID           : $__Header$
 %% Author          : Manuel Carro Li~nares
 %% Created On      : Sun Feb 28 22:20:48 1993
 %% Last Modified By: MCL
 %% Last Modified On: Tue Jun  8 19:12:48 1999
 %% Update Count    : 24
 %% Status          : Lengthy and verbose, but correct.

 %% Problem:
 %% Write the prolog code for the water jugs problem:
 %% There are two jugs, one holding 3 and the other 5 gallons of water.
 %% A number of things can be done with the jugs:  they can be filled,
 %% emptied, and dumped one into the other either until the poured-into
 %% jug is full or until the poured-out-of jug is empty.  Devise a
 %% sequence of actions that will produce 4 gallons of water in the larger
 %% jug. Note that only integer values of water will be used.


 %% First of all, the problem, as I see it, is badly specified; if 4
 %% gallons of water have to remain in the larger jug, there is a smaller
 %% jug, isn't it? The text you posted (above) does not mention anything
 %% about it, but I vaguely understand that the jugs can contain 5 and 3
 %% gallons of water, so they both are full at the beginning. Well, let's
 %% start... 

 %% I prefer clear code as long as it does not impede performance...

 %% The basic data structure is a functor
 %% 
 %% jug(Water)
 %% 
 %% and the state of the problem is given by
 %% 
 %% state(SmallJug, LargeJug)
 %% 
 %% Actions are described as functors of the form
 %% 
 %% action(Action, Jug, Amount)
 %% 
 %% or
 %%
 %% action(Action, Jug)
 %% 
 %% where Amount refers to an integer number of gallons of water,
 %% Jug can be small or large and action can be empty, fill and pour, the
 %% latter meaning that the jug alluded to is poured a given amount of water
 %% into the other jug.
 %%
 %% The solution is given as a list of functors of the form
 %%
 %% step(Action, Resulting)
 %%
 %% Its meaning is obvious.
 %% 


:- module(jugs, [solve_jugs_n/1, solve_jugs/1]).

:- use_module(library(between)).

solve_jugs_n(N) :-
	between(1, N, _),
	solve_jugs(_),
	fail
 ;
	true.

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% solve_jugs(?ListOfSteps): ListOfSteps is the solution to the jugs
 %% problem.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve_jugs(Solution):-
	Initial = state(jug(3),jug(5)),
	Final = state(jug(_),jug(4)),
	solve_jugs_(Initial, Final, [], Solution).



 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% solve_jugs(+CurrentState, ?GoalState, +StatesSoFar, ?Steps): Steps
 %% describe the history of the system from CurrentState to GoalState,
 %% having been traversed StatesSoFar.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve_jugs_(End, End, _States, []).
solve_jugs_(Actual, Goal, StatesSoFar, [step(Action, NextState)|Steps]):-
	\+ member(Actual, StatesSoFar),
	change_state_(Actual, NextState, Action),
	solve_jugs_(NextState, Goal, [Actual|StatesSoFar], Steps).



 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% change_state(+State, ?NextState, ?Action): Action is legal, and changes
 %% State into NextState
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change_state_(state(J1,J2), state(NJ1,NJ2),A):-
	change_state(J1, J2, NJ1, NJ2, A).

change_state(jug(W1), W2, jug(0), W2, action(empty, small)):- W1 > 0.
change_state(W1, jug(W2), W1, jug(0), action(empty, large)):- W2 > 0.

change_state(jug(W1), W2, jug(3), W2, action(fill, small)):-  W1 < 3.
change_state(W1, jug(W2), W1, jug(5), action(fill, large)):-  W2 < 5.

change_state(jug(W1),jug(W2),jug(0),jug(NW2),action(pour,small,W1)):-
	W1 > 0,
	W1 + W2 =< 5,
	NW2 is W1 + W2.
change_state(jug(W1),jug(W2),jug(NW1),jug(5),action(pour,small,Am)):-
	W1 > 0,
	W1 + W2 > 5,
	NW1 is W1 + W2 - 5,
	Am is W1 - NW1.

change_state(jug(W1),jug(W2),jug(NW1),jug(0),action(pour,large,W2)):-
	W2 > 0,
	W1 + W2 =< 3,
	NW1 is W1 + W2.
change_state(jug(W1),jug(W2),jug(3),jug(NW2),action(pour,large,Am)):-
	W2 > 0,
	W1 + W2 > 3,
	NW2 is W1 + W2 - 3,
	Am is W2 - NW2.

