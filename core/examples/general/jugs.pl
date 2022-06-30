:- module(jugs, [solve_jugs/1], []).

%% solve_jugs(?ListOfSteps): ListOfSteps is the solution to the jugs
%% problem.

solve_jugs(Solution):-
    Initial = state(jug(3),jug(5)),
    Final = state(jug(_),jug(4)),
    solve_jugs_(Initial, Final, [], Solution).

%% solve_jugs(+CurrentState, ?GoalState, +StatesSoFar, ?Steps): Steps
%% describe the history of the system from CurrentState to GoalState,
%% having been traversed StatesSoFar.

solve_jugs_(End, End, _States, []).
solve_jugs_(Actual, Goal, StatesSoFar, [step(Action, NextState)|Steps]):-
%   console.display(foo(Actual, StatesSoFar)), console.nl,
    \+ member(Actual, StatesSoFar),
%   console.display(bar(Actual, StatesSoFar)), console.nl,
    change_state_(Actual, NextState, Action),
    solve_jugs_(NextState, Goal, [Actual|StatesSoFar], Steps).

%% change_state(+State, ?NextState, ?Action): Action is legal, and changes
%% State into NextState

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

%% member(?Element, ?List): Element is an element of list List

%% member(Element, [Element|_]).
%% member(Element, [_|RestOfElements]):-
%%      member(Element, RestOfElements).
member(X, [Y|Xs]) :- X = Y ; member(X, Xs).
