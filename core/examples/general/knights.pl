:- module(knights, [knights/2], []).

% Example: ?- knights(5, P).

knights(N, P) :-
    path(N, 1, 1, P), !.

member_([X|_], X).
member_([_|Xs], X) :- member_(Xs, X).

path(S, X, Y, Path) :-
    S2 is S * S - 1,
    path_2(S2, [(X, Y)], S, Path).

path_2(0, Path, _, Path).
path_2(Left0, Path0, S, Path) :-
    Path0 = [P|_],
    P = (X, Y),
% display(P), nl,
    move(X, Y, X1, Y1),
    valid(X1, Y1, S),
    New = (X1, Y1),
% display(New), nl,
    \+ member_(Path0, New),
    Left is Left0 - 1,
    path_2(Left, [New|Path0], S, Path).
valid(X, Y, S) :-
    bound(X, S),
    bound(Y, S).
bound(X, S) :- X >= 1, X =< S.
move(X, Y, X1, Y1) :-
    delta(A, B),
    move_1(X, X1, A),
    move_1(Y, Y1, B).
delta(1, 2).
delta(2, 1).
move_1(X, X1, A) :- X1 is X + A.
move_1(X, X1, A) :- X1 is X - A.

