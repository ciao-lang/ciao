:- module(nqueens_clpfd, [queens_clpfd/2], [clpfd]).

% N-queens puzzle using CLP(fd).
% It solves much larger instances than queens.pl

% Example: ?- queens_clpfd(50, R).

:- use_module(library(lists)).

queens_clpfd(N, L) :- queens_(N, L, [ff]).

queens_(N, L, Lab) :-
    length(L, N),
    domain(L, 1, N),
    safe(L),
    labeling(Lab, L).

safe([]).
safe([X|L]) :-
    noattack(L, X, 1),
    safe(L).

noattack([], _, _).
noattack([Y|L], X, I) :-
    diff(X, Y, I),
    I1 is I + 1,
    noattack(L, X, I1).

diff(X,Y,I):-
    X#\=Y,
    % abs(X - Y) #\= I,
    X#\=Y+I,
    X+I#\=Y.


