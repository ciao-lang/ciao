:- module(_,_,[functional,lazy]).

:- lazy fun_eval nums_from/1.

nums_from(X) := [X | nums_from(X+1)].

% nums_from(0,[X|_]).
% nums_from(0,[X,Y,Z|_]).

take(N, [H|T]) := N = 0 ? [] 
               |  N > 0 ? [H | take(N-1, T)].

%% take(0, _)     := [].
%% take(X, [H|T]) := [H | take(X-1, T)] :- X > 0.

% nums_from(0,_Y), take(5,_Y,M).
% nums_from(0,Y), take(5,Y,M).

nums(N) := ~take(N,nums_from(0)).

%% evens(N) := ~take(N,nums_from(0)).

