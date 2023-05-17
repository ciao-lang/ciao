:- module(nqueens, [queens/2], [fsyntax]).

% N-queens puzzle (plai Prolog)

% Examples:
% ?- queens(10, R).
% ?- queens(25, R).

queens(N, Qs):-
    Ns = ~queens_list(N),
    queens_2(Ns, [], Qs).

queens_2([], Qs, Qs).
queens_2(Unplaced, Placed, Qs) :-
    sel(Q, Unplaced, NewUnplaced),
    no_attack(Q, Placed),
    queens_2(NewUnplaced, [Q|Placed], Qs).
  
no_attack(Q, Safe) :- no_attack_2(Safe, Q, 1).

no_attack_2([], _, _).
no_attack_2([Y|Ys], Queen, Nb) :-
    Queen =\= Y + Nb,
    Queen =\= Y - Nb,
    Nb1 is Nb + 1,
    no_attack_2(Ys, Queen, Nb1).

sel(X, [X|Ys], Ys).
sel(X, [Y|Ys], [Y|Zs]) :- sel(X, Ys, Zs).

queens_list(0) := [].
queens_list(N) := [N| ~queens_list(N1)] :- N > 0, N1 is N - 1.


