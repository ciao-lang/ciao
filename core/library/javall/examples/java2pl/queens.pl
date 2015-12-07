%% Sicstus 0.7, pizarro, debug code (-g, no -O): 5670ms

:- module(queens, [queens/2]).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format), [format/2]).

:- set_prolog_flag(multi_arity_warnings,off).

queens:-
        statistics(runtime, _),
        do_queens,
        statistics(runtime, [_|T]),
        format("Used ~d milliseconds~n", T).

do_queens:-
        queens(11, _), fail.
do_queens.

queens(N, Qs):-
        queens_list(N, Ns),
        queens3(Ns, [], Qs).    % To place, placed, result

queens3([], Qs, Qs).
queens3(Unplaced, Placed, Qs):-
        sel(Q, Unplaced, NewUnplaced),
        no_attack_(Q, Placed),
        queens3(NewUnplaced, [Q|Placed], Qs).

no_attack_(Q, Safe):- no_attack(Safe, Q, 1).

no_attack([], _Queen, _Nb).
no_attack([Y|Ys], Queen, Nb):-
        Queen =\= Y + Nb,
        Queen =\= Y - Nb,
        Nb1 is Nb + 1,
        no_attack(Ys, Queen, Nb1).

sel(X, [X|Ys], Ys).
sel(X, [Y|Ys], [Y|Zs]):-
        sel(X, Ys, Zs).

queens_list(0, []).
queens_list(N, [N|Ns]):-
        N > 0,
        N1 is N - 1,
        queens_list(N1, Ns).
