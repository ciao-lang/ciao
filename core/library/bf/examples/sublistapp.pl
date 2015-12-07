:- module(sublistapp, [test/0, sublistapp/2], ['bf/af']).

:- push_prolog_flag(unused_pred_warnings, no).

test :- sublistapp([a], [b]).

sublistapp(S, L) <- append(_, S, Y), append(Y, _, L).

append([], L, L) <- .
append([X|Xs], L, [X|Ys]) <- append(Xs, L, Ys).

:- pop_prolog_flag(unused_pred_warnings).
