:- module(qsort, [qsort/2], []).

% Example: ?- qsort([5,2,3,4,1],X).

qsort(Data, Out) :-
    qsort_(Data, Out, []).

qsort_([], R, R).
qsort_([X|L], R, R0) :-
    partition(L, X, L1, L2),
    qsort_(L2, R1, R0),
    qsort_(L1, R, [X|R1]).

partition([],_,[],[]).
partition([X|L],Y,[X|L1],L2) :-
    X =< Y, !,
    partition(L,Y,L1,L2).
partition([X|L],Y,L1,[X|L2]) :-
    partition(L,Y,L1,L2).

