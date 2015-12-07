:- use_package(fd).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format)).
:- use_module(library(lists)).


nat(0).
nat(N):- nat(N1), N is N1 + 1.

fib(0, 0).
fib(1, 1).
fib(N, F):-
        N > 1,
        N1 is N - 1,
        N2 is N - 2,
        fib(N1, F1),
        fib(N2, F2),
        F is F1 + F2.
        

fibfd(0, 0).
fibfd(1, 1).
fibfd(N, F):-
        N .>. 1,
        N1 .=. N - 1,
        N2 .=. N - 2,
        fibfd(N1, F1),
        fibfd(N2, F2),
        F .=. F1 + F2.
%        labeling([F]).

fibfd_ordered(0, 0).
fibfd_ordered(1, 1).
fibfd_ordered(N, F):-
        N .>. 1,
        N1 .=. N - 1,
        N2 .=. N - 2,
        F .=. F1 + F2,
        fibfd_ordered(N1, F1),
        fibfd_ordered(N2, F2).
%        labeling([F]).
