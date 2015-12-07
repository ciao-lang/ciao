:- module(_, [fib/2], []).
:- use_package(clpr).

fib(X,Y):- X .=. 0, Y .=. 0.
fib(X,Y):- X .=. 1, Y .=. 1.
fib(N,F) :-
        N .>. 1,
        N1 .=. N - 1,
        N2 .=. N - 2,
        fib(N1, F1),
        fib(N2, F2),
        F .=. F1+F2.
