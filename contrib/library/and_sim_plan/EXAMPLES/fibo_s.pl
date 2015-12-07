 %% AMADEO 14826 9111 5997 5679 4230 3417 2973
 %% Tiempo de secuencial 9138
 %% Tiempo PAR BACK 1806 1452 1044 861 702 630 630 
 %% Tiempo de memorizacion secuencial 9138

%%sim(fib(3,N),2,[all]).
:- module(fibo_s,
        [
            fib/2
	],
	[]).

:- use_package(and_sim_plan).


fib(0, 0) :- !.
fib(1, 1) :- !.
fib(N, F) :-
        N > 1,
        N1 is N - 1,
        N2 is N - 2,
        fib(N1, F1) '&'
        fib(N2, F2),
        F is F1 + F2.
