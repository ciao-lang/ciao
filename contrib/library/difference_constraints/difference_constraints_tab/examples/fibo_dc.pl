 %% - Call examples: f(10,F), f(N,89).
 %% - You can commet line ':- table fib/2.' not to use tabling
 %% - times(N,fib(N,F)) repeats the execution of fib(N,F)
 %%   to get its execution time on average.
 %%    N is the minimum time we want to execute times/2.

:- module(fibo_dc, 
	[
	    times/2,
	    fib/2
	], []).

:- use_package(library(tabling)).
:- const_table_module(library(difference_constraints/difference_constraints_tab)).
:- use_package(library(difference_constraints)).

:- include(times).

:- table fib/2.

fib(N, F) :- 
	N #= 0, 
	F #= 0.
fib(N, F) :- 
	N #= 1,
	F #= 1.
fib(N, F):-
	N #>= 2,
 	N1 #= N - 1,
  	N2 #= N - 2,
 	F1 #=< F,
 	F2 #=< F1,
        fib(N1, F1),
        fib(N2, F2),
	F #= F1 + F2. %this is possible since F1, F2 are always integers here.
