:- module(fibo_tclp_dc,
	[
	    test/1,
	    result/1,
	    fibonacci/2
	    ]).

:- use_package(tabling).
:- use_package(library(difference_constraints/difference_constraints_tab)).
:- table fibonacci/2.

fibonacci(N, F) :- 
	N #= 0, 
	F #= 0.
fibonacci(N, F) :- 
	N #= 1,
	F #= 1.
fibonacci(N, F):-
	N #>= 2,
 	N1 #= N - 1,
  	N2 #= N - 2,
 	F1 #=< F,
 	F2 #=< F1,
        fibonacci(N1, F1),
        fibonacci(N2, F2),
	F #= F1 + F2. %this is possible since F1, F2 are always integers here.


test(N) :- 
  fibonacci(N, 832040).

result(N) :-
	N = 30.