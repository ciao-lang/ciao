%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TCLP(clpR) example with fibonacci benchmark
% calling:
%
% ?- fibonacci(N,832040)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module(fibo_tclp_r,
	[
	    test/1, 
	    result/1,
	    fibonacci/2
	]).

:- use_package(tabling).
:- use_package(t_clpr).

:- table fibonacci/2.

fibonacci(0, 0).
fibonacci(1, 1).
fibonacci(N, F) :-
	N .>=. 2,
	N1 .=. N - 1,
	N2 .=. N - 2,
	F1 .>=. 0,
	F2 .>=. 0,
	F .=. F1 + F2,
	fibonacci(N1, F1),
	fibonacci(N2, F2).


test(N) :- 
	fibonacci(N, 832040).

result(A) :-
	A .=. 30.