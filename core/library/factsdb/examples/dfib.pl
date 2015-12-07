/*-----------------------------------------------------------------------------
  Fibonacci numbers with file-cached facts
-----------------------------------------------------------------------------*/
:- use_package(factsdb).

:- facts(fib0/2,fibdb).

dfib(M,N) :-
	fib0(M,N), !.
dfib(M,N) :-
	M > 1, 
	M1 is M-1, 
	M2 is M-2,
	dfib(M1,N1), 
	dfib(M2,N2),
	N is N1+N2.
