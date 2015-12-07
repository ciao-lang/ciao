/*-----------------------------------------------------------------------------
  Fibonacci numbers with (persistent, file-cached) memoization
-----------------------------------------------------------------------------*/
:- use_package(factsdb).
:- use_package(persdb).

%% Not needed, since there is a persistent_dir
%% :- multifile file_alias/2.
%% :- data file_alias/2.
%% file_alias(fib0db,persdb).
persistent_dir(fib0db,persdb).

fib0(0,0).

:- facts(fib0/2,fib0db).
:- persistent(fib0/2,fib0db).

fib(M,N) :-
	fib0(M,N), !.
fib(M,N) :-
	M > 1, 
	M1 is M-1, 
	M2 is M-2,
	fib(M1,N1), 
	fib(M2,N2),
	N is N1+N2,
	asserta_fact(fib0(M,N)).
