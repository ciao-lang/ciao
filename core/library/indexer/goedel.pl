
:- module(goedel,[ goedel/2 ], []).

/*
:- pred goedel(T,N) :: ground * num
	# "@var{N} is the goedel number of @var{T}.".
:- pred goedel(T,N) :: nonground * var.

Note that N is an integer, although in float point representation!!
*/

:- use_module(library(lists), [length/2]).
:- use_module(library(numlists), [get_primes/2]).

goedel(T,N):-
	ground(T), !,
	goedel_(T,N).
goedel(_,_).

goedel_(T,N):-
	atomic(T), !,
	name(T,L),
	odd_numbers(L,Odds),
	( Odds = [N] -> true
	; goedel_number(Odds,N)
	).
goedel_(T,N):-
	T =.. TList,
	hash_list(TList,NList),
	goedel_number(NList,N).

hash_list([],[]).
hash_list([X|Xs],[N|Ns]):-
	goedel_(X,N),
	hash_list(Xs,Ns).

goedel_number(List,N):-
	length(List,L),
	get_primes(L,PList),
	prod_of_pows(PList,List,1,N).

prod_of_pows([],[],N,N).
prod_of_pows([Pr|Prs],[Po|Pos],N0,N):-
	N1 is N0*(Pr**Po),
	prod_of_pows(Prs,Pos,N1,N).

odd_numbers([],[]).
odd_numbers([N|Ns],[O|Os]):-
	O is 2*N+1,
	odd_numbers(Ns,Os).
