:- module(_, _).

:- use_module(library(freeze)).

% Example from B Prolog

/* nreverse.pl */

top:-
    mklist(100,L),
    nrev(L,_).

go:-
	go_(500, L, L1, T),
	write(L),nl,
	write(L1),nl,
	write('execution time is :'),write(T).


go_(Size, L, L1, T):- 
    mklist(Size,L),
    statistics(runtime,[Start|_]),
    nrev(L,L1),
    statistics(runtime,[End|_]),
    T is End-Start.

mklist(N,L):-
    N=:=0,!,
    L=[].
mklist(N,L):-
    L=[N|L1],
    N1 is N-1,
    mklist(N1,L1).

nrev([],L):-
    L=[].
nrev([X|Xs],L):-
    concat(L1,[X],L),
    nrev(Xs,L1).

concat(X,Y,Z):-freeze(X,concat1(X,Y,Z)).

concat1([],L1,L2):-
    L2=L1.
concat1([X|Xs],L1,L2):-
    L2=[X|L3],
    concat(Xs,L1,L3).




%%%% Test %%%% 

test(Size, T):-
	go_(Size, _L, L1, T), 
	check_output(L1, 0, Size).

check_output(L, N0, N2):-
	functor(L, '.', 2),!, 
	L = [M|T],
	N1 is N0 + 1,
	M == N1, 
	check_output(T, N1, N2).
check_output(L, N, N):- L == [].