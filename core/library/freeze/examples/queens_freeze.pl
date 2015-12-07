:- module(_, _).

:- use_module(library(freeze)).

:- data counter/1.
counter(0).

% Example from B Prolog

/* queens.pl */
go:-
	go_(12, T),
	write('execution time is :'),write(T).


go_(Size, T):-
    statistics(runtime,[Start|_]),
    queens(Size),
    statistics(runtime,[End|_]),
    T is End-Start.
    
queens(N):-
    make_list(N,List),
    range(1,N,D),
    constrain_queens(List),
    label(List,D),
    retract_fact(counter(I)), J is I +1, assertz_fact(counter(J)),
    %write(List),nl,
    fail.
queens(_).

constrain_queens([]).
constrain_queens([X|Y]):-
    safe(X,Y,1),
    constrain_queens(Y).

safe(_,[],_).
safe(X,[Y|T],K):-
    freeze(X,freeze(Y,noattack(X,Y,K))),
    K1 is K+1,
    safe(X,T,K1).

noattack(X,Y,K):-
    X =\= Y,
    X+K =\= Y,
    X-K =\= Y.

make_list(0,[]):-!.
make_list(N,[_|Rest]):-
    N1 is N-1,
    make_list(N1,Rest).

range(N,N,[N]) :- !.
range(M,N,[M|Ns]) :-
	M < N,
	M1 is M+1,
	range(M1,N,Ns).

label([],_D).
label([V|Vs],D):-
    myselect(D,Rest,V),
    label(Vs,Rest).
    
myselect([X|Xs],Xs,X).
myselect([Y|Ys],[Y|Zs],X) :- myselect(Ys,Zs,X).


%%% test %%%

test(Size, T):-
	retractall_fact(counter(_)),
	assertz_fact(counter(0)), 
	go_(Size, T), 
	retract_fact(counter(I)), 
	sol(Size, I).


sol(1,1).
sol(2,0).
sol(3,0).
sol(4,2).
sol(5,10).
sol(6,4).
sol(7,40).
sol(8,92).
sol(9,352).
sol(10,724).
sol(11,2680).
sol(12,14200).
sol(13,73712).
sol(14,365596).
sol(15,2279184).
sol(16,14772512).
sol(17,95815104).
sol(18,666090624).
sol(19,4968057848).
sol(20,39029188884).
sol(21,314666222712).
sol(22,2691008701644).
sol(23,24233937684440).
sol(24,227514171973736).
sol(25,2207893435808352).
sol(26,22317699616364044).
sol(X, _):-
	X >= 27, 
	message(warning, 'queens_freeze : test no check').