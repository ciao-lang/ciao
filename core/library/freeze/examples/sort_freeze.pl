:- module(_, _).

:- use_module(library(freeze)).
:- use_module(library(random), [random/3, srandom/1]).

% Example from B Prolog

go:-
	go_(12, L, SortedL, T),
	write(L), nl, 
	write(SortedL), nl,
	write('execution time is :'),write(T).

go_(Size, L, SortedL, T):-
    srandom(Size), 
    generate_random(L, Size),
    length(SortedL, Size), 
    statistics(runtime,[Start|_]),
    psort(L,SortedL),
    statistics(runtime,[End|_]),
    T is End-Start.

generate_random([], 0).
generate_random([H|T], N):-
	random(0, 1000, H), 
	M is N - 1, 
	generate_random(T, M).

psort(X,Y):-sorted(Y),permutation(X,Y).

sorted([_]):-!.
sorted([X,Y|L]):-
    freeze(X,freeze(Y,X=<Y)),
    sorted([Y|L]).

permutation([],Xs):-Xs=[].
permutation([X|Xs],L):-
    L=[Y|Ys],
    myselect([X|Xs],Rest,Y),
    permutation(Rest,Ys).

myselect([X|Xs],Xs,X).
myselect([Y|Ys],[Y|Zs],X) :- myselect(Ys,Zs,X).


%%% test %%% 

test(Size, T):-
	go_(Size, L, SL, T), 
	check_output(SL, L, 0).

check_output([H|T], L, Prev):-
	H >= Prev, 
	select(H_, L, L_), H==H_, 
	check_output(T, L_, H).
check_output([], [], _).