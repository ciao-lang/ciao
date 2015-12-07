:- module(get_board,
        [
            get_board/5
	],
	[]).

:- use_module(library(random)).


get_board(Type,P,C,L,R) :- 
	iterate_col(C,Type,P,L,R).


iterate_col(0,_,_,_,[]) :- !.
iterate_col(C,Type,P,L,[R|Rs]) :-
	C1 is C - 1,
	get_line(L,Type,P,R),
	iterate_col(C1,Type,P,L,Rs).

get_line(0,_,_,[]) :- !.
get_line(L,Type,P,[R|Rs]) :-
	L1 is L - 1,
	get_number(Type,P,R),
	get_line(L1,Type,P,Rs).

get_number(bin,P,R) :-
	random(1,10,N),
	N =< P,
	!, R = 1.

get_number(bin,_P,0).

get_number(dec,_,R) :-
	random(1,10,R).