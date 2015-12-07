:- use_package(fd).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format)).

% solve the magic square puzzle for a N*N board
:- use_module(library(lists)).

magic_square(N,VSq) :-
	statistics(runtime,_),
	do_magic_square(N, VSq),
	statistics(runtime, [_,Time]),
	format("Used ~d milliseconds~n", Time).

do_magic_square(N, VSq) :-
        Last is N*N,
        length(Vars, Last),
        Vars in 1..Last,
        all_different(Vars),
        Sum is integer(N*(N**2 + 1)/2), %knowing the sum makes things easier
        all_prefix(Vars, N, Sum, VSq),
        do_cols(VSq, Sum),
        do_diags(VSq, N, Sum),
        labeling(Vars).

do_diags(VSq, N, Sum) :-
        do_diag1(VSq, 1, Sum),
        do_diag2(VSq, N, Sum).

do_diag1([],_,0).
do_diag1([H|T], N, Sum) :-
        nth(N, H, HElt),
        N1 is N + 1,
        Sum .=. HElt + TSum,
        do_diag1(T, N1, TSum).

do_diag2([],_,0).
do_diag2([H|T], N, Sum) :-
        nth(N, H, HElt),
        N1 is N - 1,
        Sum .=. HElt + TSum,
        do_diag2(T, N1, TSum).

do_cols([[]|_],_).
do_cols(List, Sum) :-
        do_col(List, Sum, Tails),
        do_cols(Tails, Sum).

do_col([],0, []).
do_col([[H|T]|T1], Sum, [T|T2]) :-
        Sum .=. H + TSum,
        do_col(T1, TSum, T2).


%split up big list into a list of lists
%and place constraints on the way
all_prefix([],_,_,[]).
all_prefix(List, N, Sum, [List1|Rest]) :-
        prefix(List, N, Sum, List1, Residue),
        all_prefix(Residue, N, Sum, Rest).

%strip the first N elements of list and constrain
%them to add up to Sum
%also return what's left
prefix(List,0,0,[],List).
prefix([H|T],N,Sum,[H|T2],Out) :-
        N > 0,
        N1 is N - 1,
        Sum .=. H + TSum,
        prefix(T, N1, TSum, T2, Out).


%% write_sq([]) :- nl.
%% write_sq([H|T]) :-
%%         write_row(H),
%%         write_sq(T).
%% 
%% write_row([]) :- nl.
%% write_row([H|T]) :-
%%         write(H), %should format appropriately
%%         write_row(T).
