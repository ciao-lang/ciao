:- module(size, [mem/0, mem_/1], []).
%:- use_module(library(format)).
%:- use_module(library(prolog_sys)).

/* Test memory usage: construct a large integers list, then double
        copy it, and make another list doubling each element */

mem :-
        %statistics,
        %statistics(runtime, _),
        size(S),
        mem_(S).
        %statistics(runtime, [_|T]),
        %format("Used ~d milliseconds~n", T),
        %statistics.

size(10000).

mem_(S) :-
        int_list(S, Il), 
        copy_list(Il, S1), 
        double_list(S1, _Sd).


/* Bad code on purpose: leave choice points */

int_list(N, [Nc|List]):-
        N > 0,
        N1 is N - 1,
        int_list(N1, List),
        N = Nc.
int_list(N, [0]):-
        N < 1.

copy_list([A], [A]).
copy_list([A,B|L], [Ac,B1|List]):-
        copy_list([B|L], [B1|List]),
        A = Ac.

double_list([A], [A2]):- A2 is A * 2.
double_list([A,B|L], [A2, B2|L2]):-
        double_list([B|L], [B2|L2]),
        A2 is A * 2.
