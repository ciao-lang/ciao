:- module(loops, [repeat/1],[assertions]).

repeat(N) :- N > 0.
repeat(N) :- N > 0, N1 is N-1, repeat(N1).
