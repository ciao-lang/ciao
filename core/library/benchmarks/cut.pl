/*
1.7. Program to test the cut operation.

In fact, it seems almost impossible to isolate the cut operator
in a simple test program. However, the following program
contains a lot of cut at exec time. It may be regarded as a
partial test of cut, and may be worthwhile for some software
implementations of Prolog.  cuttest(N), where N is the
repetition number, calls the cutit11 predicate, which performs
100 calls to a predicate cutt1 where a cut operator appears in
the second clause. Having indexing makes the evaluation of the
cut more accurate, so please indicate in our result whether or
not your Prolog system uses indexing, to clarify the
comparison with others.

-------------------CUT HERE (BEGINNING OF PROGRAM)-----------
*/

:- module(cut,[cuttest/1]).

:- use_module(benchmark_utilities).

cuttest(N):- common_loop(3, N, cutit, 300, cuttest).


cutit:- 
        cutt1([100,100,100,100,100,100,100,100,100,100,
               100,100,100,100,100,100,100,100,100,100,
               100,100,100,100,100,100,100,100,100,100,
               100,100,100,100,100,100,100,100,100,100,
               100,100,100,100,100,100,100,100,100,100,
               100,100,100,100,100,100,100,100,100,100,
               100,100,100,100,100,100,100,100,100,100,
               100,100,100,100,100,100,100,100,100,100,
               100,100,100,100,100,100,100,100,100,100,
               100,100,100,100,100,100,100,100,100,100]).

cutt1([]).
cutt1([X|L]):-X=100, !, cutt1(L).
cutt1([X|L]):-X > 100, cutt1(L).

/* ----------------------CUT HERE (END OF PROGRAM)---------- */
