/*
1.4. Program to test indexing mechanisms.

We give only one test for indexing, i.e. the selection of a
clause due to the type of an argument. This program does not test
the merits of indexing on an argument other than the first one.
It does not test for multiple indexing either. It does not show
the inefficiency which occurs if 2 choice points per clause
are created. This may happen e.g. in Warren's indexing scheme.

Each of these tests would require an extra benchmark program.
The program given below tests the main point in indexing. Right now
we think it is not worth adding all this complexity to the
benchmarks, in order to measure all the details in indexing.
Therefore we give only this single test.

---------cut here - beginning of program listing---------------
*/

:- module(index,[index/1]).

:- use_module(benchmark_utilities).

/* This program is called with "index(N)"                         */
/* It tests the efficiency of simple indexing on the 1st argument */
/* suggested value for N: 500 (interp), 2000(comp) */
/* results for Cprolog: N=500  */
/* Tloop=8.98 Tcomp=0.52 Tnet=8.47 Klips=1.24  */

index(N):- common_loop(3, N, index_clause, 21, index).

/* loop with calls to the actual benchmark program for indexing */
index_clause :-
        p(a),
        p([a]),
        p(s(a)),
        p(b),
        p([b]),
        p(t(b)),
        p(c),
        p([c]),
        p(u(c)),
        p(d),
        p([d]),
        p(v(d)),
        p(e),
        p([e]),
        p(w(e)),
        p(f),
        p([f]),
        p(x(f)),
        p(g),
        p([g]),
        p(y(g)).

/* test program which can be optimised by indexing */
p(a).
p([a]).
p(s(a)).
p(b).
p([b]).
p(t(b)).
p(c).
p([c]).
p(u(c)).
p(d).
p([d]).
p(v(d)).
p(e).
p([e]).
p(w(e)).
p(f).
p([f]).
p(x(f)).
p(g).
p([g]).
p(y(g)).


/* -------------------cut here - end of program listing-------------- */
