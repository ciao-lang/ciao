/*
1.6 Program to test dereferencing  (deref(N)).


/* Program to benchmark the dereferencing speed.
 * It constructs a list containing 500 variables which are
 * then bound together. Since different systems use different
 * strategies for binding variables on the global stack,
 * the whole is made for two lists
 * and the long variable chain is created only in one of them.
 * No compensation loop is therefore necessary.
 * Suggested value for N is over 100;
 * Results for Cprolog, N=200:
 * Tfirst loop=1.23 Tsecond loop=9.4 Tnet=8.16 Klips=0.29
 */

:- module(deref,[deref/1], []).

:- use_module(benchmark_utilities).

/*-------------------CUT HERE (BEGINNING OF PROGRAM)-----------*/

deref(N):-
        make_list(500, L1, _),
        make_list(500, L2, Last),
        bind_forward(L1),
        bind_backward(L2),
        L2 = [a|_],
        common_loop(3, N, bind_refs(L1, Last), 24, deref).

/*
 * Bind repeatively a cons cell to another one.
 */

bind_refs(First, Last):-
        ref(First),
        ref(Last).

ref(Cons) :-
        Cons = [a|_],
        Cons = [a|_],
        Cons = [a|_],
        Cons = [a|_],
        Cons = [a|_],
        Cons = [a|_],
        Cons = [a|_],
        Cons = [a|_],
        Cons = [a|_],
        Cons = [a|_].

/*
 * Create a variable chain if in ?- equal(X, Y) the system binds
 * X to Y.
 */
bind_forward([a]) :- !.
bind_forward([X, Y|T]) :-
        equal(X, Y),
        bind_forward([Y|T]).

/*
 * Create a variable chain if in ?- equal(X, Y) the system binds
 * Y to X.
 */
bind_backward([_X]) :- !.
bind_backward([X, Y|T]) :-
        bind_backward([Y|T]),
        equal(X, Y).

equal(X, X).

/*
 * Create a list containing variables and return the pointer to the
 * first and to the last cons cell.
 */
make_list(1, L, L) :- L = [_X].
make_list(N, [_X|Rest], Last) :-
        N > 1,
        N1 is N - 1,
        make_list(N1, Rest, Last).
/* ----------------CUT HERE (END OF PROGRAM)------------------ */
