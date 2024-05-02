:- module(test_timeout, [q/2,test_pred/1], [assertions,dynamic]).

:- doc(author, "Ignacio Casso").
:- doc(author, "Manuel Hermenegildo").

% Not working yet? 
% :- set_prolog_flag(unittest_default_timeout,500).

:- test q(X,Y) => (Y=1 ; Y=2) + (try_sols(1), timeout(500)) # "Should succeed.".
:- test q(X,Y) => (Y=1 ; Y=2) + (try_sols(2), timeout(500)) # "Should time out (on second solution).".

q(a,1).
q(b,2) :- q(b,2).

:- test test_pred(N) : (N=5)                     # "Timeout test 1,  should probably not time out.".
:- test test_pred(N) : (N=50)                    # "Timeout test 2a, should probably not time out.".
:- test test_pred(N) : (N=50)    + timeout(5)    # "Timeout test 2b, should time out.".
:- test test_pred(N) : (N=500)   + timeout(2000) # "Timeout test 3,  should probably not time out.".
:- test test_pred(N) : (N=1000)  + timeout(500)  # "Timeout test 4,  should probably time out.".
:- test test_pred(N) : (N=4000)  + timeout(500)  # "Timeout test 5,  should probably time out.".
:- test test_pred(N) : (N=10000) + timeout(500)  # "Timeout test 6,  should probably time out.".
:- test test_pred(N) : (N=20000) + timeout(500)  # "Timeout test 7,  should probably time out.".

:- dynamic p/1.

% A test predicate with multiple solutions; branches progressively longer
test_pred(N) :-
    N1 is N,
    test_pred__(N1).
test_pred(N) :-
    N1 is 2*N,
    test_pred__(N1).
test_pred(N) :-
    N1 is 4*N,
    test_pred__(N1).

test_pred__(N) :-
    abolish(p/1),
    N1 is N*1000,
    test_pred_(N1).

test_pred_(0).
test_pred_(N) :-
    N>0,
    do_something,
    N1 is N-1,
    test_pred_(N1).

do_something :-
    assert(p(a)),
    retract(p(_)).
