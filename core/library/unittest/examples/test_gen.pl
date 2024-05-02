:- module(_, [], [assertions, nativeprops, basicmodes, unittestdecls/*, rtchecks*/]).

% Test for generation, multiple times, solutions, timeouts, and aborts.

% Other process: ?- run_tests(test_gen, [], [check, show_results])).
% Same process: ?- run_tests(test_gen, [sameproc], [check, show_results])).
% Note that abort will stop the test driver if run with 'sameproc'.

:- use_module(engine(io_basic)).

:- use_module(library(unittest/unittest_props), [try_sols/2]).

:- compilation_fact(do_abort).
:- compilation_fact(do_pause).
:- compilation_fact(do_loop).

:- if(defined(do_abort)).
do_abort :- display(user_error, abortedonerr), nl(user_error), display(abortedonout), nl, halt(1).
:- else.
do_abort.
:- endif.

:- if(defined(do_pause)).
:- use_module(library(system), [pause/1]).
do_pause :- pause(1).
:- else.
do_pause.
:- endif.

:- if(defined(do_loop)).
do_loop :- repeat, fail. % loop forever
:- else.
do_loop.
:- endif.

:- export(q/2).

:- test q(A, B) : (A = a) => (B = b)
     # "test 3 should pass".

q(a, b) :- do_abort.
q(a, b) :- do_pause.

:- export(r/2).

:- test r(A, B) : (A = a) => (B = b) + timeout(100)
     # "test 4 should timeout".

r(a, b) :- do_loop.

:- export(p/2).

:- test p(A, B) : (A = a) => (B = b)
     # "test 1 should pass".
:- test p(A, B) : (A = a) => (B = b) + try_sols(4)
     # "test 2 should pass".
:- test p(A, B) : (A = c; A = a ; A = b) => (B = b) + ( generate_from_calls_n(3), try_sols(4) )
     # "test 3 should pass".
:- test p(A, B) : ((true;true;true), (A = c; A = a ; A = b)) => (B = b) + ( generate_from_calls_n(6), try_sols(4) )
     # "test 4 should pass". % (repeat 3 times, 3 different calls)

% :- pred p(A,B) : term(A) => (B=c).
   
p(a, b) :- do_pause.
p(a, b) :- do_pause.
p(a, b) :- do_abort.
p(a, b) :- do_pause.
p(a, a) :- do_pause. % NOTE: error here prevents execution of the last one
p(a, c) :- do_pause.

% NOTE: uncomment to raise a 'comp_error' (compilation error) message to unittest driver
%foo :- bar.


