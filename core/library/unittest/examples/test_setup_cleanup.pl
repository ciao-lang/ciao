:- module(_, _, [assertions, nativeprops, basicmodes, rtchecks, unittestdecls, hiord]).

:- doc(author, "Jos@'{e} Luis Bueno").

:- doc(module, "Some examples of setup and cleanup in unit tests..").

:- use_module(engine(io_basic)).

% ---------------------------------------------------------------------------
% tests only with setup

:- test p + (setup(a))
   # "test should pass".

:- test p + (setup(b))
   # "test should abort due to an exception in setup".

% TODO: This should be detected staticly
:- test p + (setup(d))
   # "test should abort due to undefined setup goal".

:- test p + (setup(c))
   # "test should abort due to a fail in setup".

% ---------------------------------------------------------------------------
% tests only with cleanup

:- test p + (cleanup(a))
   # "test should pass".

:- test p + (cleanup(b))
   # "test should throw a warning due to an exception in cleanup".

% TODO: This should be detected staticly
:- test p + (cleanup(d))
   # "test should throw a warning due to undefined cleanup goal".

:- test p + (cleanup(c))
   # "test should throw a warning due to a fail in cleanup".

% ---------------------------------------------------------------------------
% tests only with setup and cleanup

:- test p + (setup(a), cleanup(a))
   # "test should pass".

:- test p + (setup(b), cleanup(a))
   # "test should abort due to an exception in setup".

:- test p + (setup(a), cleanup(b))
   # "test should throw a warning due to an exception in cleanup".

:- test p + (setup(b), cleanup(b))
    # "test should abort due to an exception in setup".

:- test p + (setup(d), cleanup(a))
   # "test should abort due to undefined setup goal".

:- test p + (setup(a), cleanup(c))
   # "test should throw a warning due to a fail in cleanup".

% ---------------------------------------------------------------------------

p :- true.
a :- true.
b :- throw(b).
c :- fail.

% ---------------------------------------------------------------------------
% More complex example with multiple solutions

:- test nd(A, B) : (A = a) => (B = b)
   + (try_sols(4),
      setup(nd_setup(A)), cleanup(nd_cleanup(A)) )
   # "nd test 1".

:- test nd(A, B) : (A = c; A = a ; A = b) => (B = b)
   + (generate_from_calls_n(3),
      try_sols(4),
      setup(nd_setup(A)), cleanup(nd_cleanup(A)) )
   # "nd test 2".

:- test nd(A, B) : ((true;true;true), (A = c; A = a ; A = b)) => (B = b)
   + (generate_from_calls_n(6),
      try_sols(4),
      not_fails,
      setup(nd_setup(A)), cleanup(nd_cleanup(A)) )
   # "nd test 3".

nd(a, b) :- true.
nd(a, b) :- true.
nd(a, b) :- true.
nd(a, b) :- true.
nd(a, a) :- true. % NOTE: error here prevents execution of the last one
nd(a, c) :- true.

nd_setup(A) :- display('Calling test setup for: '), display(A), nl.
nd_cleanup(A) :- display('Calling test cleanup for: '), display(A), nl.

