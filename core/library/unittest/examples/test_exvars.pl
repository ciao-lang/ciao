:- module(test_exvars, [], [assertions, nativeprops, basicmodes]).

% Tests for "existential variables" in test assertions.
%
% Those are variables shared between calls and success parts that are
% not in the head.

% (before exvar fix)
%
% :- export(p/3).
% :- test p(A,B,aux(X,Y)) : (A = f(X,Y)) => (B = f(Y,X))
%    # "test should fail".
% 
% p(f(X,Y), f(X,Y), _).
% 
% :- export(q/3).
% :- test q(A,B,aux(X,Y)) : (A = f(X,Y)) => (B = f(Y,X))
%    # "test should pass".
% 
% q(f(X,Y), f(Y,X), _).

:- export(p/2).
:- test p(A,B) : (A = f(X,Y)) => (B = f(Y,X))
   # "test should fail".

p(f(X,Y), f(X,Y)).

:- export(q/2).
:- test q(A,B) : (A = f(X,Y)) => (B = f(Y,X))
   # "test should pass".

q(f(X,Y), f(Y,X)).

