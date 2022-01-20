:- module('lists.test', _, [assertions, regtypes]).

% NOTE: non exhaustive tests and experimental (may change in the future)
%  - use member/2 and backtracking (similar to test case generation)
%  - shared variables between calls/success
%  - compat fields

:- use_module(library(lists), [nonsingle/1]).
nonsingle(A) :- lists:nonsingle(A).

:- test nonsingle(A) : ( A = [a] ) + fails # "nonsingle fails.".

:- test nonsingle(A) : ( A = [a, b] ) # "nonsingle succeeds.".

:- use_module(library(lists), [append/3]).
append(A,B,C) :- lists:append(A,B,C).

:- test append(A, B, C) :
   ( A = [1,2,3], B = [4, 5] ) => ( C = [1, 2, 3, 4, 5] ) # "Simple call to append".

%:- test append(A, B, X) : (L = [([1, 2], [1, 2, 4, 5, 6]), ([1, 2, 3],
%   [1, 2, 3, 4, 5, 6])], B = [4, 5, 6], member((A,X2), L)) => (X==X2)
%   # "Simple append test".
:- test append(A, B, X) : (A = [1,2], B = [4,5,6]) => (X=[1,2,4,5,6])# "Simple append test".
:- test append(A, B, X) : (A = [1,2,3], B = [4,5,6]) => (X=[1,2,3,4,5,6]) # "Simple append test".

:- test append(A, B, X) : ( X = [1, 2, 3] ) => member((A, B), [([],
   [1, 2, 3]), ([1] , [2, 3]), ([1, 2] , [3]), ([1, 2, 3], [])]) #
   "Test of reverse call".

:- test append(A, B, X) : ( A = [], B = [] ) => ( X == [] ) # "Empty test.".

:- test append(_A, B, X) : ( B = [2], X = [1,2,3] ) + fails # "Test of a call that fails".

:- test append(X, Y, Z) : ( Y = [2], Z = [1,2] ) => ( X == [1] ) # "Test of a reverse call.".

:- export(reverse/2).
:- use_module(library(lists), [reverse/2]).
reverse(A,B) :- lists:reverse(A,B).

:- test reverse(A, B) : ( A = [1, 2, 3] ) => ( B = [3, 2, 1] ) # "Reverse a list".

:- export(reverse/3).
:- use_module(library(lists), [reverse/3]).
reverse(A,B,C) :- lists:reverse(A,B,C).

:- test reverse(A, B, C)
   :: ( var(B) ) : ( A = [1, 2, 3] ) => ( C = [3, 2, 1 | B] ) + true # "reverse/3 test".

