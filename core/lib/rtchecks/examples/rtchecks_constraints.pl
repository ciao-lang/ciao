:- module(rtchecks_constraints,[test/0, sum/3],
        [
         assertions
        ,regtypes
        ,nativeprops
        ,rtchecks
        ]).

:- use_package(expander).

% TODO: have examples of all acceptable constraints --NS

test :-
        % wrong calls
        mul(-2,-2,4),
        % wrong success
        sum(2,2,_).

:- pred sum(A,B,C) => constraint([A + B >= C]).

sum(X,Y,Z) :- Z is X + Y + 1. % BUG here

% TODO: expand to set of 4 cases ([+,-]*[+,-]=[+,-]) to test disjunctions? --NS
:- pred mul(P,Q,R) : constraint([P >= 0, Q >= 0]) => constraint([R >= 0]).

mul(X,Y,Z) :- Z is X * Y.
