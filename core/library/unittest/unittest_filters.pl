:- module(unittest_filters, [], []).

%! \title Filters for unit tests

%! \module This module implements some test filters for testing only a
%  subset of the tests in a module. Each test corresponds to a test
%  structure `test_db(TestId, Module, F, A, Dict, Comment, Body, Loc)`,
%  and a filter is a predicate that succeeds for only some of those
%  structures.
%
%  To add a filter, just implement your own filter predicates
%  following the example of the filters already available.
%
%  Example usage, run tests for the predicate p/1:
%  ```ciao_inferior
%  ?- run_tests(M, [filter(p/1)], [check, show_results]).
%  ```

:- use_module(library(lists), [append/3]).
:- use_module(library(regexp/regexp_code), [match_posix/2]).
:- use_module(library(assertions/assrt_lib), [assertion_body/7]).

% TODO: do not use test_db structure directly, use a
% dedicate, cleaner structure for filters and have in this module a
% translation between both

:- multifile test_filter/2.

% some built-in filters
test_filter(true, _).
test_filter(fail, _) :- fail.
test_filter(false, _) :- fail.
test_filter((A,B), T) :-
    test_filter(A, T), test_filter(B, T).
test_filter((A;B), T) :-
    ( test_filter(A, T) ; test_filter(B, T) ).

% Predicate filter
test_filter(F/A, test_db(_,_,F,A,_,_,_,_)).
% Module and predicate filter
test_filter(M:F/A, test_db(_,M,F,A,_,_,_,_)).

% Location filters
test_filter(location_filter(Src,LB,LE), test_db(_,_,_,_,_,_,_,loc(Src,LB,LE))).
test_filter(buffer_point_filter(Src,L), test_db(_,_,_,_,_,_,_,loc(Src,LB,LE))) :-
    LB =< L, L =< LE.

% Regular expressions filter
test_filter(regexp(Regexp), test_db(_,_,_,_,_,_,Body,_)) :-
    assertion_body(_,_,_,_,_,Comment,Body),
    match_posix(Regexp, Comment).

% Filter for tags (E.g., "[ISO]") in test assertion comments
test_filter(label(Label), T) :-
    append("\\[",  Label, Regexp0),
    append(Regexp0, "\\].*", Regexp),
    test_filter(regexp(Regexp), T).
