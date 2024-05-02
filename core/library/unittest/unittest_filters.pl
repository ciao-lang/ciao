:- module(unittest_filters, [], []).

% This module implements some test filters for testing only a subset
% of the tests in a module. Each test corresponds to a test structure
% test_db(TestId, Module, F, A, Dict, Comment, Body, Loc),
% and a filter is a predicate that succeeds for only some of those
% structures. To add a filter, just implement your own filter
% predicates following the example of the filters already
% available

% Example: run_tests(M, [filter(F/A)], [check]) will only run
% the tests for the predicate F/A.

% TODO: do not use test_db structure directly, use a
% dedicate, cleaner structure for filters and have in this module a
% translation between both

:- multifile test_filter/2.

% some built-in filters
test_filter(no_filter, _).
%
% Predicate filter
test_filter(F/A, test_db(_,_,F,A,_,_,_,_)).
% Module and predicate filter
test_filter(M:F/A, test_db(_,M,F,A,_,_,_,_)).
%
test_filter(location_filter(Src,LB,LE), test_db(_,_,_,_,_,_,_,loc(Src,LB,LE))).
%
test_filter(buffer_point_filter(Src,L), test_db(_,_,_,_,_,_,_,loc(Src,LB,LE))) :-
    LB =< L, L =< LE.
%
% TODO: regular expressions
%
% TODO: tags in test assertion comments
