:- module(idx_test, [], [hiord, indexer]).

% Two indicies for foo/4
:- export(foo/4).
:- index foo(+,?,*,i), foo(?,?,?,i).

foo(a,b,c(d),9).
foo(e,f,g(h),11) :- foo(a,b,c(d),9).
foo(_,z,_,_) :- baz.

baz.

% Default system indexing: 1st argument, first level
% (this should add no overhead at all)
:- export(idx1/4).
:- index idx1(+,?,?,?).

idx1(a,b,c(d),9).
idx1(e,f,g(h),11) :- idx1(a,b,c(d),9).
idx1(_,z,_,_) :- baz.

% Default system indexing: 1st argument, first level
% (this should add minor/no overhead)
:- export(idx2/4).
:- index idx2(?,+,?,?).

idx2(a,b,c(d),9).
idx2(e,f,g(h),11) :- idx2(a,b,c(d),9).
idx2(_,z,_,_) :- baz.

% ---------------------------------------------------------------------------
% Indexing meta_predicates

:- export(mymaplist/2).
:- index mymaplist(?, +). % TODO: This must appear before meta_predicate
:- meta_predicate mymaplist(pred(1), ?).
mymaplist(_,    []).
mymaplist(Goal, [Elem|Tail]) :-
	call(Goal, Elem),
	mymaplist(Goal, Tail).

%% The code above should be equivalent to thid one:
%
%:- export(maplist/2).
%:- meta_predicate maplist(pred(1), ?).
%maplist(Goal, List) :-
%	maplist_(List, Goal).
%
%:- meta_predicate maplist_(?, pred(1)).
%maplist_([], _).
%maplist_([Elem|Tail], Goal) :-
%	call(Goal, Elem),
%	maplist_(Tail, Goal).
