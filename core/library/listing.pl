:- module(listing, [listing/0, listing/1],[assertions]).

:- use_module(engine(internals), 
        ['$current_clauses'/2,
         '$first_instance'/2,
         '$unlock_predicate'/1,
         '$current_instance'/5,
         '$current_predicate'/2]).
:- use_module(library(write), [portray_clause/1]).
:- use_module(library(parse_spec), [parse_functor_spec/5]).

% TODO: load in the toplevel by default?
% TODO: combine with dynamic_clauses package

listing :-
	'$current_predicate'(_, Pred),
	listing1(Pred),
	fail.
listing.

listing(Arg) :-
	parse_functor_spec(Arg, user, X, listing1(X), listing/1-1).

listing1(Pred) :-
	'$current_clauses'(Pred, Root),
	'$first_instance'(Root, _),
	nl,
	'$current_instance'(Head, Body, Root, _, no_block),
        '$unlock_predicate'(Root),   % We do not know what has to be listed
	portray_clause((Head:-Body)).
