:- module(listing, [listing/0, listing/1], [assertions, isomodes]).

:- doc(title, "Printing dynamic predicates").
:- doc(author, "The Ciao Development Team").

:- doc(module, "This module implements predicates to pretty print the
   contents of the dynamic predicate database to the output
   stream. These predicates are introduced for compatibility with
   legacy C-Prolog, as well as modern Prolog implementations. Note
   that the predicate representation may change due to internal code
   expansions.").

% E.g. https://www2.cs.duke.edu/csl/docs/cprolog.html

:- use_module(engine(internals), 
        ['$current_clauses'/2,
         '$first_instance'/2,
         '$unlock_predicate'/1,
         '$current_instance'/5,
         '$current_predicate'/2]).
:- use_module(engine(io_basic), [nl/0]).
:- use_module(library(write), [portray_clause/1]).
:- use_module(library(parse_spec), [parse_functor_spec/5]).

% TODO: load in the toplevel by default?
% TODO: combine with dynamic_clauses package

:- pred listing # "Show the definition of all the dynamic predicates
   using @pred{portray_clause/1}.".

listing :-
	'$current_predicate'(_, Pred),
	listing1(Pred),
	fail.
listing.

:- pred listing(+Spec) # "Show the definition the specified predicate given
   by @var{Spec} (@tt{F/A}) using @pred{portray_clause/1}.".

listing(Arg) :-
	parse_functor_spec(Arg, user, X, listing1(X), listing/1-1).

listing1(Pred) :-
	'$current_clauses'(Pred, Root),
	'$first_instance'(Root, _),
	nl,
	'$current_instance'(Head, Body, Root, _, no_block),
        '$unlock_predicate'(Root),   % We do not know what has to be listed
	portray_clause((Head:-Body)).
