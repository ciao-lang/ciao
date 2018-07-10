:- module(_, [], [assertions, dcg, fsyntax]).

:- doc(title, "Fiber-local data").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module implements support for data predicates
   local to each @concept{fiber}, which we denote as
   @concept{transient} data predicates.").

% ---------------------------------------------------------------------------

:- include(library(fibers/fibers_hooks)).

% ---------------------------------------------------------------------------
% Transient state

:- doc(bug, "Locks may be needed (at least for saving/restoring").
:- doc(bug, "FID is missing in t_data").

:- data t_data/1.

% ---------------------------------------------------------------------------
% Save/restore transient data facts

:- use_module(library(aggregates), [findall/3]).

:- export(t_data_restore/1).
t_data_restore(State) :-
	retractall_fact(t_data(_)),
	( member(Fact, State),
	    assertz_fact(t_data(Fact)),
	    fail
	; true
	).

:- export(t_data_dump/1).
t_data_dump(State) :-
	findall(Fact, t_data(Fact), State).

% ---------------------------------------------------------------------------

:- doc(bug, "Allow nondet; add undefined state so that I can use defaults").
:- doc(bug, "Allow commit/transactions?").

% :- meta_predicate t_defined_data(transient).
:- export('$t_defined_data'/1).
'$t_defined_data'(Fact) :-
	( (Fact as transient).decl -> true ; fail ).

% :- meta_predicate t_current_fact(transient).
:- export('$t_current_fact'/1).
% Get value of @var{Fact} in @var{State}
'$t_current_fact'(Fact) :- !,
	fresh_fact(Fact, Fact0),
	( current_fact(t_data(Fact0)) ->
	    Fact = Fact0
	; % Not found, fail
	  fail
	).

% :- meta_predicate t_set_fact(transient).
:- export('$t_set_fact'/1).
% Set new value for @var{Fact}
'$t_set_fact'(Fact) :-
 	fresh_fact(Fact, Fact0),
 	( retract_fact(t_data(Fact0)) -> true ; true ),
	assertz_fact(t_data(Fact)).

% Forget args of @var{Fact} in @var{Fact2}
fresh_fact(Fact, Fact2) :-
	functor(Fact, F, N),
	functor(Fact2, F, N).

% ---------------------------------------------------------------------------

:- export(apply1/3).
apply1(N, X) := Y :-
	functor(Y, N, 1),
	arg(1, Y, X).

