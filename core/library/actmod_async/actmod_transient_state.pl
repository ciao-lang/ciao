:- module(_, [], [assertions, dcg, fsyntax]).

:- doc(title, "Transient state").
:- doc(author, "Jose F. Morales").

:- doc(module, "Transient data is similar to @concept{module
   instances}, where instances are (implicitly) created automatically,
   one per each different initiated query (and continued during
   distributed execution).").

% ---------------------------------------------------------------------------

:- include(library(actmod_async/actmod_async_hooks)).

% ---------------------------------------------------------------------------
% Transient state

% TODO: needs locks, not thread safe
% TODO: we may need multiple instances
:- data t_data/1.
:- export(curr_topmod/1).
:- data curr_topmod/1.

% ---------------------------------------------------------------------------
% Save/restore transient data facts

:- use_module(library(aggregates), [findall/3]).

:- export(t_data_restore/2).
t_data_restore(TopMod, State) :-
	retractall_fact(t_data(_)),
	retractall_fact(curr_topmod(_)),
	assertz_fact(curr_topmod(TopMod)),
	( member(Fact, State),
	    assertz_fact(t_data(Fact)),
	    fail
	; true
	).

:- export(t_data_dump/1).
t_data_dump(State) :-
	findall(Fact, t_data(Fact), State).

% ---------------------------------------------------------------------------

% TODO: allow nondet? (add 'undefined' to state so that I can use the defaults)

:- export(t_defined_data/1).
t_defined_data(Fact) :-
	( 'transient.decl'(Fact) -> true ; fail ).

:- export(t_current_fact/1).
% Get value of @var{Fact} in @var{State}
t_current_fact(Fact) :- !,
	fresh_fact(Fact, Fact0),
	( current_fact(t_data(Fact0)) ->
	    Fact = Fact0
	; % Not found, fail
	  fail
	).

:- export(t_set_fact/1).
% Set new value for @var{Fact}
t_set_fact(Fact) :-
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

