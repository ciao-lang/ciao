
:- module(byrd,
	[ flipflop/0,
	  spy/1,
	  nospy/1,
	  trace/1,
	  trace0/1
	],
	[ assertions
	]).

:- use_module(engine(internals), [module_concat/3, term_to_meta/2]).
:- use_module(library(messages), [simple_message/2]).

% ----------------------------------------------------------------------

:- doc(title,"Byrd box tracing model").
:- doc(author, "Francisco Bueno").
:- doc(module,"
	This module can be used to spy predicates in a program. Spying
	means here to display messages at the call, exit, fail and redo
	ports of goals for the predicate spied.

        In order to spy a predicate using this module a program expansion
        has to be performed on the program module defining the predicate.
        This is achieved by the @tt{byrdbox} package.

        This library is a programming exercise in CIAO expansion packages.
        It is meanly intended as an auxiliary library for
        @tt{library(tracing)}.
").

:- doc(usage,"
	@tt{:- use_package(byrdbox).}

        in the program module defining the code to be spied; or

        @tt{:- use_module(library(byrdbox/byrd)).}

        to call the predicates exported by this module.
").

:- doc(bug,"Only predicates with a static definition will be spied.").
:- doc(bug,"Does not work properly with dynamic predicates
	(since a new clause is added for them which may cause
         change in the program behaviour).").
:- doc(bug,"Does not work properly with multifile/dynamic predicates
	which have no static clauses defined (they go undetected).").
:- doc(bug,"@tt{spy(M:F/A)} does not complain if predicate
	@tt{F/A} does not belong to module @tt{M}.").

% ----------------------------------------------------------------------

:- data spying/1.

/* Only exported predicates!!
:- meta_predicate spy(spec), nospy(spec).

spy(Spec):-
	term_to_meta(F/A, Spec),
	functor(Pred,F,A),
	( spying(Pred), ! ; asserta_fact(spying(Pred)) ).

nospy(Spec):-
	term_to_meta(F/A, Spec),
	functor(Pred,F,A),
	retractall_fact(spying(Pred)).
*/

:- doc(spy(M:F/A),"
	Turns spying of predicate @tt{F/A} in module @tt{M} on.
        Declarations of the form @tt{spy(F/A)} (where @tt{M} is implicitly
        taken as the module where the declaration appears) can also be used,
	as long as the @tt{byrdbox} package is included.").

spy(M:F/A):-
	module_concat(M,F,Fun),
	functor(Pred,Fun,A),
	( spying(Pred), ! ; asserta_fact(spying(Pred)) ).

:- doc(nospy(M:F/A),"
	Turns spying of predicate @tt{F/A} in module @tt{M} off.").

nospy(M:F/A):-
	module_concat(M,F,Fun),
	functor(Pred,Fun,A),
	retractall_fact(spying(Pred)).

:- data tracing/0.
:- data notracing/0.

tracing.

:- doc(hide,flipflop/0).

flipflop:-
	retract_fact(tracing),
	asserta_fact(notracing).
flipflop:-
	retract_fact(notracing),
	asserta_fact(tracing),
	fail.

:- meta_predicate trace(goal).
:- doc(trace(Goal),"
	Displays messages at the ports of @code{Goal}, as long as spying
        is on for its predicate.").

trace(Goal):-
        term_to_meta(G,Goal),
	spying(G), !,
	trace0(Goal).
trace(Goal):-
 	call(Goal).

:- meta_predicate trace0(goal).
:- doc(trace0(Goal),"Displays messages at the ports of @code{Goal}.").

trace0(Goal):-
	term_to_meta(G,Goal),
	( simple_message("CALL: ~q",[G])
	; simple_message("FAIL: ~q",[G]),
	  fail
	),
	call(Goal),
	( simple_message("EXIT: ~q",[G])
	; simple_message("REDO: ~q",[G]),
	  fail
	).
