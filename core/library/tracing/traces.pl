
:- module(traces,
	[ spy/1,
	  nospy/1,
	  trace/3
	],
	[ assertions
	]).

:- use_module(engine(internals), [module_concat/3]).
:- use_module(library(byrdbox/byrd), [nospy/1, spy/1, trace0/1]).

% ----------------------------------------------------------------------

:- doc(title,"Byrd box tracing model (library(tracing))").
:- doc(author, "Francisco Bueno").
:- doc(module,"
	This module can be used to spy predicates in a program. Spying
        a predicate means here that messages are displayed at the call,
	exit, fail and redo ports of the goals in the bodies of the
	clauses of the predicate spied.

        In order to spy a predicate using this module a program expansion
        has to be performed on the program module defining the predicate.
        This is achieved by the @tt{tracing} package.

        This library is a programming exercise in CIAO expansion packages.
        It does not replace the standard interactive debugger, which
        has more functionality and is better integrated with the emacs
        mode. However, this library can sometimes be useful because it is
        more lightweight and allows tracing compiled code.
").

:- doc(usage,"
	@tt{:- use_package(tracing).}

        in the program module defining the code to be spied; or

        @tt{:- use_module(library(tracing/traces)).}

        to call the predicates exported by this module.
").

:- doc(bug,"Only predicates with a static definition will be spied.").
:- doc(bug,"Does not work properly within meta-calls
	(the calls within it go undetected).").
:- doc(bug,"@tt{spy(M:F/A)} does not complain if predicate
	@tt{F/A} does not belong to module @tt{M}.").
:- doc(bug,"Since declarations in source are expanded as calls to this
	module within @tt{initialization}, it depends on the order in
        which modules are loaded that they have effect or not.").

% ----------------------------------------------------------------------

:- data spying/2.

:- redefining(spy/1).
:- doc(spy(M:F/A),"
	Turns spying of predicate @tt{F/A} in module @tt{M} on.
        Declarations of the form @tt{spy(F/A)} (where @tt{M} is implicitly
        taken as the module where the declaration appears) can also be used,
	as long as the @tt{tracing} package is included.").

spy(M:F/A):-
	byrd:spy(M:F/A),
	module_concat(M,F,Fun),
	( spying(Fun,A), ! ; asserta_fact(spying(Fun,A)) ).

:- redefining(nospy/1).
:- doc(nospy(M:F/A),"
	Turns spying of predicate @tt{F/A} in module @tt{M} off.").

nospy(M:F/A):-
	byrd:nospy(M:F/A),
	module_concat(M,F,Fun),
	retractall_fact(spying(Fun,A)).

:- meta_predicate trace(goal,?,?).
:- doc(trace(Goal,F,A),"
	Displays messages at the ports of @code{Goal}, as long as spying
        is on for predicate @code{F/A} (module-name expanded).").

trace(Goal,F,A):-
	spying(F,A), !,
	trace0(Goal).
trace(Goal,_F,_A):-
 	call(Goal).
