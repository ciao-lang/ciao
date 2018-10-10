:- package(tabling).

% TODO: separate _doc.pl file

:- use_package(assertions).

:- doc(nodoc,assertions).
:- doc(nodoc,assertions_basic).

:- doc(filetype, package).

:- doc(author,"Pablo Chico de Guzm@'{a}n Huerta").
:- doc(author,"The Ciao Development Team").

:- doc(title,"Tabling execution").
 %% :- doc(subtitle,"(Using O-CHAT Technique)").

:- doc(stability,beta).
:- doc(module, "This module supports the evaluation of predicates using
     tabled resolution (@em{tabling}). Tabling is an alternative
     execution strategy for logic programs that records calls and
     their answers in order to reuse them in future calls. It improves
     Prolog declarativity and can improve efficiency by avoiding
     repeated computations
     @cite{tamaki.iclp86-short,Warren92-short}. Tabling is guaranteed
     to terminate when Prolog programs have the @em{bounded term-depth
     property}. Some examples of the use of tabling can be found in
     the sub-directory @em{tabling\_examples} of the tabling library.

     Adding a @decl{table/1} declaration to a predicate makes the
     compiler and run-time system distinguish the first occurrence
     of a tabled goal (the @em{generator}) and subsequent calls which
     are identical up to variable renaming (the @em{consumers}).  The
     generator applies resolution using the program clauses to derive
     answers for the goal.  Consumers @em{suspend} the current
     execution path (using implementation-dependent means) and move to
     a different branch.  When such an alternative branch finally
     succeeds, the answer generated for the initial query is inserted
     in a table associated with the original goal. This makes it
     possible to reactivate suspended calls and to continue execution
     at the point where it was stopped. Thus, consumers do not recompute 
     those calls, but obtain instead the answers from the
     table where they have been previously inserted by the producer.

     Predicates not marked as tabled are executed following standard SLD
     resolution, hopefully with (minimal or no) overhead due to the
     availability of tabling in the system. 

     Our current version of tabling only supports @em{local}
     evaluation and @em{variant tabling}
     @cite{Rao96athread-short}. Local evaluation computes all the
     answers of a tabled predicate before returning any of them. Note
     that a call to a tabled predicate will never return if this call
     has infinite answers. Variant tabling considers two calls/answers
     to be the same only when they are identical under variable
     renaming.

     By declaring a predicate as tabled, a program translation is
     performed to abstract the use of internal tabling primitives for
     the user. Our tabling implementation technique follows the
     @math{\Theta}-CHAT approach @cite{demoen99:chat_complexity} which
     does not require major changes in the compiler or run-time
     system.

     Finally, our tabling implementation supports the combination of
     tabling with constraints. By using the TCLP interface of a
     constraint solver, e.g., @decl{:- use_package(t\_clpq)} the
     tabling engine uses the entailment check provided by the
     constraint solver to detect more particular calls / answers. Some
     examples of the use of TCLP can be found in the sub-directory
     @em{tclp\_examples} of the tabling library.

     The TCLP interface of a constraint solver must implement the
     predicates @pred{call_domain_projection/2},
     @pred{answer_domain_projection/2},
     @pred{call_store_projection/3}, @pred{answer_store_projection/3},
     @pred{call_entail/2}, @pred{answer_check_entail/3} and
     @pred{apply_answer/2} as an API for the tabling engine. Some
     examples of TCLP interfaces can be found in the
     @lib{different\_constraints}, @lib{t\_clpq} and @lib{t\_clpr}
     libraries.").

:- doc(usage, "The @tt{TABLED_EXECUTION} flag must be set to @tt{yes} during
     system configuration in order to compile the engine with support
     for the execution of goals with tabling. It is set to @tt{yes} by
     default.").

:- doc(bug, "This implementation of tabling is a beta version. Tabled
     predicate execution is currently incompatible with several Ciao
     features including stack reallocation, garbage collection, some
     cuts, and parallelism. In order to mitigate the impact of the
     current limitations in the memory management of tabling the size
     of the execution stacks is defined much larger than normal when
     the tabling package is imported. The implementation of TCLP is an
     alpha version").


:- doc(table/1,"It declares a tabled predicate.").

:- new_declaration(table/1).

% :- new_declaration(const_table_module/1).

:- new_declaration(active_tclp/0).
:- new_declaration(table_aggregate/1).
:- new_declaration(table_subsumption/0).

:- op(1150, fx, [ table, table_aggregate, table_subsumption ]).
 %% :- op(1150, fx, [ const_table_module ]).

:- load_compilation_module(library(tabling/tabling_tr)).
:- add_sentence_trans(tabling_tr:do_term_expansion/3, 750). % TODO: Probably not right priority


:- use_module(library(tabling/tabling_rt)).

:- reexport(library(tabling/tabling_rt),
	    [
		print_counters/0,        % debug:    print_counters_c
		tabling_stats/0,
		set_tabling_flag/2,      % debug:    set_tabling_flag_c
		current_tabling_flag/2,  % debug:    current_tabling_flag_c
		abolish_all_tables/0
	    ]).


