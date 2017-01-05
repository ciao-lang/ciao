:- package(tabling).

:- push_prolog_flag(unused_pred_warnings, no).

:- use_package(assertions).

:- doc(nodoc, assertions).

:- doc(filetype, package).

:- doc(author,"Pablo Chico de Guzm@'{a}n Huerta").
:- doc(author,"The CLIP Group").

:- doc(title,"Tabling Execution").
 %% :- doc(subtitle,"(Using O-CHAT Technique)").

:- doc(module, "This module supports the evaluation of predicates
     using tabled resolution (@em{tabling}). Tabling is an alternative
     execution strategy for logic programs that records calls and
     their answers in order to reuse them in future calls. It improves
     Prolog declarativity and can improve efficiency by avoiding
     repeated computations
     @cite{tamaki.iclp86-short,Warren92-short}. Tabling is guaranteed
     to terminate when Prolog programs have the @em{bounded term-depth
     property}. Some examples of the use of tabling can be found in
     the sub-directory @em{examples} of the tabling library.

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
     tabling with constraints. By using @decl{const\_table\_module/1}
     the user indicates the constraint domain to be combined with
     tabling. This constraint domain must implement the predicates
     @pred{lookup\_attr\_call/5}, @pred{lookup\_attr\_answer/4},
     @pred{reinstall\_gen\_space/2} and @pred{consume\_attr\_answer/2}
     as an API for the tabling engine. An implementation example can be
     found in the @lib{different\_constraints} library.").

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
     the tabling package is imported.").


:- doc(table/1,"It declares a tabled predicate.").
:- new_declaration(table/1).

:- doc(const_table_module/1,"It declares a tabled constraint module.").
:- new_declaration(const_table_module/1).

:- op(1150, fx, [ table ]).
 %% :- op(1150, fx, [ const_table_module ]).

:- load_compilation_module(library(tabling/tabling_tr)).
:- add_sentence_trans(tabling_tr:do_term_expansion/3, 750). % TODO: Probably not right priority

:- doc(doinclude,abolish_all_tables/0).
:- doc(doinclude,tabled_call/1).
:- doc(doinclude,lookup_trie/3).
:- doc(doinclude,execute_call/4).
:- doc(doinclude,consume_answer/4).
:- doc(doinclude,new_answer/0).
:- doc(doinclude,new_attr_answer/3).
:- doc(doinclude,lookup_answer/2).
 %% :- doc(doinclude,'$forward_trail'/2).

:- doc(appendix, "An example of translation of a tabled predicate in
 	order to execute it with SLG resolution is shown below:

@begin{verbatim}
:- use_package(library(tabling)).
:- table path/2.

path(X,Z) :- 
	edge(X,Y), 
	path(Y,Z).

path(X,Z) :-
        edge(X,Z).
 @end{verbatim}

 is translated into:

@begin{verbatim}
path(X,Y) :- 
	tabled_call('path:path_slg'(X,Y)).

path_slg(X,Y) :- 
	edge(X,Z),
	tabled_call('path:path_slg'(Z,Y)),
    new_answer.

path_slg(X,Y) :-
	edge(X,Y),
	new_answer.
@end{verbatim} 

  This translation is adapted in case we are combining tabling with
  constraints:

@begin{verbatim}
:- use_package(library(tabling)).
:- use_package(library(difference_constraints)).
:- use_package(library(difference_constraints/difference_constraints_tab)).
:- table path/2.

path(X,Z) :- 
	edge(X,Y), 
	path(Y,Z).

path(X,Z) :-
        edge(X,Z).
 @end{verbatim}

 is translated into:

@begin{verbatim}
path(X,Y) :- 
	lookup_trie('path:path_slg'(X,Y),Root,SF),
	difference_constraints:lookup_attr_call(Root,SF,PGen,CallSpace,LPrune),
	execute_call('path:path_slg'(X,Y),SF,PGen,LPrune),	      
	consume_answer(SF,PGen,AnsSpace,AttrVars),   
	difference_constraints:reinstall_gen_space(SF,CallSpace),	      
	difference_constraints:consume_attr_answer(AnsSpace,AttrVars).

path_slg(X,Y) :- 
	edge(X,Z),
	lookup_trie('path:path_slg'(Z,Y),Root,SF),
	difference_constraints:lookup_attr_call(Root,SF,PGen,CallSpace,LPrune),
	execute_call('path:path_slg'(Z,Y),SF,PGen,LPrune),	      
	consume_answer(SF,PGen,AnsSpace,AttrVars),   
	difference_constraints:reinstall_gen_space(SF,CallSpace),	      
	difference_constraints:consume_attr_answer(AnsSpace,AttrVars),
	lookup_answer(Node,Attrs),
	difference_constraints:lookup_attr_answer(Node,Attrs,Space,LPruneAns),
	new_attr_answer(Node,Space,LPruneAns).

path_slg(X,Y) :-
	edge(X,Y),
	lookup_answer(Node,Attrs),
	difference_constraints:lookup_attr_answer(Node,Attrs,Space,LPruneAns),
	new_attr_answer(Node,Space,LPruneAns).
@end{verbatim} 

  where some primitives have to be defined by the constraint domain
  library.").

:- use_module(library(tabling/tabling_rt)).

:- pop_prolog_flag(unused_pred_warnings).
