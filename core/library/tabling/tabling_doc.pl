:- use_package(assertions).
:- doc(nodoc,assertions).
:- doc(nodoc,assertions_basic).

:- doc(title,"Tabling execution").
 %% :- doc(subtitle,"(Using O-CHAT Technique)").

:- doc(filetype, package).
:- doc(stability,beta).

:- doc(author,"Pablo Chico de Guzm@'{a}n Huerta").
:- doc(author,"Joaqu@'{i}n Arias").
:- doc(author,"The Ciao Development Team").

:- doc(module, "This library package allows the evaluation of
   predicates using tabled resolution (@em{tabling}). Tabling is an
   alternative execution strategy for logic programs that records
   calls and their answers in order to reuse them in future calls. It
   improves Prolog declarativity and can improve efficiency by
   avoiding repeated computations
   @cite{tamaki.iclp86-short,Warren92-short}. Tabling is guaranteed to
   terminate when Prolog programs have the @em{bounded term-depth
   property}. Some examples of the use of tabling can be found at the
   end of this section.

   @section{Tabling}

   Adding a @decl{table/1} declaration to a predicate makes the
   compiler and run-time system distinguish the first occurrence of a
   tabled goal (the @em{generator}) and subsequent calls which are
   identical up to variable renaming (the @em{consumers}).  The
   generator applies resolution using the program clauses to derive
   answers for the goal.  Consumers @em{suspend} the current execution
   path (using implementation-dependent means) and move to a different
   branch.  When such an alternative branch finally succeeds, the
   answer generated for the initial query is inserted in a table
   associated with the original goal. This makes it possible to
   reactivate suspended calls and to continue execution at the point
   where it was stopped. Thus, consumers do not recompute those calls,
   but obtain instead the answers from the table where they have been
   previously inserted by the producer.

   Predicates not marked as tabled are executed following standard SLD
   resolution, hopefully with (minimal or no) overhead due to the
   availability of tabling in the system.

   Our current version of tabling only supports @em{local} evaluation
   and @em{variant tabling} @cite{Rao96athread-short}. Local
   evaluation computes all the answers of a tabled predicate before
   returning any of them. Note that a call to a tabled predicate will
   never return if this call has infinite answers. Variant tabling
   considers two calls/answers to be the same only when they are
   identical under variable renaming.

   By declaring a predicate as tabled, a program translation is
   performed to abstract the use of internal tabling primitives for
   the user. Our tabling implementation technique follows the
   @math{\Theta}-CHAT approach @cite{demoen99:chat_complexity} which
   does not require major changes in the compiler or run-time system.

   @section{Tabled Constraint Logic Programming}

   The TCLP implementation allows the combination of tabling with
   constraints. The initial implementation described in
   @cite{chico-tclp-flops2012-large} has been modified by a modular
   implementation, called Mod TCLP, which is described in
   @cite{TCLP-tplp2019}. By using the Mod TCLP interface of a
   constraint solver, e.g., @decl{:- use\_package(t\_clpq)}, the
   tabling engine uses the entailment check provided by the constraint
   solver to detect more particular calls / answers.

   The TCLP interface of a constraint solver must implement the
   following interface:

@begin{verbatim}
call_domain_projection/2
answer_domain_projection/2
call_store_projection/3
answer_store_projection/3
call_entail/2
answer_check_entail/3
apply_answer/2
@end{verbatim}

   as an API for the tablingq engine. Some examples of TCLP interfaces
   are @tt{different\_constraints}, @tt{t\_clpq}, and @tt{t\_clpr}
   libraries.  Some examples of the use of TCLP can be found at the
   end of this section (with some examples of the TCLP interface).

   @section{Some TCLP applications}

   The current implementation of Mod TCLP has been used to develop
   more complex applications:

   @begin{itemize}

   @item The Implementation of an Abstract Interpretation Algorithm
     described in @cite{arias19:ciaopp-tclp}:

     Abstract interpretation requires a fixpoint computation. PLAI is
     a fixpoint algorithm implemented by the abstract interpreter of
     CiaoPP, an analyzer and optimizer suite for logic programs, part
     of the Ciao development environment. In this paper, we adapt the
     existing PLAI implementation in CiaoPP using tabled constraint
     logic programming. The tabling engine is used to compute the
     fixpoint and the constraint engine computes the LUB of the
     abstract substitutions of different clauses. That provides, on
     one hand, much simpler code since the fixpoint computation is
     taken care of by the underlying tabling machinery, and, in most
     cases, performance gains, since some crucial operations (such as
     branch switching and resumption) are executed by the tabling
     engine. Determining that the fixpoint has been reached uses
     semantic equivalence, e.g., whether syntactically different
     representations of an abstract substitution actually refer to the
     same element in the abstract domain. This is delegated to the
     abstract domain operations, transparently to the analyzer. As a
     result, the tabling analyzer can reuse answers in more cases than
     if syntactical equality were used to detect repeated calls, and
     better performance, even taking into account the additional cost
     associated to these checks, is achieved. The implementation
     presented is based on the TCLP framework available in Ciao
     Prolog. It is one-third the size of the initial fixpoint
     implementation we started with, and its performance has been
     evaluated by analysing several programs with different abstract
     domains.

   @item Incremental Evaluation of Lattice-Based Aggregates in Logic
     Programming described in @cite{atclp-padl2019} and available as a
     bundle, @tt{:- use_package(tclp_aggregates)}, in the current
     distribution of Ciao:

     Aggregates are used to compute single pieces of information from
     separate data items, such as records in a database or answers to
     a query to a logic program. The maximum and minimum are
     well-known examples of aggregates. The computation of aggregates
     in Prolog or variant-based tabling can loop even if the aggregate
     at hand can be finitely determined. When answer subsumption or
     mode-directed tabling is used, termination improves, but the
     behavior observed in existing proposals is not consistent. We
     present a framework to incrementally compute aggregates for
     elements in a lattice. We use the entailment and join relations
     of the lattice to define (and compute) aggregates and decide
     whether some atom is compatible with (entails) the aggregate. The
     semantics of the aggregates defined in this way is consistent
     with the LFP semantics of tabling with constraints. Our
     implementation is based on the TCLP framework available in Ciao
     Prolog, and improves its termination properties w.r.t. similar
     approaches. Defining aggregates that do not fit into the lattice
     structure is possible, but some properties guaranteed by the
     lattice may not hold. However, the flexibility provided by this
     possibility justifies its inclusion. We validate our design with
     several examples and we evaluate their performance.

  @end{itemize}
").

:- doc(usage, "The @tt{TABLED_EXECUTION} flag must be set to @tt{yes}
   during system configuration in order to compile the engine with
   support for the execution of goals with tabling. It is set to
   @tt{yes} by default.").

:- doc(appendix, "
   @subsection{Some examples using Tabling} 

   We now illustrate some of the uses of the package of tabling
   through examples.  The following example defines a simple predicte
   @tt{path(X,Y)} which returns the transitive closuer of @tt{edge/2}
   without entering loops:

@begin{verbatim}
:- use_package(tabling).
:- table path/2.

path(X,Y) :- path(X,Z), edge(Z,Y).
path(X,Y) :- edge(X,Y).

edge(a,b).
edge(b,a).
@end{verbatim}

   Other examples can be found in the source and library directories
   and in @cite{pablo-phd}.

   @subsection{Some examples using Tabled Constraint Logic Programming}

   We now illustrate some of the uses of the package of t_clpq through
   examples.  The following example defines a simple predicte
   @tt{fibonacci(N,F)} which returns the fibonacci number @tt{F} given
   the index @tt{N} and returns the index @tt{N} given the fibonacci
   number @tt{F}:

@begin{verbatim}
:- use_package(tabling).
:- use_package(t_clpq).
:- table fibonacci/2.

fibonacci(0, 0).
fibonacci(1, 1).
fibonacci(N, F) :-
    N .>=. 2,
    N1 .=. N - 1,
    N2 .=. N - 2,
    F1 .>=. 0,
    F2 .>=. 0,
    F .=. F1 + F2,
    fibonacci(N1, F1),
    fibonacci(N2, F2).
@end{verbatim}

   Other examples can be found in the source and library directories
   and in @cite{TCLP-tplp2019}.  @subsection{Some examples of TCLP
   interface}

   We now illustrate the implementation of a TCLP interface to link a
   constraint solver wiht the tabling engine through examples.  The
   following example defines the interface of @tt{clpq} wiht the
   tabling engine:

@begin{verbatim}
:- use_package(clpq).
:- use_module(library(clpq/clpq_dump), [clpqr_dump_constraints/3]).
:- active tclp.

call_domain_projection(Vars, st(Vars,_)).
call_entail(st(Vars,_), st(FGen-ProjGen)) :-
       Vars = FGen, clpq_entailed(ProjGen).
call_store_projection(_, st(Vars,_), st(F,Proj)) :-
       clpqr_dump_constraints(Vars, F , Proj).

answer_domain_projection(Vars, st(F,Proj)) :-
       clpqr_dump_constraints(Vars, F, Proj).
answer_check_entail(st(F,_), st(FAns,ProjAns), 1) :-
       F = FAns, clpq_entailed(ProjAns), !.
answer_check_entail(st(F,Proj), st(FAns,ProjAns), -1) :-
       F = FAns, clpq_meta(ProjAns), clpq_entailed(Proj).
answer_store_projection(_, St, St).

apply_answer(Vars, st(FAns,ProjAns)) :-  
       Vars = FAns, clpq_meta(ProjAns).
@end{verbatim}
       
   Other examples can be found in the source and library
   directories and in @cite{TCLP-tplp2019}.
").

:- doc(bug, "This implementation of tabling is a beta version. Tabled
   predicate execution is currently incompatible with several Ciao
   features including stack reallocation, garbage collection, some
   cuts, and parallelism. In order to mitigate the impact of the
   current limitations in the memory management of tabling the size of
   the execution stacks is defined much larger than normal when the
   tabling package is imported.").

:- doc(bug, "The implementation of Mod TCLP is an alpha version that
   requires futher improvements.").

:- doc(table/1,"It declares a tabled predicate.").

:- new_declaration(table/1).

% :- new_declaration(const_table_module/1).
