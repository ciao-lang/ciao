:- module(conc_aggregates, [findall/3], [assertions, isomodes, nativeprops]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(prolog_sys), [new_atom/1]).

:- doc(title,"All solutions concurrent predicates").
:- doc(author,"Manuel Carro (concurrency-safeness)").

:- doc(module,"This module implements thread-safe aggregation
predicates.  Its use and results should be the same as those in the
@concept{aggregates} library, but several goals can use them
concurrently without the interference and wrong results (due to
implementation reasons) @concept{aggregates} might lead to.  This
particular implementation is completely based on the one used in the
@concept{aggregates} library (whose original authors were
@author{Richard A. O'Keefe} and @author{David H.D. Warren}).
").

 %% 
 %%    When there are many solutions to a problem, and when all those
 %%    solutions are required to be collected together, this can be
 %%    achieved by repeatedly backtracking and gradually building up a
 %%    list of the solutions.  The following built-in predicates are
 %%    provided to automate this process.

:- on_abort(retractall_fact('$$temp_sol_conc_findall'(_,_))).

:- meta_predicate findall(?, :, ?).

% Save solutions

:- concurrent '$$temp_sol_conc_findall'/2.

:- pred setof(@Template, +Goal, ?Set) => (term(Template),
callable(Goal), list(Set)) + iso # "Finds the @var{Set} of instances
of the @var{Template} satisfying the @var{Generator}.  The set is in
ascending order (see @pred{compare/3} for a definition of this order)
without duplicates, and is non-empty.  If there are no solutions,
@pred{setof/3} fails.  @pred{setof/3} may succeed in more than one
way, binding free variables in the @var{Generator} to different
values. This can be avoided by using existential quantifiers on the
free variables in front of the @var{Generator}, using
@pred{^/2}. E.g., in @tt{A^p(A,B)}, @tt{A} is existentially
quantified.  Safe in concurrent apllications.".
:- doc(bug, "Thread-safe @pred{setof/3} is not yet implemented.").
:- export(setof/3).
:- impl_defined(setof/3).

:- pred bagof(@Template, +Generator, ?Bag) => (term(Template),
callable(Goal), list(Set)) + iso # "Finds all the
instances of the @var{Template} produced by the @var{Generator}, and
returns them in the @var{Bag} in the order in which they were found.
If the @var{Generator} contains free variables which are not bound in
the @var{Template}, it assumes that this is like any other Prolog
question and that you want bindings for those variables.  This can be
avoided by using existential quantifiers on the free variables in
front of the @var{Generator}, using @pred{^/2}.  Safe in concurrent
applications.".
:- doc(bug, "Thread-safe @pred{bagof/3} is not yet implemented.").
:- export(bagof/3).
:- impl_defined(bagof/3).

:- pred findall(?Template, +Generator, ?List) => (term(Template),
callable(Goal), list(Set)) + (iso, is_det) # "A special case
of bagof, where all free variables in the @var{Generator} are taken to
be existentially quantified. Safe in concurrent applications.".

findall(Template, Goal, Solutions):-
        new_atom(Id),
        assert_solutions(-Template, Goal, Id),
        recover_solutions(Id, [], Solutions).

assert_solutions(Template, Goal, Id):-
        asserta_fact('$$temp_sol_conc_findall'(Id, '-')),
        call(Goal),
        asserta_fact('$$temp_sol_conc_findall'(Id, Template)),
        fail.
assert_solutions(_Template, _Goal, _Id).

recover_solutions(Id, List, Tail):-
        retract_fact_nb('$$temp_sol_conc_findall'(Id, Term)), !,
        list_all_sols(Term, Id, List, Tail).

list_all_sols('-', _Id, List, List):- !.
list_all_sols(-Term, Id, SoFar, Rest):-
        retract_fact_nb('$$temp_sol_conc_findall'(Id, NewTerm)), !,
        list_all_sols(NewTerm, Id, [Term|SoFar], Rest).
