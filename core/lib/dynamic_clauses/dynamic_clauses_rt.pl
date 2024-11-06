:- module(dynamic_clauses_rt, [
    asserta/1, asserta/2, assertz/1, assertz/2, assert/1, assert/2,
    retract/1, retractall/1, abolish/1, clause/2,
    erase/1
],[noprelude,assertions,isomodes,regtypes,datafacts]).

:- doc(title,"Dynamic predicates (source preserving) (runtime)").

% TODO: Merge duplicated code and definitions from dynamic_rt.pl

:- doc(author, "Daniel Cabeza (original)").
:- doc(author, "Jose F. Morales (improved)").
:- doc(author, "The Ciao Development Team").

:- doc(usage, "Do not use this module directly (use the
   @lib{dynamic_clauses} package instead).").

:- doc(module, "This module implements the assert/retract family of
   predicates to manipulate dynamic predicates, preserving the
   original source definitions.").

:- doc(bug, "Package dynamic_clauses cannot be loaded into the shell.").

:- use_module(engine(basiccontrol)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_typing)).
:- use_module(engine(exceptions)).
:- use_module(engine(arithmetic)).
:- use_module(engine(atomic_basic)).

:- use_module(engine(runtime_control), [module_split/3]).
:- use_module(engine(internals)).
:- reexport(library(dynamic/dynamic_rt), [dynamic/1, data/1, wellformed_body/3, current_predicate/1, current_predicate/2]).
:- use_module(library(dynamic/dynamic_rt), [clause/2]).
:- import(dynamic_rt, [dynamic1/2]). % (low-level import)

% ---------------------------------------------------------------------------

:- regtype cl(C) # "@var{C} is a well-formed clause".
cl(X) :- 
    cgoal(X).
cl((H :- B)) :- 
    cgoal(H),
    body(B).

:- regtype body(B) # "@var{B} is a clause body".
body(!).
body((A,B)):-
    body(A),
    body(B).
body((A->B)):-
    body(A),
    body(B).
body((A;B)):-
    body(A),
    body(B).
body((\+ A)):-
    body(A).
body(if(A,B,C)):-
    body(A),
    body(B),
    body(C).
body(_^A) :- 
    body(A).
body(Goal) :-
    cgoal(Goal).

% ---------------------------------------------------------------------------

% Decompose a clause (for assert-like predicates)
decomp_assert(Clause,_Term, Goal,_ClHead,_ClBody,_SrcHead,_SrcBody) :-
    var(Clause), !,
    throw(error(instantiation_error, Goal-1)).
decomp_assert(Clause, Term, Goal, ClHead, ClBody, SrcHead, SrcBody) :-
    term_to_meta(Cl, Clause),
    clause_head_and_body(Cl, 'basiccontrol:true', ClHead, ClBody),
    check_head(ClHead, Goal-1, SrcHead),
    clause_head_and_body(Term, true, _, SrcBody0),
    ( wellformed_body(SrcBody0, +, SrcBody) -> true
    ; throw(error(type_error(clause, Term), Goal-1))
    ),
    dynamic1(ClHead, Goal),
    dynamic1(SrcHead, Goal).

% Decompose a clause (for retract-like predicates)
decomp_retract(Clause,_Term,Goal,_ClHead,_ClBody,_SrcHead,_SrcBody) :-
    var(Clause), !,
    throw(error(instantiation_error, Goal-1)).
decomp_retract(Clause, Term, Goal, ClHead, ClBody, SrcHead, SrcBody) :-
    term_to_meta(Cl, Clause),
    clause_head_and_body(Cl, 'basiccontrol:true', ClHead, ClBody),
    check_head(ClHead, Goal-1, SrcHead),
    clause_head_and_body(Term, true, _, SrcBody).

clause_head_and_body((H :- B),_T, H, B) :- !.
clause_head_and_body(H, True, H, True).

% ---------------------------------------------------------------------------

:- meta_predicate asserta(addterm(clause)).
:- pred asserta(+Clause): cl(Clause) + (iso, native)
# "The current instance of @var{Clause} is interpreted as a clause and is
   added to the current program.  The predicate concerned must be dynamic.
   The new clause becomes the @em{first} clause for the predicate concerned.
   Any uninstantiated variables in @var{Clause} will be replaced by new
   private variables.".
%% along with copies of any subgoals blocked on these variables.

asserta(Clause, Term) :-
    decomp_assert(Clause, Term, asserta/1, ClHead, ClBody, SrcHead, SrcBody),
    asserta_internal(ClHead, ClBody),
    asserta_internal(SrcHead, SrcBody).

:- meta_predicate asserta(addterm(clause), ?).
:- pred asserta(+Clause,-Ref): cl(Clause) + native
# "Like @tt{asserta/1}.  @var{Ref} is a unique identifier of the asserted
   clause.".

asserta(Clause, Term, (RefC, RefI)) :-
    decomp_assert(Clause, Term, asserta/2, ClHead, ClBody, SrcHead, SrcBody),
    asserta_internal_ref(ClHead, ClBody, RefC),
    asserta_internal_ref(SrcHead, SrcBody, RefI).


:- meta_predicate assertz(addterm(clause)).
:- pred assertz(+Clause): cl(Clause) + (iso, native)
# "Like @tt{asserta/1}, except that the new clause becomes the @em{last}
   clause for the predicate concerned.".

assertz(Clause, Term) :-
    decomp_assert(Clause, Term, assertz/1, ClHead, ClBody, SrcHead, SrcBody),
    assertz_internal(ClHead, ClBody),
    assertz_internal(SrcHead, SrcBody).

:- meta_predicate assertz(addterm(clause), ?).
:- pred assertz(+Clause,-Ref): cl(Clause) + native
# "Like @tt{assertz/1}. @var{Ref} is a unique identifier of the asserted
   clause.".

assertz(Clause, Term, (RefC, RefI)) :-
    decomp_assert(Clause, Term, assertz/2, ClHead, ClBody, SrcHead, SrcBody),
    assertz_internal_ref(ClHead, ClBody, RefC),
    assertz_internal_ref(SrcHead, SrcBody, RefI).

:- meta_predicate assert(addterm(clause)).
:- pred assert(+Clause): cl(Clause) + native
# "Identical to @tt{assertz/1}. Included for compatibility.".

assert(Clause, Term) :- 
    decomp_assert(Clause, Term, assert/1, ClHead, ClBody, SrcHead, SrcBody),
    assertz_internal(ClHead, ClBody),
    assertz_internal(SrcHead, SrcBody).

:- meta_predicate assert(addterm(clause), ?).
:- pred assert(+Clause,-Ref): cl(Clause) + native
# "Identical to @tt{assertz/2}. Included for compatibility.".

assert(Clause, Term, (RefC, RefI)) :-
    decomp_assert(Clause, Term, assert/2, ClHead, ClBody, SrcHead, SrcBody),
    assertz_internal_ref(ClHead, ClBody, RefC),
    assertz_internal_ref(SrcHead, SrcBody, RefI).

asserta_internal(Head, Body) :-
    '$compile_term'([Head|Body], Ptr0), 
    '$current_clauses'(Head, Root),
    '$inserta'(Root, Ptr0).

asserta_internal_ref(Head, Body, Ref) :-
    '$compile_term'([Head|Body], Ptr0), 
    '$current_clauses'(Head, Root),
    '$inserta'(Root, Ptr0),
    '$ptr_ref'(Ptr0, Ref).

assertz_internal(Head, Body) :-
    '$compile_term'([Head|Body], Ptr0), 
    '$current_clauses'(Head, Root),
    '$insertz'(Root, Ptr0).

assertz_internal_ref(Head, Body, Ref) :-
    '$compile_term'([Head|Body], Ptr0), 
    '$current_clauses'(Head, Root),
    '$insertz'(Root, Ptr0),
    '$ptr_ref'(Ptr0, Ref).

:- doc(erase(Ref), "Deletes the clause referenced by @var{Ref}, the
   identifier obtained by using @pred{asserta/2} or @pred{assertz/2}.").

erase((RefC, RefI)) :- !,
    datafacts_rt:erase(RefC),
    datafacts_rt:erase(RefI).
erase(Ref) :- datafacts_rt:erase(Ref).


% TODO:[JF] removed check for nonlocal_procedure, recover? It may be
%   useful to (optionally?) prevent bodies being expanded from
%   different modules:
%     throw(error(permission_error(modify, nonlocal_procedure, F/A),Spec))

% TODO:[JF] not reachable due to "atom_expansion_add_goals(V, _, _, _, _, _, _) :- var(V), !, fail." in mexpand, fixme?
% check_head(V, Spec, _) :- var(V), !,
%     throw(error(instantiation_error, Spec)).
check_head(N, Spec, _) :- number(N), !,
    throw(error(type_error(callable, N), Spec)).
check_head(H, Spec, _) :-
    '$predicate_property'(H, _, Prop),
    Prop/\2 =:= 0, !,  % not dynamic, xref rt_exp.c
    functor(H, F, A),
    throw(error(permission_error(modify, static_procedure, F/A), Spec)).
check_head(H, _Spec, SrcHead) :-
    check_head0(H, SrcHead).

check_head0(H, SrcHead) :-
    functor(H, F, A),
    atom_concat(F, '-\3\clause', F2),
    functor(SrcHead, F2, A),
    copy_args(A, H, SrcHead).

% TODO: duplicated
copy_args(0, _,  _) :- !.
copy_args(I, F1, F2) :-
    arg(I, F1, X),
    arg(I, F2, X),
    I1 is I-1,
    copy_args(I1, F1, F2).


:- meta_predicate retract(addterm(retracting_clause)).
:- pred retract(+Clause): cl(Clause) + (iso, native)
# "The first clause in the program that matches @var{Clause} is erased.
   The predicate concerned must be dynamic. 

   The predicate @tt{retract/1}
   may be used in a non-determinate fashion, i.e., it will successively
   retract clauses matching the argument through backtracking. If reactivated
   by backtracking, invocations of the predicate whose clauses are being
   retracted will proceed unaffected by the retracts. This is also true
   for invocations of @tt{clause} for the same predicate. The space occupied
   by a retracted clause will be recovered when instances of the clause are
   no longer in use.".

retract(Clause, Term) :-
    decomp_retract(Clause, Term, retract/1, ClHead, _ClBody, SrcHead, SrcBody),
    % Let us retract first the source version (SrcHead); then assume that we retract the right
    retract_internal(SrcHead, SrcBody),
    ( retract_internal(ClHead, _) -> true % (assume same order as SrcHead)
    ; throw(bug(internal_error, retract/1))
    ).

retract_internal(Head, Body) :-
    '$current_clauses'(Head, Root), 
    '$current_instance'(Head, Body, Root, Ptr, no_block),
    '$erase'(Ptr),
    '$unlock_predicate'(Root).


:- meta_predicate retractall(addterm(fact)).
:- pred retractall(+Head): cgoal + native
# "Erase all clauses whose head matches @var{Head}, where @var{Head} must
   be instantiated to an atom or a compound term.  The predicate concerned
   must be dynamic.  The predicate definition is retained.".

retractall(Head,_Term) :-
    var(Head), !,
    throw(error(instantiation_error, retractall/1-1)).
retractall(Head, _Term) :-
    term_to_meta(H, Head),
    check_head(H, retractall/1-1, SrcHead),
    retractall_(H, SrcHead).

retractall_(Head, SrcHead) :-
    retract_internal(Head, _),
    ( retract_internal(SrcHead, _) -> true
    ; throw(bug(internal_error, retractall/1))
    ),
    fail.
retractall_(_, _).

:- meta_predicate abolish(spec).
:- pred abolish(+Spec): predname + (iso, native)
# "Erase all clauses of the predicate specified by the predicate spec
   @var{Spec}.  The predicate definition itself is also erased (the
   predicate is deemed undefined after execution of the abolish). The
   predicates concerned must all be user defined.".

abolish(Spec) :-
    term_to_meta(F/A, Spec),
    functor(Head, F, A), !,
    abolish_data_of(Head),
    abolish_hooks(Head),
    '$abolish'(Head).

abolish_data_of(ClHead) :-
    check_head0(ClHead, SrcHead),
    ( retract_internal(SrcHead, _), fail ; true ).
    
:- trust pred do_on_abolish(G) : cgoal(G).
:- multifile do_on_abolish/1.

:- doc(do_on_abolish(Head),"A hook predicate which will be called
    when the definition of the predicate of @var{Head} is abolished.").

abolish_hooks(Head) :-
    do_on_abolish(Head),
      fail.
abolish_hooks(_).

% TODO: cgoal/1 in assertion may be wrong? (primitive(fact))
% TODO: fix documentation
:- meta_predicate clause(primitive(fact), ?).
:- pred clause(+Head,?Body): cgoal(Head) => body(Body) + (iso, native)
# "The clause '@var{Head} @tt{:-} @var{Body}' exists in the current
   module. The predicate concerned must be dynamic.".

clause(Head0, _) :- var(Head0), !,
    throw(error(instantiation_error, clause/2)).
clause(Head0, Body) :-
    check_head0(Head0, SrcHead),
    term_to_meta(SrcHead, SrcHead2),
    dynamic_rt:clause(SrcHead2, Body).

