:- module(dynamic_rt, [
        asserta/1, asserta/2, assertz/1, assertz/2, assert/1, assert/2,
        retract/1, retractall/1, abolish/1, clause/2, mfclause/2,
        current_predicate/1, current_predicate/2,
        dynamic/1, data/1, erase/1, wellformed_body/3
        ],[assertions,isomodes,regtypes]).

:- set_prolog_flag(multi_arity_warnings, off).

:- use_module(engine(internals)).
:- use_module(library(prolog_sys), [new_atom/1]).
:- use_module(library(iso_misc), [sub_atom/5]).

:- meta_predicate asserta(addmodule(addterm(clause))).
:- meta_predicate asserta(addmodule(addterm(clause)), ?).
:- meta_predicate assertz(addmodule(addterm(clause))).
:- meta_predicate assertz(addmodule(addterm(clause)), ?).
:- meta_predicate assert(addmodule(addterm(clause))).
:- meta_predicate assert(addmodule(addterm(clause)), ?).
:- meta_predicate retract(addmodule(addterm(retracting_clause))).
:- meta_predicate retractall(addmodule(addterm(fact))).
:- meta_predicate abolish(spec).
:- meta_predicate dynamic(addmodule).
:- meta_predicate data(addmodule).
:- meta_predicate current_predicate(addmodule).

:- doc(title,"Dynamic predicates").

:- doc(author, "Daniel Cabeza").
:- doc(author, "The CLIP Group").

:- doc(usage, "To be able to handle dynamic predicates in a module,
   load the library package @lib{dynamic_clauses}, either by putting it
   in the package list of the module or using the @decl{use_package/1}
   directive.  Do not load directly the @lib{dynamic_rt} module.").

:- doc(module,"The package @lib{dynamic_clauses} provides the
   assert/retract family of predicates to manipulate dynamic predicates.

   The defined predicates allow modification of the program as it is
   actually running.  Clauses can be added to the program
   (@em{asserted}) or removed from the program (@em{retracted}), as well
   as inspected.  Note that in Ciao only the dynamic predicates of the
   current module (or accessible dynamic multifile predicates) can be
   accessed and modified.  This limits the bad impact to global analysis
   of this dynamic modification of the program.  Thus, if dynamic
   predicates are exported, to be able to inspect or modify them
   externally some accessing predicates need to be implemented and
   exported alongside.

   For the inspecting/manipulating predicates, the argument which
   corresponds to the clause head must be instantiated to an atom or a
   compound term.  The argument corresponding to the clause must be
   instantiated either to a term @tt{Head :- Body} or, if the body part
   is empty, to @tt{Head}. An empty body part is represented as
   @tt{true}.

   Note that using this library is very detrimental to global analysis,
   and that for most uses the predicates listed in
   @ref{Fast/concurrent update of facts} suffice.").

:- doc(doinclude, dynamic/1).

:- true decl dynamic(Predicates) : sequence_or_list(predname) + iso
        # "Defines each predicate in @var{Predicates} as a
          @concept{dynamic predicate}.  If a predicate is defined
          dynamic in a file, it must be defined dynamic in every file
          containing clauses for that predicate. The directive should
          precede all clauses of the affected predicates.  This
          directive is defined as a prefix operator in the compiler.".

:- pred asserta(+Clause): clause + (iso, native)
# "The current instance of @var{Clause} is interpreted as a clause and is
   added to the current program.  The predicate concerned must be dynamic.
   The new clause becomes the @em{first} clause for the predicate concerned.
   Any uninstantiated variables in @var{Clause} will be replaced by new
   private variables.".
%% along with copies of any subgoals blocked on these variables.

asserta(Clause, Term, M) :-
        dynamic_decomposition(Clause, Term, M, asserta/1,
                              ClHead, ClBody, ClData),
        asserta_internal(ClHead, ClBody),
        asserta_internal(ClData, true).

:- pred asserta(+Clause,-Ref): clause(Clause) + native
# "Like @tt{asserta/1}.  @var{Ref} is a unique identifier of the asserted
   clause.".

asserta(Clause, Term, M, (RefC, RefI)) :-
        dynamic_decomposition(Clause, Term, M, asserta/2,
                              ClHead, ClBody, ClData),
        asserta_internal(ClHead, ClBody, RefC),
        asserta_internal(ClData, true, RefI).


:- pred assertz(+Clause): clause + (iso, native)
# "Like @tt{asserta/1}, except that the new clause becomes the @em{last}
   clause for the predicate concerned.".

assertz(Clause, Term, M) :-
        dynamic_decomposition(Clause, Term, M, assertz/1,
                              ClHead, ClBody, ClData),
        assertz_internal(ClHead, ClBody),
        assertz_internal(ClData, true).

:- pred assertz(+Clause,-Ref): clause(Clause) + native
# "Like @tt{assertz/1}. @var{Ref} is a unique identifier of the asserted
   clause.".

assertz(Clause, Term, M, (RefC, RefI)) :-
        dynamic_decomposition(Clause, Term, M, assertz/2,
                              ClHead, ClBody, ClData),
        assertz_internal(ClHead, ClBody, RefC),
        assertz_internal(ClData, true, RefI).

:- pred assert(+Clause): clause + native
# "Identical to @tt{assertz/1}. Included for compatibility.".

assert(Clause, Term, M) :- 
        dynamic_decomposition(Clause, Term, M, assert/1,
                              ClHead, ClBody, ClData),
        assertz_internal(ClHead, ClBody),
        assertz_internal(ClData, true).

:- pred assert(+Clause,-Ref): clause(Clause) + native
# "Identical to @tt{assertz/2}. Included for compatibility.".

assert(Clause, Term, M, (RefC, RefI)) :-
        dynamic_decomposition(Clause, Term, M, assert/2,
                              ClHead, ClBody, ClData),
        assertz_internal(ClHead, ClBody, RefC),
        assertz_internal(ClData, true, RefI).

dynamic_decomposition(Clause,_Term,_M, Goal,_ClHead,_ClBody,_ClData) :-
        var(Clause), !,
        throw(error(instantiation_error, Goal-1)),
        fail.
dynamic_decomposition(Clause, Term, M, Goal, ClHead, ClBody, ClData) :-
        term_to_meta(Cl, Clause),
        clause_head_and_body(Cl, 'basiccontrol:true', ClHead, ClBody),
        check_head(ClHead, M, Goal-1, ClData),
        clause_head_and_body(Term, true, Head, Body0),
        ( wellformed_body(Body0, +, Body) ->
             dynamic1(ClHead, Goal),
             arg(1, ClData, Head),
             arg(2, ClData, Body)
        ; throw(error(type_error(clause, Term), Goal-1)),
          fail
        ).

asserta_internal(Head, Body) :-
        '$compile_term'([Head|Body], Ptr0), 
        '$current_clauses'(Head, Root),
	'$inserta'(Root, Ptr0).

asserta_internal(Head, Body, Ref) :-
        '$compile_term'([Head|Body], Ptr0), 
        '$current_clauses'(Head, Root),
	'$inserta'(Root, Ptr0),
	'$ptr_ref'(Ptr0, Ref).

assertz_internal(Head, Body) :-
        '$compile_term'([Head|Body], Ptr0), 
        '$current_clauses'(Head, Root),
	'$insertz'(Root, Ptr0).

assertz_internal(Head, Body, Ref) :-
        '$compile_term'([Head|Body], Ptr0), 
        '$current_clauses'(Head, Root),
	'$insertz'(Root, Ptr0),
	'$ptr_ref'(Ptr0, Ref).

:- doc(erase(Ref), "Deletes the clause referenced by @var{Ref}, the
   identifier obtained by using @pred{asserta/2} or @pred{assertz/2}.").

erase((RefC, RefI)) :- !,
        data_facts:erase(RefC),
        data_facts:erase(RefI).
erase(Ref) :- data_facts:erase(Ref).


clause_head_and_body((H :- B),_T, H, B) :- !.
clause_head_and_body(H, True, H, True).

% check_head(V, _, Spec, _) :- var(V), !,
%         throw(error(instantiation_error, Spec)).
check_head(N, _, Spec, _) :- number(N), !,
        throw(error(type_error(callable, N), Spec)),
        fail.
check_head(H, _, Spec, _) :-
        '$predicate_property'(H, _, Prop),
        Prop/\2 =:= 0, !,  % not dynamic, xref nondet.c
        functor(H, F, A),
        throw(error(permision_error(modify, static_procedure, F/A), Spec)),
        fail.
check_head(H, M, Spec, ClData) :-
        functor(H, F, A),
        ( atom_concat('multifile:',_, F) ->
            ClData = 'multifile:\3\mfclause'(_,_)
        ; module_name(M, MN),
	  atom_concat(MN, ':', MC),
          atom_concat(MC, _, F) ->
            atom_concat(MC, '\3\clause', ClFun),
            functor(ClData, ClFun, 2)
        ; throw(error(permision_error(modify, nonlocal_procedure, F/A),Spec)),
          fail
        ).

module_name(user(_), user) :- !.
module_name(M, M).

:- doc(wellformed_body(BodyIn,Env,BodyOut),
   "@var{BodyIn} is a well-formed clause body. @var{BodyOut} is its
    counterpart with no single-variable meta-goals (i.e., with @tt{call(X)}
    for @tt{X}). @tt{Env} denotes if global cuts are admissible in
    @tt{BodyIn} (@tt{+} if they are, @tt{-} if they are not)."). 

wellformed_body(B, _, call(B)) :- var(B), !.
wellformed_body(!, Env, !) :- !, Env = + .
wellformed_body((A,B), Env, (A1,B1)) :- !,
        wellformed_body(A, Env, A1),
	wellformed_body(B, Env, B1).
wellformed_body((A->B), Env, (A1->B1)) :- !,
        wellformed_body(A, -, A1),
	wellformed_body(B, Env, B1).
wellformed_body((A;B), Env, (A1;B1)) :- !,
        wellformed_body(A, Env, A1),
	wellformed_body(B, Env, B1).
wellformed_body((\+ A), _, (\+ A1)) :- !,
        wellformed_body(A, -, A1).
wellformed_body(if(A,B,C), Env, if(A1,B1,C1)) :- !,
        wellformed_body(A, -, A1),
        wellformed_body(B, Env, B1),
        wellformed_body(C, Env, C1).
wellformed_body(L^A, Env, L^A1) :- !,
        wellformed_body(A, Env, A1).
wellformed_body(Goal, _, Goal) :-
	functor(Goal, F, _),
	atom(F).

:- doc(dynamic(Spec), "@var{Spec} is of the form @var{F}/@var{A}.
        The predicate named @var{F} with arity @var{A} is made
        @concept{dynamic} in the current module at runtime (useful for
        predicate names generated on-the-fly).  If the predicate functor
        name @var{F} is uninstatiated, a new, unique, predicate name is
        generated at runtime.").

:- pred dynamic(+Spec): predname.

dynamic(F/A, Mod) :-
        atom(F), !,
        module_concat(Mod, F, MF),
        functor(Head, MF, A),
        asserta_fact('$imports'(Mod, Mod, F, A, Mod)), % defines/3 is not dynamic
	dynamic1(Head, dynamic/2).

dynamic(F/A, Mod) :-
        var(F), !,
        new_atom(F),
        dynamic(F/A, Mod).


:- doc(doinclude, data/1).

:- pred data(+Spec): predname.

:- doc(data(Spec), "@var{Spec} is of the form @var{F}/@var{A}.  The
        predicate named @var{F} with arity @var{A} is made
        @concept{data} in the current module at runtime (useful for
        predicate names generated on-the-fly).  If the predicate functor
        name @var{F} is uninstatiated, a new, unique, predicate name is
        generated at runtime. ").

% By now identical to dynamic
data(F/A, Mod) :-
        atom(F), !,
        module_concat(Mod, F, MF),
        functor(Head, MF, A), !,
        asserta_fact('$imports'(Mod, Mod, F, A, Mod)), % defines/3 in not dynamic
	dynamic1(Head, data/2).
data(F/A, Mod) :-
        var(F), !,
        new_atom(F),
        data(F/A, Mod).


dynamic1(F, Goal) :-
	'$predicate_property'(F, _, Prop), !,
	(   Prop/\2 =:= 2 -> true		% dynamic, xref nondet.c
        ;   functor(F, N, A),
            throw(error(permission_error(modify,static_procedure,N/A), Goal)),
            fail
	).
dynamic1(F, _) :-
	functor(F, Name, Ar),
	'$define_predicate'(Name/Ar, consult),
	'$set_property'(F, (dynamic)).		% xref indexing.c

:- pred retract(+Clause): clause + (iso, native)
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


retract(Clause,_Term,_M) :-
        var(Clause), !,
        throw(error(instantiation_error, retract/1-1)),
        fail.
retract(Clause, Term, M) :-
        term_to_meta(Cl, Clause),
	copy_term(Cl, Cl_copy), % To detach Clause from Term
        clause_head_and_body(Cl_copy, 'basiccontrol:true', ClHead, ClBody),
        check_head(ClHead, M, retract/1-1, ClData),
        clause_head_and_body(Term, true, Head, Body),
	arg(1, ClData, Head),
	arg(2, ClData, Body),
	retract_internal(ClHead, ClBody),
	( retract_internal(ClData, _True) -> true
	; error(['Internal error in retract/1, please inform ciao-bug'])
	).

retract_internal(Head, Body) :-
	'$current_clauses'(Head, Root), 
        '$current_instance'(Head, Body, Root, Ptr, no_block),
	'$erase'(Ptr),
        '$unlock_predicate'(Root).


:- pred retractall(+Head): callable + native
# "Erase all clauses whose head matches @var{Head}, where @var{Head} must
   be instantiated to an atom or a compound term.  The predicate concerned
   must be dynamic.  The predicate definition is retained.".

retractall(Head,_Term,_M) :-
        var(Head), !,
        throw(error(instantiation_error, retractall/1-1)),
        fail.
retractall(Head, Term, M) :-
        term_to_meta(H, Head),
	check_head(H, M, retractall/1-1, ClData),
	arg(1, ClData, Term),
	retractall_(H, ClData).

retractall_(Head, ClData) :-
	retract_internal(Head, _),
	( retract_internal(ClData, _True) -> true
	; error(['Internal error in retractall/1, please inform ciao-bug'])
	),
	fail.
retractall_(_, _).

:- pred abolish(+Spec): predname + (iso, native)
# "Erase all clauses of the predicate specified by the predicate spec
   @var{Spec}.  The predicate definition itself is also erased (the
   predicate is deemed undefined after execution of the abolish). The
   predicates concerned must all be user defined.".

abolish(Spec) :-
        term_to_meta(F/A, Spec),
        functor(Head, F, A), !,
	abolish_data_of(F, A),
	abolish_hooks(Head),
	'$abolish'(Head).

abolish_data_of(F, A) :-
        ( atom_concat('multifile:',F_a, F) ->
	    functor(Head, F_a, A),
            ClData = 'multifile:\3\mfclause'(Head,_)
        ; sub_atom(F, Before, 1, _, ':'),
	  ColonPos is Before+1,
	  sub_atom(F, 0, ColonPos, MC),
	  sub_atom(F, ColonPos, _, 0, F_a),
          atom_concat(MC, '\3\clause', ClFun),
	  functor(ClData, ClFun, 2),
	  functor(Head, F_a, A),
	  arg(1, ClData, Head)
        ),
	( retract_internal(ClData, _True), fail ; true ).
	
:- trust pred do_on_abolish(G) : callable(G).
:- multifile do_on_abolish/1.

:- doc(do_on_abolish(Head),"A hook predicate which will be called
	when the definition of the predicate of @var{Head} is abolished.").

abolish_hooks(Head) :-
        do_on_abolish(Head),
          fail.
abolish_hooks(_).


:- pred clause(+Head,?Body): callable(Head) => body(Body) + (iso, native)
# "The clause '@var{Head} @tt{:-} @var{Body}' exists in the current
   module. The predicate concerned must be dynamic.".

:- impl_defined(clause/2).

:- pred mfclause(+Head,?Body): callable(Head) => body(Body) + (iso, native)
# "There is a clause '@var{Head} @tt{:-} @var{Body}' of a dynamic multifile
   predicate accessible from this module.".

:- doc(hide, '\3\mfclause'/2).

:- multifile '\3\mfclause'/2.
:- data '\3\mfclause'/2.

mfclause(H, B) :- '\3\mfclause'(H, B).

:- pred current_predicate(?Spec) => predname + (iso, native)
        # "A predicate in the current module is named @var{Spec}.".

:- pred current_predicate(?Spec,?Module) => predname * internal_module_id
        + native
        # "A predicate in @var{Module} is named @var{Spec}. @var{Module}
           never is an engine module.".

current_predicate(F/A,M) :-
        current_module(M),
        module_concat(M,'',MPref),
        '$predicate_property'(P, _, _),
        functor(P, MF, A),
        atom_concat(MPref,F,MF).

:- regtype clause(C) # "@var{C} is a well-formed clause".

clause(X) :- 
	callable(X).
clause((H :- B)) :- 
	callable(H),
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
	callable(Goal).

:- doc(bug, "Package dynamic_clauses cannot be loaded into the shell.").
