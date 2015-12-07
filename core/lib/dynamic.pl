:- module(dynamic, [
        asserta/1, asserta/2, assertz/1, assertz/2, assert/1, assert/2,
        retract/1, retractall/1, abolish/1,
        clause/2, clause/3, current_predicate/1, current_predicate/2,
        dynamic/1, data/1, wellformed_body/3
        ],[assertions,isomodes,regtypes]).

:- use_module(engine(internals)).
:- use_module(library(prolog_sys), [new_atom/1]).

:- meta_predicate asserta(clause).
:- meta_predicate asserta(clause, ?).
:- meta_predicate assertz(clause).
:- meta_predicate assertz(clause, ?).
:- meta_predicate assert(clause).
:- meta_predicate assert(clause, ?).
:- meta_predicate retract(clause).
:- meta_predicate retractall(fact).
:- meta_predicate abolish(spec).
:- meta_predicate dynamic(addmodule).
:- meta_predicate data(addmodule).
:- meta_predicate clause(fact,?).
:- meta_predicate clause(fact,?,?).
:- meta_predicate current_predicate(addmodule).

:- doc(title,"Dynamic predicates").

:- doc(author, "The CLIP Group").

:- doc(module,"This module implements the assert/retract family of
   predicates to manipulate dynamic predicates.

   The predicates defined in this module allow modification of the
   program as it is actually running.  Clauses can be added to the
   program (@em{asserted}) or removed from the program (@em{retracted}).
   For these predicates, the argument which corresponds to the clause
   head must be instantiated to an atom or a compound term. The argument
   corresponding to the clause must be instantiated either to a term
   @tt{Head :- Body} or, if the body part is empty, to @tt{Head}. An
   empty body part is represented as @tt{true}.  Note that using this
   library is very detrimental to global analysis, and that for most
   uses the predicates listed in @ref{Fast/concurrent update of facts}
   suffice.").

:- doc(doinclude, dynamic/1).

:- true decl dynamic(Predicates) : sequence_or_list(predname) + iso
        # "Defines each predicate in @var{Predicates} as a
          @concept{dynamic predicate}.  If a predicate is defined
          dynamic in a file, it must be defined dynamic in every file
          containing clauses for that predicate. The directive should
          precede all clauses of the affected predicates.  This
          directive is defined as a prefix operator in the compiler.".


:- pred asserta(Clause): cl + (iso, native)
# "The current instance of @var{Clause} is interpreted as a clause and is
    added to the current program.  The predicate concerned must be dynamic.
    The new clause becomes the @em{first} clause for the predicate concerned.
    Any uninstantiated variables in @var{Clause} will be replaced by new
    private variables.".
% %% along with copies of any subgoals blocked on these variables.

asserta(Clause) :-
        dynamic_clauses(Clause, Root, Ptr0, asserta/1),
	'$inserta'(Root, Ptr0).



:- pred asserta(+Clause,-Ref): cl(Clause) + native
# "Like @tt{asserta/1}. @var{Ref} is a unique identifier of the asserted
   clause.".

asserta(Clause, Ref) :-
        dynamic_clauses(Clause, Root, Ptr0, asserta/2),
	'$inserta'(Root, Ptr0),
	'$ptr_ref'(Ptr0, Ref).

:- pred assertz(+Clause):cl + (iso, native)
# "Like @tt{asserta/1}, except that the new clause becomes the @em{last}
   clause for the predicate concerned.".


assertz(Clause) :-
        dynamic_clauses(Clause, Root, Ptr0, assertz/1),
	'$insertz'(Root, Ptr0).


:- pred assertz(+Clause,-Ref): cl(Clause) + native
# "Like @tt{assertz/1}. @var{Ref} is a unique identifier of the asserted
   clause.".

assertz(Clause, Ref) :-
        dynamic_clauses(Clause, Root, Ptr0, assertz/2),
	'$insertz'(Root, Ptr0),
	'$ptr_ref'(Ptr0, Ref).


:- pred assert(+Clause):cl + native
# "Identical to @tt{assertz/1}. Included for compatibility.".


assert(Clause) :-
        dynamic_clauses(Clause, Root, Ptr0, assert/1),
	'$insertz'(Root, Ptr0).

:- pred assert(+Clause,-Ref): cl(Clause) + native
# "Identical to @tt{assertz/2}. Included for compatibility.".


assert(Clause, Ref) :-
        dynamic_clauses(Clause, Root, Ptr0, assert/2),
	'$insertz'(Root, Ptr0),
	'$ptr_ref'(Ptr0, Ref).

dynamic_clauses(Clause, Root, Ptr0, Goal) :-
        term_to_meta(CL, Clause),
	(   canonical_clause(CL, Head, Body0),
	    wellformed_body(Body0, +, Body) ->
	    dynamic1(Head, Goal),
	    '$compile_term'([Head|Body], Ptr0), 
	    '$current_clauses'(Head, Root)
	;  
            throw(error(type_error(clause, CL), Goal-1))
	).

canonical_clause((H :- B), H, B) :- !,
	functor(H, F, _),
	atom(F).
canonical_clause(H, H, 'basiccontrol:true') :-
	functor(H, F, _),
	atom(F).

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
        functor(Head, MF, A), !,
        asserta_fact('$imports'(Mod, Mod, F, A, Mod)), % defines/3 in not dynamic
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
            throw(error(permission_error(modify, static_procedure, N/A), Goal))
	).
dynamic1(F, _) :-
	functor(F, Name, Ar),
	'$define_predicate'(Name/Ar, consult),
	'$set_property'(F, (dynamic)).		% xref indexing.c


 %% Now in library(concurrency)
 %% :- pred concurrent(+Spec).
 %% 
 %% :- doc(concurrent(F/A), "The predicate named @var{F} with arity
 %% @var{A} is made @concept{concurrent} in the current module at runtime
 %% (useful for predicate names generated on-the-fly).  If the predicate
 %% functor name @var{F} is uninstatiated, a new, unique, predicate name
 %% is generated at runtime. ").
 %% 
 %% 
 %% concurrent(F/A, Mod) :-
 %%         atom(F), !,
 %%         module_concat(Mod, F, MF),
 %%         functor(Head, MF, A), !,
 %%         asserta_fact(imports(Mod, Mod, F, A, Mod)), % defines/3 in not dynamic
 %% 	concurrent1(Head, data/2).
 %% concurrent(F/A, Mod) :-
 %%         var(F), !,
 %%         new_atom(F),
 %%         concurrent(F/A, Mod).
 %% 
 %% 
 %% concurrent1(F, Goal) :-
 %% 	'$predicate_property'(F, _, Prop), !,
 %% 	(   Prop/\2 =:= 2 -> true		% dynamic, xref nondet.c
 %%         ;   functor(F, N, A),
 %%             throw(error(permission_error(modify, static_procedure, N/A), Goal))
 %% 	).
 %% concurrent1(F, _) :-
 %% 	functor(F, Name, Ar),
 %% 	'$define_predicate'(Name/Ar, consult),
 %% 	'$set_property'(F, (concurrent)).		% xref indexing.c


:- pred retract(+Clause): cl + (iso, native)
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

retract(Clause) :-
        term_to_meta(CL, Clause),
	canonical_clause(CL, Head, Body), 
	dynamic1(Head, retract/1),
	'$current_clauses'(Head, Root), 
        '$current_instance'(Head, Body, Root, Ptr, no_block),
	'$erase'(Ptr),
        '$unlock_predicate'(Root).


:- pred retractall(+Head): callable + native
# "Erase all clauses whose head matches @var{Head}, where @var{Head} must
   be instantiated to an atom or a compound term.  The predicate concerned
   must be dynamic.  The predicate definition is retained.".

retractall(Head) :-
        term_to_meta(H, Head),
	nonvar(H),
        retractall_(H).

retractall_(Head) :-
	dynamic1(Head, retractall/1),
	'$current_clauses'(Head, Root), 
        '$current_instance'(Head, _, Root, Ptr, no_block),
	'$erase'(Ptr),
        '$unlock_predicate'(Root),
	fail.
retractall_(_).


:- pred abolish(+Spec): predname + (iso, native)
# "Erase all clauses of the predicate specified by the predicate spec
   @var{Spec}. The predicate definition itself is also erased (the
   predicate is deemed undefined after execution of the abolish). The
   predicates concerned must all be user defined.".

abolish(Spec) :-
        term_to_meta(F/A, Spec),
        functor(Head, F, A), !,
	abolish_data_of(Head),
	'$abolish'(Head).


:- trust pred do_on_abolish(G) : callable(G).
:- multifile do_on_abolish/1.

:- doc(do_on_abolish(Head),"A hook predicate which will be called
	when the definition of the predicate of @var{Head} is abolished.").

abolish_data_of(Head) :-
        do_on_abolish(Head),
        fail.
abolish_data_of(_).


:- pred clause(+Head,?Body): callable(Head) => body(Body) + (iso, native)
# "The clause '@var{Head} @tt{:-} @var{Body}' exists in the current
   program. The predicate concerned must be dynamic.".

clause(HEAD, Body) :-
        term_to_meta(Head, HEAD),
	nonvar(Head),
	dynamic1(Head, clause/2),
	'$current_clauses'(Head, Root),
	'$current_instance'(Head, Body, Root, _, no_block),
        '$unlock_predicate'(Root).

:- doc(clause(Head,Body,Ref),"Like @tt{clause(Head,Body)}, plus the
   clause is uniquely identified by @var{Ref}.").


:- pred clause(+Head,?Body,?Ref): callable(Head) => (body(Body), nonvar(Ref))
# "@var{Head} must be instantiated to an atom or a compound term.".

:- pred clause(?Head,?Body,+Ref) => (callable(Head), body(Body))
# "@var{Ref} must be instantiated to a valid identifier.".


clause(HEAD, Body, Ref) :-
	'$ptr_ref'(Ptr, Ref), !, 
	'$instance'(Head, Body, Ptr), 
	Head\==0,
        term_to_meta(Head, HEAD).
clause(HEAD, Body, Ref) :-
        term_to_meta(Head, HEAD),
	nonvar(Head), 
	dynamic1(Head, clause/3),
	'$current_clauses'(Head, Root), 
        '$current_instance'(Head, Body, Root, Ptr, no_block),
        '$unlock_predicate'(Root),
	'$ptr_ref'(Ptr, Ref).

% predicate not defined in dynamic.pl

:- pred current_predicate(?Spec) => predname + (iso, native)
         # "A predicate in the current module is named @var{Spec}.".


:- pred current_predicate(?Spec,?Module) => predname * internal_module_id + native
        # "A predicate in @var{Module} is named @var{Spec}. @var{Module}
           never is an engine module.".

current_predicate(F/A,M) :-
        current_module(M),
        module_concat(M,'',MPref),
        '$predicate_property'(P, _, _),
        functor(P, MF, A),
        atom_concat(MPref,F,MF).

:- regtype cl/1. % clause -> in fact reduces to callable

cl(X) :- 
	callable(X).
cl((H :- B)) :- 
	callable(H),
	body(B).


:- regtype body/1.
body(X) :- 
	callable(X).
body((X,Xs)) :-
	callable(X),
	body(Xs).
