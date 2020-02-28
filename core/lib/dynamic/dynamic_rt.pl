:- module(dynamic_rt, [
    asserta/1, asserta/2, assertz/1, assertz/2, assert/1, assert/2,
    retract/1, retractall/1, abolish/1,
    clause/2, clause/3,
    %current_predicate/1, current_predicate/2, dynamic/1, data/1, %(see below)
    wellformed_body/3
], [noprelude,assertions,isomodes,regtypes,datafacts]).

:- doc(title,"Dynamic predicates (not source preserving) (runtime)").
:- doc(author, "The Ciao Development Team").

:- doc(usage, "Do not use this module directly (use the @lib{dynamic}
   package instead).").

:- doc(module,"This module implements the assert/retract family of
   predicates to manipulate dynamic predicates. This module does not
   preserve the original source definition of dynamic predicates.").

:- use_module(engine(basiccontrol)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_typing)).
:- use_module(engine(term_compare)).
:- use_module(engine(exceptions)).
:- use_module(engine(arithmetic)).
:- use_module(engine(atomic_basic)).
:- use_module(engine(system_info)).
:- use_module(engine(internals)).

% ---------------------------------------------------------------------------

:- if(defined(optim_comp)).
% TODO: optim_comp does not support documenting a predicate defined in other modules
:- else.
:- doc(doinclude, dynamic/1).
:- decl dynamic(Predicates) : sequence_or_list(predname) + iso
   # "Defines each predicate in @var{Predicates} as a @concept{dynamic
   predicate}.  If a predicate is defined dynamic in a file, it must
   be defined dynamic in every file containing clauses for that
   predicate. The directive should precede all clauses of the affected
   predicates.  This directive is defined as a prefix operator in the
   compiler.".
:- endif.

% ---------------------------------------------------------------------------

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

% ---------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- meta_predicate asserta(primitive(clause)).
:- else.
:- meta_predicate asserta(clause).
:- endif.
:- pred asserta(Clause) : cl + (iso, native)
   # "The current instance of @var{Clause} is interpreted as a clause
   and is added to the current program.  The predicate concerned must
   be dynamic.  The new clause becomes the @em{first} clause for the
   predicate concerned.  Any uninstantiated variables in @var{Clause}
   will be replaced by new private variables.".
% %% along with copies of any subgoals blocked on these variables.

:- if(defined(optim_comp)).
asserta(Clause) :-
    dynamic_clauses(Clause, Head, Body, asserta/1),
    '$asserta'(Head, Body).
:- else.
asserta(Clause) :-
    dynamic_clauses(Clause, Root, Ptr0, asserta/1),
    '$inserta'(Root, Ptr0).
:- endif.

:- if(defined(optim_comp)).
:- meta_predicate asserta(primitive(clause), ?).
:- else.
:- meta_predicate asserta(clause, ?).
:- endif.
:- pred asserta(+Clause,-Ref) : cl(Clause) + native
   # "Like @tt{asserta/1}. @var{Ref} is a unique identifier of the
   asserted clause.".

:- if(defined(optim_comp)).
asserta(Clause, Ref) :-
    dynamic_clauses(Clause, Head, Body, asserta/2),
    '$asserta_ref'(Head, Body, Ref).
:- else.
asserta(Clause, Ref) :-
    dynamic_clauses(Clause, Root, Ptr0, asserta/2),
    '$inserta'(Root, Ptr0),
    '$ptr_ref'(Ptr0, Ref).
:- endif.

:- if(defined(optim_comp)).
:- meta_predicate assertz(primitive(clause)).
:- else.
:- meta_predicate assertz(clause).
:- endif.
:- pred assertz(+Clause) : cl + (iso, native)
   # "Like @tt{asserta/1}, except that the new clause becomes the
   @em{last} clause for the predicate concerned.".

:- if(defined(optim_comp)).
assertz(Clause) :-
    dynamic_clauses(Clause, Head, Body, assertz/1),
    '$assertz'(Head, Body).
:- else.
assertz(Clause) :-
    dynamic_clauses(Clause, Root, Ptr0, assertz/1),
    '$insertz'(Root, Ptr0).
:- endif.

:- if(defined(optim_comp)).
:- meta_predicate assertz(primitive(clause), ?).
:- else.
:- meta_predicate assertz(clause, ?).
:- endif.
:- pred assertz(+Clause,-Ref) : cl(Clause) + native
   # "Like @tt{assertz/1}. @var{Ref} is a unique identifier of the
   asserted clause.".

:- if(defined(optim_comp)).
assertz(Clause, Ref) :-
    dynamic_clauses(Clause, Head, Body, assertz/2),
    '$assertz_ref'(Head, Body, Ref).
:- else.
assertz(Clause, Ref) :-
    dynamic_clauses(Clause, Root, Ptr0, assertz/2),
    '$insertz'(Root, Ptr0),
    '$ptr_ref'(Ptr0, Ref).
:- endif.

:- if(defined(optim_comp)).
:- meta_predicate assert(primitive(clause)).
:- else.
:- meta_predicate assert(clause).
:- endif.
:- pred assert(+Clause) : cl + native
   # "Identical to @tt{assertz/1}. Included for compatibility.".

:- if(defined(optim_comp)).
assert(Clause) :-
    dynamic_clauses(Clause, Head, Body, assert/1),
    '$assertz'(Head, Body).
:- else.
assert(Clause) :-
    dynamic_clauses(Clause, Root, Ptr0, assert/1),
    '$insertz'(Root, Ptr0).
:- endif.

:- if(defined(optim_comp)).
:- meta_predicate assert(primitive(clause), ?).
:- else.
:- meta_predicate assert(clause, ?).
:- endif.
:- pred assert(+Clause,-Ref) : cl(Clause) + native
   # "Identical to @tt{assertz/2}. Included for compatibility.".

:- if(defined(optim_comp)).
assert(Clause, Ref) :-
    dynamic_clauses(Clause, Head, Body, assert/2),
    '$assertz_ref'(Head, Body, Ref).
:- else.
assert(Clause, Ref) :-
    dynamic_clauses(Clause, Root, Ptr0, assert/2),
    '$insertz'(Root, Ptr0),
    '$ptr_ref'(Ptr0, Ref).
:- endif.

:- if(defined(optim_comp)).
dynamic_clauses(CL, Head, Body, Goal) :-
    ( canonical_clause(CL, Head, Body0),
      wellformed_body(Body0, +, Body) ->
        '$check_dynamic'(Head, Goal)
    ; throw(error(type_error(clause, CL), Goal-1))
    ).
:- else.
dynamic_clauses(Clause, Root, Ptr0, Goal) :-
    term_to_meta(CL, Clause),
    ( canonical_clause(CL, Head, Body0),
      wellformed_body(Body0, +, Body) ->
        dynamic1(Head, Goal),
        '$compile_term'([Head|Body], Ptr0), 
        '$current_clauses'(Head, Root)
    ; throw(error(type_error(clause, CL), Goal-1))
    ).
:- endif.

canonical_clause((H :- B), H, B) :- !,
    functor(H, F, _),
    atom(F).
canonical_clause(H, H, 'basiccontrol:true') :-
    functor(H, F, _),
    atom(F).

:- doc(wellformed_body(BodyIn,Env,BodyOut), "@var{BodyIn} is a
   well-formed clause body. @var{BodyOut} is its counterpart with no
   single-variable meta-goals (i.e., with @tt{call(X)} for
   @tt{X}). @tt{Env} denotes if global cuts are admissible in
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

:- if(defined(optim_comp)).
:- meta_predicate retract(primitive(clause)).
:- else.
:- meta_predicate retract(clause).
:- endif.
:- pred retract(+Clause) : cl + (iso, native)
   # "The first clause in the program that matches @var{Clause} is
   erased. The predicate concerned must be dynamic.

   The predicate @tt{retract/1}
   may be used in a non-determinate fashion, i.e., it will successively
   retract clauses matching the argument through backtracking. If reactivated
   by backtracking, invocations of the predicate whose clauses are being
   retracted will proceed unaffected by the retracts. This is also true
   for invocations of @tt{clause} for the same predicate. The space occupied
   by a retracted clause will be recovered when instances of the clause are
   no longer in use.".

:- if(defined(optim_comp)).
retract(CL) :-
    canonical_clause(CL, Head, Body), 
    '$check_dynamic'(Head, retract/1),
    '$erase_nb'(Head, Body).
:- else.
retract(Clause) :-
    term_to_meta(CL, Clause),
    canonical_clause(CL, Head, Body), 
    dynamic1(Head, retract/1),
    '$current_clauses'(Head, Root), 
    '$current_instance'(Head, Body, Root, Ptr, no_block),
    '$erase'(Ptr),
    '$unlock_predicate'(Root).
:- endif.

:- if(defined(optim_comp)).
:- meta_predicate retractall(primitive(fact)).
:- else.
:- meta_predicate retractall(fact).
:- endif.
:- pred retractall(+Head) : callable + native
   # "Erase all clauses whose head matches @var{Head}, where
   @var{Head} must be instantiated to an atom or a compound term.  The
   predicate concerned must be dynamic.  The predicate definition is
   retained.".

:- if(defined(optim_comp)).
retractall(H) :-
    nonvar(H), % TODO: is that possible with meta_predicate decl?
    retractall_(H).

retractall_(Head) :-
    '$check_dynamic'(Head, retractall/1),
    '$erase_nb'(Head, _Body),
    fail.
retractall_(_).
:- else.
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
:- endif.

:- if(defined(optim_comp)).
:- meta_predicate abolish(primitive(spec)).
:- else.
:- meta_predicate abolish(spec).
:- endif.
:- pred abolish(+Spec) : predname + (iso, native)
   # "Erase all clauses of the predicate specified by the predicate
   spec @var{Spec}. The predicate definition itself is also erased
   (the predicate is deemed undefined after execution of the
   abolish). The predicates concerned must all be user defined.".

:- if(defined(optim_comp)).
abolish(F/A) :-
    functor(Head, F, A), !,
    abolish_data_of(Head),
    '$abolish'(Head).
:- else.
abolish(Spec) :-
    term_to_meta(F/A, Spec),
    functor(Head, F, A), !,
    abolish_data_of(Head),
    '$abolish'(Head).
:- endif.

:- pred do_on_abolish(G) : callable(G).
:- multifile do_on_abolish/1.

:- doc(do_on_abolish(Head),"A hook predicate which will be called when
   the definition of the predicate of @var{Head} is abolished.").

abolish_data_of(Head) :-
    do_on_abolish(Head),
    fail.
abolish_data_of(_).

:- if(defined(optim_comp)).
:- meta_predicate clause(primitive(fact),?).
:- else.
:- meta_predicate clause(fact,?).
:- endif.
:- pred clause(+Head,?Body) : callable(Head) => body(Body) + (iso, native)
   # "The clause '@var{Head} @tt{:-} @var{Body}' exists in the current
   program. The predicate concerned must be dynamic.".

:- if(defined(optim_comp)).
clause(Head, Body) :-
    nonvar(Head),
    '$check_dynamic'(Head, clause/2),
    '$current_nb'(Head, Body).
:- else.
clause(HEAD, Body) :-
    term_to_meta(Head, HEAD),
    nonvar(Head),
    dynamic1(Head, clause/2),
    '$current_clauses'(Head, Root),
    '$current_instance'(Head, Body, Root, _, no_block),
    '$unlock_predicate'(Root).
:- endif.

:- if(defined(optim_comp)).
:- meta_predicate clause(primitive(fact),?,?).
:- else.
:- meta_predicate clause(fact,?,?).
:- endif.
:- doc(clause(Head,Body,Ref),"Like @tt{clause(Head,Body)}, plus the
   clause is uniquely identified by @var{Ref}.").
:- pred clause(+Head,?Body,?Ref) : callable(Head) => (body(Body), nonvar(Ref))
   # "@var{Head} must be instantiated to an atom or a compound term.".
:- pred clause(?Head,?Body,+Ref) => (callable(Head), body(Body))
   # "@var{Ref} must be instantiated to a valid identifier.".

:- if(defined(optim_comp)).
clause(Head, Body, Ref) :-
    % TODO: fix! head cannot be a variable because of the meta expansion... should meta expansions be moded?
    '$check_dynamic'(Head, clause/3),
    '$current_nb_ref'(Head, Body, Ref).
:- else.
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
:- endif.

% ---------------------------------------------------------------------------

:- if(defined(optim_comp)).
%
:- use_module(engine(rt_exp), ['$check_dynamic'/2]).
:- reexport(engine(rt_exp), [
    current_predicate/1,
    current_predicate/2,
    data/1,
    dynamic/1]).
%
:- else.
%
:- use_module(engine(runtime_control), [current_module/1, new_atom/1]).

:- export(current_predicate/1).
:- export(current_predicate/2).
:- meta_predicate current_predicate(addmodule).
:- pred current_predicate(?Spec) => predname + (iso, native)
   # "A predicate in the current module is named @var{Spec}.".

:- pred current_predicate(?Spec,?Module)
   => predname * internal_module_id + native
   # "A predicate in @var{Module} is named @var{Spec}. @var{Module}
   never is an engine module.".

current_predicate(F/A,M) :-
    current_module(M),
    module_concat(M,'',MPref),
    '$predicate_property'(P, _, _),
    functor(P, MF, A),
    atom_concat(MPref,F,MF).

:- export(dynamic/1).
:- meta_predicate dynamic(addmodule).
:- pred dynamic(+Spec) : predname.
:- doc(dynamic(Spec), "@var{Spec} is of the form @var{F}/@var{A}.  The
   predicate named @var{F} with arity @var{A} is made
   @concept{dynamic} in the current module at runtime (useful for
   predicate names generated on-the-fly).  If the predicate functor
   name @var{F} is uninstatiated, a new, unique, predicate name is
   generated at runtime.").

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

:- doc(doinclude, data/1). % TODO: needed?
:- export(data/1).
:- meta_predicate data(addmodule).
:- pred data(+Spec) : predname.
:- doc(data(Spec), "@var{Spec} is of the form @var{F}/@var{A}.  The
   predicate named @var{F} with arity @var{A} is made @concept{data}
   in the current module at runtime (useful for predicate names
   generated on-the-fly).  If the predicate functor name @var{F} is
   uninstatiated, a new, unique, predicate name is generated at
   runtime. ").

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
    (   Prop/\2 =:= 2 -> true               % dynamic, xref nondet.c
    ;   functor(F, N, A),
        throw(error(permission_error(modify, static_procedure, N/A), Goal))
    ).
dynamic1(F, _) :-
    functor(F, Name, Ar),
    '$define_predicate'(Name/Ar, consult),
    '$set_property'(F, (dynamic)).          % xref indexing.c
%
:- endif.

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
%%     concurrent1(Head, data/2).
%% concurrent(F/A, Mod) :-
%%         var(F), !,
%%         new_atom(F),
%%         concurrent(F/A, Mod).
%% 
%% 
%% concurrent1(F, Goal) :-
%%     '$predicate_property'(F, _, Prop), !,
%%     (   Prop/\2 =:= 2 -> true               % dynamic, xref nondet.c
%%         ;   functor(F, N, A),
%%             throw(error(permission_error(modify, static_procedure, N/A), Goal))
%%     ).
%% concurrent1(F, _) :-
%%     functor(F, Name, Ar),
%%     '$define_predicate'(Name/Ar, consult),
%%     '$set_property'(F, (concurrent)).               % xref indexing.c

