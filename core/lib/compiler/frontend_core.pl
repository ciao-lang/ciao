% (included file)
:- doc(section, "Front-end definitions for the core language").
% TODO: This can be split in smaller pieces

% ---------------------------------------------------------------------------
:- doc(subsection, "Definition of Module Handlers").
% TODO: merge with core_OC

decl__treatDom(module(_,_)).
decl__treat(module(_,_),_Base,_M,_VNs, Ln0, Ln1) :- !,
    compiler_error(Ln0, Ln1, nonstarting(module,2)).

decl__treatDom(module(_,_,_)).
decl__treat(module(_,_,_),_Base,_M,_VNs, Ln0, Ln1) :- !,
    compiler_error(Ln0, Ln1, nonstarting(module,3)).

decl__treatDom(package(_)).
decl__treat(package(_),_Base,_M,_VNs, Ln0, Ln1) :- !,
    compiler_error(Ln0, Ln1, nonstarting_package).

% ---------------------------------------------------------------------------
:- doc(subsection, "Support for compilation modules").

% TODO: Load compilation modules in a separate module so that the
%   scope of runtime module expansions is reduced.

do_load_compilation_module(BFile, _, _) :- % TODO: simplify
    % Already processed
    in_mode(In), processed(BFile, In),
    % and loaded dynamically (fix 'compmod_skip' bug)
    defines_module(BFile, Module),
    this_module(M), dyn_imports(M, Module),
    !.
do_load_compilation_module(_, File, Base) :-
    undo_decls(Base),
    this_module(M),
    use_mod(File, all, M),
    redo_decls(Base).

% Translation hooks without module qualification is ambiguous.
% (there is no explicit connection with load_compilaion_module)
check_qualified_hook(P, DeclF, DeclA, Ln0, Ln1) :-
    ( nonvar(P), P = (_:_) -> true
    ; error_in_lns(Ln0, Ln1, error,
                   ['Refusing to process ',~~(DeclF/DeclA),
                    ', missing module qualification in ', ~~(P)])
    ).

do_add_sentence_trans(M, P, Prior, Base, Ln0, Ln1) :-
    ( add_sentence_trans_and_init(M, P, Prior) ->
        asserta_fact(undo_decl(Base, add_sentence_trans(M, P, Prior),
                                     del_sentence_trans(M)))
    ; warning_failed_decl(Ln0, Ln1, add_sentence_trans(P, Prior))
    ).

do_add_term_trans(M, P, Prior, Base, Ln0, Ln1) :-
    % TODO: if predicate P does not exist, compilation complains a lot of times
    ( add_term_trans(M, P, Prior) ->
        asserta_fact(undo_decl(Base, add_term_trans(M, P, Prior),
                                     del_term_trans(M)))
    ; warning_failed_decl(Ln0, Ln1, add_term_trans(P, Prior))
    ).

% ---------------------------------------------------------------------------
:- doc(subsubsection, "Directives to add translation hooks").

% ---------------------------------------------------------------------------
% load_compilation_module/1 - Load a compilation module

decl__treatDom(load_compilation_module(_)).
decl__treat(load_compilation_module(File), Base,_M,_VNs,_Ln0,_Ln1) :- !,
    get_base_name(File, BFile, _, _),
    assertz_fact(loads(Base, File)),
    do_load_compilation_module(BFile, File, Base).

% ---------------------------------------------------------------------------
% add_sentence_trans/2 - Add a sentence translation hook

decl__treatDom(add_sentence_trans(_,_)).
decl__treat(add_sentence_trans(P, Prior), Base, M,_VNs, Ln0, Ln1) :- !,
    check_qualified_hook(P, add_sentence_trans, 2, Ln0, Ln1),
    do_add_sentence_trans(M, P, Prior, Base, Ln0, Ln1).

% ---------------------------------------------------------------------------
% add_term_trans/2 - Add a term translation hook

decl__treatDom(add_term_trans(_,_)).
decl__treat(add_term_trans(P, Prior), Base, M,_VNs, Ln0, Ln1) :- !,
    check_qualified_hook(P, add_term_trans, 2, Ln0, Ln1),
    do_add_term_trans(M, P, Prior, Base, Ln0, Ln1).

% ---------------------------------------------------------------------------
% add_goal_trans/2 - Add a goal translation hook

decl__treatDom(add_goal_trans(_,_)).
decl__treat(add_goal_trans(P, _),_Base,_M,_VNs,Ln0,Ln1) :- !,
    % Just check that the hook is module qualified
    check_qualified_hook(P, add_goal_trans, 2, Ln0, Ln1).

% ---------------------------------------------------------------------------
% add_clause_trans/2 - Add a clause translation hook

% These four processed from clause_of
decl__treatDom(add_clause_trans(_,_)).
decl__treat(add_clause_trans(P, _),_Base,_M,_VNs,Ln0,Ln1) :- !,
    % Just check that the hook is module qualified
    check_qualified_hook(P, add_clause_trans, 2, Ln0, Ln1).

% ---------------------------------------------------------------------------
:- doc(subsection, "Extensible Declarations").

% ---------------------------------------------------------------------------
% new_declaration/1 - Declare a new declaration

decl__treatDom(new_declaration(_)).
decl__treat(new_declaration(S), Base,_M,_VNs, Ln0, Ln1) :- !,
    do_new_decl(S, off, Base, Ln0, Ln1).

% ---------------------------------------------------------------------------
% new_declaration/2 - Declare a new declaration (with visibility)

decl__treatDom(new_declaration(_, _)).
decl__treat(new_declaration(S, ITF), Base,_M,_VNs, Ln0, Ln1) :- !,
    do_new_decl(S, ITF, Base, Ln0, Ln1).

do_new_decl(S, ITF, Base, Ln0, Ln1) :-
    ( S = F/A, functor(D, F, A) ->
        asserta_fact(new_decl(Base, D, ITF))
    ; compiler_error(Ln0, Ln1, badly_formed(new_declaration, S))
    ).

% ---------------------------------------------------------------------------
:- doc(subsection, "Module entities").

% initialization/1 - Initialization hook
decl__treatDom(initialization(_)).
decl__treat(initialization(_),_Base,_M,_VNs,_Ln0,_Ln1) :- !. % (treated elsewhere)

% on_abort/1 - Abort hook
decl__treatDom(on_abort(_)).
decl__treat(on_abort(_),_Base,_M,_VNs,_Ln0,_Ln1) :- !. % (treated elsewhere)

% ---------------------------------------------------------------------------
% redefining/1 - Declare a predicate redefinition

decl__treatDom(redefining(_)).
decl__treat(redefining(P), Base,_M,_VNs, Ln0, Ln1) :- !,
    do_redefining(P, Base, Ln0, Ln1).

do_redefining(F/A, Base, _, _) :- !,
    defines_module(Base, M),
    asserta_fact(redefining(M, F, A)).
do_redefining(Bad,_Base, Ln0, Ln1) :-
    error_in_lns(Ln0, Ln1, error,
                 ['bad predicate indicator pattern ',~~(Bad)]).

% ---------------------------------------------------------------------------
% multifile/1 - Declare a predicate as a multifile

decl__treatDom(multifile(_)).
decl__treat(multifile(Spec), Base,_M,_VNs, Ln0, Ln1) :- !,
    do_multifile(Spec, Base, Ln0, Ln1).

do_multifile(Spec, Base, Ln0, Ln1) :-
    sequence_contains(Spec, bad_spec_error(multifile, Ln0, Ln1), F, A),
      ( retract_fact(defines_pred(Base,F,A)) -> true ; true ),
      add_multifile_pred(Base,F,A),
    fail.
do_multifile(_, _, _, _).

add_multifile_pred(Base,F,A) :-
    ( current_fact(multifile_pred(Base,F,A)) -> true
    ; assertz_fact(multifile_pred(Base,F,A))
    ).

% ---------------------------------------------------------------------------
% data/1 dynamic/1 concurrent/1
% Set the preddef of a predicate to Def using Decl declaration (Decl
% is used to emit error messages)

decl__treatDom(data(_)).
decl__treat(data(L), Base,_M,_VNs, Ln0, Ln1) :- !,
    do_dyn_decl(L, Base, data, Ln0, Ln1).

decl__treatDom(dynamic(_)).
decl__treat(dynamic(L), Base,_M,_VNs, Ln0, Ln1) :- !,
    do_dyn_decl(L, Base, dynamic, Ln0, Ln1).

decl__treatDom(concurrent(_)).
decl__treat(concurrent(L), Base,_M,_VNs, Ln0, Ln1) :- !,
    do_dyn_decl(L, Base, concurrent, Ln0, Ln1).

do_dyn_decl(Spec, Base, Decl, Ln0, Ln1) :-
    sequence_contains(Spec, bad_spec_error(Decl, Ln0, Ln1), F, A),
      defined_in_source(Base, F, A),
      assert_dyn_decl(Base, F, A, Decl, Ln0, Ln1),
    fail.
do_dyn_decl(_, _, _, _, _).

assert_dyn_decl(Base, F, A, Decl, Ln0, Ln1) :-
    dyn_decl(Base, F, A, Decl2), !,
    ( Decl2 = Decl -> true
    ; compiler_error(Ln0, Ln1, incompatible_decl(F,A,Decl,Decl2))
    ).
assert_dyn_decl(Base, F, A, Decl,_Ln0,_Ln1) :-
    assertz_fact(dyn_decl(Base, F, A, Decl)).

% ---------------------------------------------------------------------------
% impl_defined/1

decl__treatDom(impl_defined(_)).
decl__treat(impl_defined(L), Base,_M,_VNs, Ln0, Ln1) :- !,
    do_impl_defined(L, Base, Ln0, Ln1).

do_impl_defined(SL, Base, Ln0, Ln1) :-
    sequence_contains(SL, bad_spec_error(impl_defined, Ln0, Ln1), F, A),
      defined_in_source(Base, F, A),
      asserta_fact(impl_defines(Base, F, A)),
    fail.
do_impl_defined(_, _, _, _).

% ----------------------------------------------------------------
% meta_predicate/1 - metapredicate information for a predicate

decl__treatDom(meta_predicate(_)).
decl__treat(meta_predicate(Spec), Base,_M,_VNs, Ln0, Ln1) :- !,
    do_meta_predicate(Spec, Base, Ln0, Ln1).

do_meta_predicate(V, _, Ln0, Ln1) :- var(V), !,
    compiler_error(Ln0, Ln1, bad_meta_predicate(V)).
%do_meta_predicate((Spec0,Spec), Base, Ln0, Ln1) :- !,
%        do_meta_predicate(Spec0, Base, Ln0, Ln1),
%        do_meta_predicate(Spec, Base, Ln0, Ln1).
do_meta_predicate(Spec, Base,_Ln0,_Ln1) :-
    functor(Spec, F, A),
    atom(F), integer(A),
    functor(NSpec, F, A),
    normalize_meta_args(1, A, Spec, NSpec), !,
    assertz_fact(meta_pred(Base,F,A,NSpec)).
do_meta_predicate(Bad, _, Ln0, Ln1) :-
    compiler_error(Ln0, Ln1, bad_meta_predicate(Bad)).

normalize_meta_args(N, A, _, _):- N>A, !.
normalize_meta_args(N, A, Spec, NSpec):-
    arg(N, Spec, X),
    normalize_meta_arg(X, NX),
    arg(N, NSpec, NX),
    N1 is N+1,
    normalize_meta_args(N1, A, Spec, NSpec).

normalize_meta_arg('?', '?'). % A variable is valid also!
normalize_meta_arg('-', '?').
normalize_meta_arg('+', '?').
normalize_meta_arg(':', goal).
normalize_meta_arg(addmodule, addmodule(?)).
normalize_meta_arg(addmodule(X), addmodule(Xn)) :-
    normalize_meta_arg(X, Xn).
normalize_meta_arg(X, X) :- real_meta_arg(X).

real_meta_arg(goal).
real_meta_arg(clause).
real_meta_arg(retracting_clause).
real_meta_arg(fact).
real_meta_arg(spec).
real_meta_arg(pred(N)) :- integer(N), N>=0, N=<255.
real_meta_arg(list(X)) :- real_meta_arg(X).
real_meta_arg(addterm(X)) :- real_meta_arg(X).

% ---------------------------------------------------------------------------
:- doc(subsection, "Support for conditional compilation").

decl__treatDom(compilation_fact(_)).
decl__treat(compilation_fact(Fact), _Base, M,_VNs, _Ln0, _Ln1) :- !,
    % TODO: use line numbers?
    add_condcomp_fact(Fact, M).

% ---------------------------------------------------------------------------
:- doc(subsection, "Definitions for packages and included files").

% ---------------------------------------------------------------------------
% use_package/1 - Include a package

decl__treatDom(use_package(_)).
decl__treat(use_package(Package), Base, M,_VNs, Ln0, Ln1) :- !,
    do_use_package(Base, M, Ln0, Ln1, Package).

do_use_package(Base, Module, Ln0, Ln1, Fs) :-
    do_use_package_(Fs, Base, Module, Ln0, Ln1).

% TODO: accepts terms in list tail, e.g. [a|k], which is ugly
do_use_package_([], _, _, _, _) :- !.
do_use_package_([F|Fs], Base, Module, Ln0, Ln1) :- !,
    do_use_package_(F, Base, Module, Ln0, Ln1),
    do_use_package_(Fs, Base, Module, Ln0, Ln1).
do_use_package_(F, Base, Module, Ln0, Ln1) :-
    package_file(F, P), !,
    ( current_fact(package(Base,P)) -> true
    ; assertz_fact(package(Base,P)),
      do_include(package, P, Base, Module, Ln0, Ln1)
    ).
do_use_package_(F, _, _, Ln0, Ln1) :-
    compiler_error(Ln0, Ln1, bad_package_file(F)).

package_file(F, P) :-
    ( atom(F) -> P = library(F)
    ; functor(F,_,1) -> P = F
    ).

% ---------------------------------------------------------------------------
% include/1 - Include a file

decl__treatDom(include(_)).
decl__treat(include(File), Base, M,_VNs, Ln0, Ln1) :- !,
    do_include(source, File, Base, M, Ln0, Ln1).

% Type can be 'source' for included files, 'package' for packages.
do_include(Type, File, Base, Module,_Ln0,_Ln1) :-
    nonvar(File),
    get_base_name(File, SourceBase, SourceFile, _), !,
    assertz_fact(includes(Base, File)),
    %
    now_doing_include(Type, SourceFile),
    '$open'(SourceFile, r, Stream),
    ( read_sentence(Stream, Base, Sentence) -> true ; fail ), % (once)
    check_include_decl(Type, SourceBase, Sentence, Rest),
    % TODO: Merge with read_record_file_/3
    ( member(Sentence2, Rest)
    ; read_sentence(Stream, Base, Sentence2)
    ),
    ( Sentence2 = end_of_file(_, _) ->
        true
    ; process_sentence(Sentence2, Base, SourceFile, Module),
      fail
    ),
    !,
    close(Stream),
    end_doing.
do_include(_Type, File,_Base,_Module, Ln0, Ln1) :-
    compiler_error(Ln0, Ln1, bad_file(File)).

now_doing_include(source, SourceFile) :- now_doing(['Including ',SourceFile]).
now_doing_include(package, SourceFile) :- now_doing(['Using package ',SourceFile]).

% Check that packages contains the right declarations. Nothing is
% required for included source.
check_include_decl(source, _, Sentence, [Sentence]).
check_include_decl(package, SourceBase, Sentence, Sentences) :-
    ( Sentence = sentence(Data, _, _, Ln0, Ln1),
      Data = (:- package(M)) ->
        Sentences = [],
        module_from_base(SourceBase, SM),
        ( SM = M -> % Allow vars in package declarations
            true
        ; compiler_error(Ln0, Ln1, bad_package(SourceBase, M))
        )
    ; % Do not consume the sentence, it is not a valid package declaration
      Sentences = [Sentence],
      sentence_lines(Sentence, Ln0, Ln1),
      warning_package_missing(Ln0, Ln1)
    ).
    
sentence_lines(sentence(_,_,_,Ln0,Ln1), Ln0, Ln1).
sentence_lines(end_of_file(Ln0,Ln1), Ln0, Ln1).
    
warning_package_missing(L0, L1) :-
    error_in_lns(L0, L1, warning,
                 ['Source used as package without package declaration']).

% ---------------------------------------------------------------------------
:- doc(subsection, "Declaration of Module Properties").

% ---------------------------------------------------------------------------
% Import/(re)export symbols from/to modules/classes

decl__treatDom(use_module(_,_)).
decl__treat(use_module(File,Imports), Base,_M,_VNs, Ln0, Ln1) :- !,
    do_use_module(File, Imports, Base, Ln0, Ln1).

decl__treatDom(use_module(_)).
decl__treat(use_module(File), Base,_M,_VNs, Ln0, Ln1) :- !,
    do_use_module(File, all, Base, Ln0, Ln1).

decl__treatDom(ensure_loaded(_)).
decl__treat(ensure_loaded(File), Base,_M,_VNs,_Ln0,_Ln1) :- !,
    get_base_name(File, _, _, _),
    assertz_fact(adds(Base,File)).

% import/2 - Import predicates from a module without checks
decl__treatDom(import(_,_)).
decl__treat(import(Module,Imports), Base,_M,_VNs, Ln0, Ln1) :- !,
    do_import(Module, Imports, Base, Ln0, Ln1).

decl__treatDom(reexport(_)).
decl__treat(reexport(File), Base,_M,_VNs, Ln0, Ln1) :- !,
    do_reexport(File, all, Base, Ln0, Ln1).

decl__treatDom(reexport(_,_)).
decl__treat(reexport(File,Preds), Base,_M,_VNs, Ln0, Ln1) :- !,
    do_reexport(File, Preds, Base, Ln0, Ln1).

% export/1 - Export predicates or symbols
% NOTE: Not exactly like 'public'. This allows ':- export(_)'.
decl__treatDom(export(_)).
decl__treat(export(Exports), Base,_M,_VNs, Ln0, Ln1) :- !,
    assert_export_list(Exports, Base, Ln0, Ln1).

% ---------------------------------------------------------------------------
:- doc(subsection, "Compiler/system options and syntactic extensions for this module").

% op/3 - Define an operator
decl__treatDom(op(_, _, _)).
decl__treat(op(P, F, O), Base,_M,_VNs,_Ln0,_Ln1) :- !,
    do_op(P, F, O, Base).

do_op(P, F, O, Base) :-
    ( ensure_op_undone(P, F, O, Base),
      op(P, F, O), ! % This can give errors
    ; true
    ).

ensure_op_undone(Prec, F, Ops, Base) :-
    integer(Prec), 0=<Prec, Prec=<1200,
    nonvar(F),
    op_type(F, T),
    atom_or_atom_list(Ops), !,
    ensure_ops_undone(Ops, F, T, Prec, Base).
ensure_op_undone(_, _, _, _). % do not fail to give errors

ensure_ops_undone([Op|Ops], F, T, Prec, Base) :- !,
    ensure_ops_undone(Op, F, T, Prec, Base),
    ensure_ops_undone(Ops, F, T, Prec, Base).
ensure_ops_undone([], _, _, _, _) :- !.
ensure_ops_undone(Op, F, T, Prec, Base) :-
    ( current_op(CPrec, CF, Op), op_type(CF, T) ->
      asserta_fact(undo_decl(Base,op(Prec,F,Op),op(CPrec,CF,Op)))
    ; asserta_fact(undo_decl(Base,op(Prec,F,Op),op(0,F,Op)))
    ).

op_type(fy, pre).
op_type(fx, pre).
op_type(yfx, in).
op_type(xfy, in).
op_type(xfx, in).
op_type(yf, post).
op_type(xf, post).

atom_or_atom_list(A) :- atom(A), !.
atom_or_atom_list([A|L]) :-
    atom(A),
    atom_or_atom_list(L).

% set_prolog_flag/2 - Set a compilation flag
decl__treatDom(set_prolog_flag(_, _)).
decl__treat(set_prolog_flag(Flag, Value), Base,_M,_VNs, Ln0, Ln1) :- !,
    do_set_pl_flag(Flag, Value, Base, Ln0, Ln1).

do_set_pl_flag(Flag, Value, Base, Ln0, Ln1) :-
    ( prolog_flag(Flag, Old, Value) ->
        asserta_fact(undo_decl(Base, set_prolog_flag(Flag,Value),
                                     set_prolog_flag(Flag,Old)))
    ; warning_failed_decl(Ln0, Ln1, set_prolog_flag(Flag, Value))
    ).

% push_prolog_flag/2 - Push a compilation flag
decl__treatDom(push_prolog_flag(_, _)).
decl__treat(push_prolog_flag(Flag, Value), Base,_M,_VNs, Ln0, Ln1) :- !,
    do_push_pl_flag(Flag, Value, Base, Ln0, Ln1).

do_push_pl_flag(Flag, Value, Base, Ln0, Ln1) :-
    ( push_prolog_flag(Flag, Value) ->
        asserta_fact(undo_decl(Base, push_prolog_flag(Flag,Value),
                                     pop_prolog_flag(Flag)))
    ; warning_failed_decl(Ln0, Ln1, push_prolog_flag(Flag, Value))
    ).

% pop_prolog_flag/1 - Pop a compilation flag
decl__treatDom(pop_prolog_flag(_)).
decl__treat(pop_prolog_flag(Flag), Base,_M,_VNs, Ln0, Ln1) :- !,
    do_pop_pl_flag(Flag, Base, Ln0, Ln1).

do_pop_pl_flag(Flag, Base, Ln0, Ln1) :-
    ( current_prolog_flag(Flag, Value),
      pop_prolog_flag(Flag) ->
        asserta_fact(undo_decl(Base, pop_prolog_flag(Flag),
                                     push_prolog_flag(Flag,Value)))
    ; warning_failed_decl(Ln0, Ln1, pop_prolog_flag(Flag))
    ).

