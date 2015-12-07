:- module(_, [inliner_sentence_tr/3, inliner_goal_tr/3, inline_db/4,
		lit_clause_arity/4, in_inline_module_db/2, compound_struct/3],
	    [assertions, hiord, dcg]).

:- use_module(library(pathnames)).
:- use_module(library(aggregates)).
:- use_module(library(terms)).
:- use_module(library(freeze)).
:- use_module(library(sort)).
:- use_module(library(lists)).
:- use_module(library(hiordlib)).
:- use_module(library(messages)).
:- use_module(library(terms_vars)).
:- use_module(library(write)).
:- use_module(library(terms_check)).
:- use_module(library(compiler/c_itf_internal),
	    [defines_module/2, meta_args/2, multifile/3, imports_pred/7,
		exports_pred/3, location/1]).
:- use_module(library(assertions/assrt_lib),    [assertion_read/9]).
:- use_module(engine(meta_inc), [meta_inc_args/3]).
:- use_module(library(rtchecks/rtchecks_basic), [list_to_lits/2]).

:- doc(author, "Edison Mera").

:- doc(title, "Inline expansions and unfolding.").

:- doc(module, "This module provides an inliner, and an unfolder.

The inliner tries to avoid calls to predicates whose clauses are
mutually exclusive, substituting the literal with the body of such
predicate.

The unfolder tries to resolve as many computations as possible at
compilation time, that is, meta calls in which the goal is
instantiated and partial unifications in the head and in the body of
clauses that can be resolved statically.

Can be understood as a restricted partial evaluator.

Examples:
It is being used in the rtcheck package and in the resources analysis
of ciaopp.").

:- doc(bug, "Suggestion to debug: rename this file and debug the
	renamed file, because it is compiled statically in the
	shell.").

:- data source_clause_db/3.
:- data meta_pred_db/4.
:- data rename_goal_db/4.
:- data inline_db/4.
:- data unused_inline_db/4.
:- data renamer_db/3.
:- data unused_renamer_db/4.
:- data unused_unfold_db/3.
:- data no_slice_db/3.
:- data inline_module_db/4.
:- data in_inline_module_db/2.
:- data unfold_db/2.
:- data generated_db/3.
:- data unfold_meta_db/1.
:- data use_module_db/4.

:- doc(bug, "If the predicate being inline contains cuts (!), it
	could generate a malformed body error. --EMM").

% :- argnames iparams(locks, pending, nuvars, vars, level, module).

show_unused_inline_messages(M) :-
	findall(message_lns(Source, Ln0, Ln1, warning, ['Undefined predicate ',
		    ~~(M:F/A), ' have inline declaration.']),
	    retract_fact(unused_inline_db(F, A, M, loc(Source, Ln0, Ln1))),
	    Messages),
	messages(Messages).

show_unused_renamer_messages(M) :-
	findall(message_lns(Source, Ln0, Ln1, warning, ['Undefined predicate ',
		    ~~(M:F/A), ' have renamer declaration.']),
	    retract_fact(unused_renamer_db(F, A, M, loc(Source, Ln0, Ln1))),
	    Messages),
	messages(Messages).


inliner_sentence_tr(0, _, M) :-
	!,
	cleanup_db(M).
inliner_sentence_tr(end_of_file, end_of_file, _) :-
	!.
% 	cleanup_db.
inliner_sentence_tr((:- Declaration), Clauses, M) :-
	!,
	process_declaration(Declaration, Clauses, M).
inliner_sentence_tr((Head :- Body), Clauses, M) :-
	!,
	process_sentence(Head, Body, Clauses, M).
inliner_sentence_tr(Head, Clauses, M) :-
	process_sentence(Head, true, Clauses, M).

inliner_goal_tr(end_of_file, _, M) :-
	!,
	show_unused_inline_messages(M),
	show_unused_renamer_messages(M),
	cleanup_db(M),
	fail.
inliner_goal_tr(Goal0, Goal, M) :-
	varset(Goal0, Vars), %Kludge to avoid removal of variables
	body_expansion(Goal0, iparams([], _, Vars, Vars, 2, M), Goal, Clauses,
	    []),
	(Clauses \== [] -> warning_cl_goal_exp(Clauses), fail ; true),
	!,
	Goal0 \== Goal.

% Assume that meta predicates should be unfolded:
process_declaration(unfold_meta, [], M) :-
	assertz_fact(unfold_meta_db(M)).

% By default, unfold the meta arguments in meta predicates:
process_declaration(meta_predicate(PredSpec), Clauses, M) :-
	!,
	process_meta_predicate(PredSpec, Clauses, M).
% Unfold the specified arguments even if they are partially instantiated:
process_declaration(unfold(PredSpec), [], M) :-
	!,
	assert_unfold(PredSpec, M).
% Used internally to mark the start of an inlined module
process_declaration(ini_inline_module(Base, Alias, PredList), [], M) :-
	!,
	asserta_fact(inline_module_db(Base, Alias, PredList, M)),
	asserta_fact(in_inline_module_db(Base, M)).
% Used internally to mark the end of an inlined module
process_declaration(end_inline_module(Base), [], M) :-
	!,
	retract_fact(in_inline_module_db(Base, M)),
	(
	    inline_module_db(Base, _, PredList, M),
	    nonvar(PredList) ->
	    findall(F/A, (member(F/A, PredList), \+inline_db(F, A, _, M)),
		UnusedPreds),
	    (
		UnusedPreds \== [] ->
		location(Loc),
		warning_message(Loc,
		    "Inlined predicates ~w, not exported by ~w",
		    [UnusedPreds, Base])
	    ;
		true
	    )
	;
	    true
	).
% Expand the goal inline if possible, and if predicate is unused,
% removes its definition in the module, saving space:
process_declaration(inline(F/A), [], M) :-
	!,
	assert_inline(F, A, srclocal, M),
	assert_unused_inline(F, A, M).
% Declare a user-defined name for a specialization:
process_declaration(renamer(F/A), [], M) :-
	!,
	assert_renamer(F, A, M).
% Remove the inline declaration
process_declaration(noinline(F/A), [], M) :-
	!,
	retract_fact(inline_db(F, A, _, M)).
% Allows usage of inline/unfolded predicates in the body the involved
% predicate, but do not remove its definition even if it is unused.
process_declaration(use_inline(F/A), [], M) :-
	!,
	assert_inline(F, A, srclocal, M),
	assert_unused_inline(F, A, M),
	assert_no_slice(F, A, M).
% Allows inlining of the predicates specifield in PredList in a module
process_declaration(inline_module(Alias, PredList), Clauses, M) :-
	!,
	process_inline_module(Alias, PredList, Clauses, M).
% Allows inlining of all the predicates in a module
process_declaration(inline_module(Alias), Clauses, M) :-
	!,
	process_inline_module(Alias, _, Clauses, M).
process_declaration(use_module(Alias), Clauses, M) :-
	!,
	process_use_module(use_module(Alias), combine_inline_info, Clauses, M).
process_declaration(use_module(Alias, PredList), Clauses, M) :-
	!,
	process_use_module(use_module(Alias, PredList), combine_inline_info,
	    Clauses, M).
process_declaration(Decl0, Decl, M) :-
	in_inline_module_db(_, M),
	!,
	(pass_thru_decl(Decl0) -> Decl = [(:- Decl0)] ; Decl = []).

use_module_alias_predlist(use_module(Alias, PredList), Alias, PredList).
use_module_alias_predlist(use_module(Alias),           Alias, _).

union_lists(PrevList, CurrList, UnionList) :-
	( var(PrevList) -> UnionList = PrevList
	; var(CurrList) -> UnionList = CurrList
	; union(PrevList, CurrList, UnionList)
	).

diff_lists(PrevList, CurrList, DiffList) :-
	( var(PrevList) ->DiffList = []
	; var(CurrList) ->
	    (var(PrevList) -> DiffList = [] ; DiffList = CurrList)
	; difference(CurrList, PrevList, DiffList)
	).

combine_inline_info(Base, M, PrevList, CurrList, UnionList, DiffList) :-
	(inline_module_db(Base, _, InlineList, M) -> true ; InlineList = []),
	union_lists(PrevList,   InlineList, UnionList0),
	union_lists(UnionList0, CurrList,   UnionList),
	diff_lists(UnionList0, CurrList, DiffList).

combine_usage_info(_, _, PrevList, CurrList, UnionList, DiffList) :-
	union_lists(PrevList, CurrList, UnionList),
	diff_lists(PrevList, CurrList, DiffList).

:- meta_predicate process_use_module(?, pred(6), ?, ?).
process_use_module(Decl, CombineInfo, Clauses, M) :-
	use_module_alias_predlist(Decl, Alias, CurrList),
	absolute_file_name(Alias, File),
	file_base(File, Base),
	( retract_fact(use_module_db(Base, PrevInlineModule, PrevList, M)) ->
	    true ; PrevList = []
	),
	CombineInfo(Base, M, PrevList, CurrList, UnionList, DiffList),
	( DiffList == [] -> DiffClauses = []
	; var(DiffList) -> DiffClauses = [(:- use_module(Alias))]
	; DiffClauses = [(:- use_module(Alias, DiffList))]
	),
	( (in_inline_module_db(_, M) ; PrevInlineModule == yes) ->
	    Clauses = DiffClauses,
	    InlineModule = yes
	;
	    Clauses = [(:- Decl)],
	    InlineModule = no
	),
	asserta_fact(use_module_db(Base, InlineModule, UnionList, M)).


pass_thru_decl(use_package(_)).
pass_thru_decl(use_module(_)).
pass_thru_decl(use_module(_, _)).
pass_thru_decl(include(_)).

file_base(File, Base) :-
	path_basename(File, FN),
	path_splitext(FN, Base, _Ext).

% :- use_module(library(read)).
:- use_module(library(compiler/c_itf_internal),
	    [expand_module_decl/5, read_sentence/3]).
process_inline_module(Alias, PredList,
	    [(:- use_package(Package)),
		(:- use_package(.(BAuto)))], M) :-
	% get_idx(Alias, M, Idx),
	% atom_number(AIdx, Idx),
	absolute_file_name(Alias, File),
	file_base(File, Base),
	% The next way to process inline modules is a kludge, a better
	% way is to support it in the compiler. --EMM
	atom_concat([M, '_', Base, '_auto'], BAuto),
	atom_concat(BAuto, '.pl', FAuto),
	absolute_file_name(Alias, File),
	open(File, read, Stream),
	% read(Stream, _),
	read_sentence(Stream, Base, Sentence),
	expand_module_decl(Sentence, Base, module, module(_Module, Exports,
		Package), _Rest),
	(var(PredList) -> PredList = Exports ; true),
	open(FAuto, write, OStream),
	writeq(OStream, (:- package(BAuto))),
	display(OStream, '.\n'),
	writeq(OStream, (:- ini_inline_module(Base, Alias, PredList))),
	display(OStream, '.\n'),
	% writeq(OStream, (:- use_package(Package))),
	% display(OStream, '.\n'),
	writeq(OStream, (:- push_prolog_flag(unused_pred_warnings, yes))),
	display(OStream, '.\n'),
	writeq(OStream, (:- include(Alias))),
	display(OStream, '.\n'),
	writeq(OStream, (:- pop_prolog_flag(unused_pred_warnings))),
	display(OStream, '.\n'),
	writeq(OStream, (:- end_inline_module(Base))),
	display(OStream, '.\n'),
	close(OStream).

assert_unfold(PredSpec, M) :-
	functor(PredSpec, F, A),
	functor(PrevSpec, F, A),
	retractall_fact(unfold_db(PrevSpec, M)),
	assertz_fact(unfold_db(PredSpec, M)),
	location(Loc),
	assertz_fact(unused_unfold_db(PredSpec, M, Loc)).

assert_inline(F, A, Alias, M) :-
	inline_db(F, A, _, M) -> true
    ;
	assertz_fact(inline_db(F, A, Alias, M)).

assert_unused_inline(F, A, M) :-
	location(Loc),
	assertz_fact(unused_inline_db(F, A, M, Loc)).

assert_renamer(F, A, M) :-
	renamer_db(F, A, M) -> true
    ;
	assertz_fact(renamer_db(F, A, M)),
	location(Loc),
	assertz_fact(unused_renamer_db(F, A, M, Loc)).

assert_no_slice(F, A, M) :-
	( no_slice_db(F, A, M) -> true
	; assertz_fact(no_slice_db(F, A, M)) ).

process_meta_predicate(PredSpec, Clauses, M) :-
	functor(PredSpec, F, LitArity),
	(
	    in_inline_module_db(Base, M) ->
	    inline_module_db(Base, Alias, PredList, M),
	    assert_inline(F, LitArity, Alias, M),
	    ( nonvar(PredList), member(F/LitArity, PredList) ->
		assert_unused_inline(F, LitArity, M)
	    ; true
	    ),
	    assert_unfold_from_meta_decl(PredSpec, M),
	    assert_meta_pred_if_required(F, LitArity, PredSpec, Clauses, M)
	;
	    unfold_meta_db(M) ->
	    assert_unfold_from_meta_decl(PredSpec, M),
	    assert_meta_pred_if_required(F, LitArity, PredSpec, Clauses, M)
	;
	    Clauses = [(:- meta_predicate(PredSpec))]
	).

assert_meta_pred_if_required(F, LitArity, PredSpec, Clauses, M) :-
	inline_db(F, LitArity, _, M) ->
	Clauses = [],
	assertz_fact(meta_pred_db(F, LitArity, PredSpec, M))
    ;
	Clauses = [(:- meta_predicate(PredSpec))].

assert_unfold_from_meta_decl(PredSpec, M) :-
	functor(PredSpec, F, A),
	functor(UfldSpec, F, A),
	\+ unfold_db(UfldSpec, M) ->
	meta_unfold(1, PredSpec, UfldSpec),
	assertz_fact(unfold_db(UfldSpec, M))
    ;
	true.

meta_unfold(A, PredSpecs, UFldSpecs) :-
	arg(A, PredSpecs, PredSpec),
	arg(A, UFldSpecs, UFldSpec),
	meta_unfold_each(PredSpec, UFldSpec),
	A1 is A + 1,
	!,
	meta_unfold(A1, PredSpecs, UFldSpecs).
meta_unfold(_, _, _).

meta_unfold_each(goal,          down) :- !.
meta_unfold_each(clause,        down) :- !.
meta_unfold_each(fact,          no) :- !.
meta_unfold_each(pred(_),       down) :- !.
meta_unfold_each(list(ArgSpec), UFldSpec) :-
	!,
	meta_unfold_each(ArgSpec, UFldSpec).
meta_unfold_each(addterm(ArgSpec), UFldSpec) :-
	!,
	meta_unfold_each(ArgSpec, UFldSpec).
meta_unfold_each(addmodule(ArgSpec), UFldSpec) :-
	!,
	meta_unfold_each(ArgSpec, UFldSpec).
meta_unfold_each(_, no).

lit_clause_arity(M, F, LitArity, ClauseArity) :-
	meta_predicate(F, LitArity, Meta, M),
	meta_inc_args(Meta, LitArity, ClauseArity) -> true
    ;
	LitArity = ClauseArity.

assert_clause_if_required(Head, F, LitArity, Body, M) :-
	( functor(Spec, F, LitArity), unfold_db(Spec, M)
	; inline_db(F, LitArity, _, M)
	; \+ ( ( use_module_db(_Base, _, PredList, M) ->
		    member(F/LitArity, PredList) ; true ) )
	) ->
	assert_clause(Head, Body, M)
    ;
	true.

assert_clause(Head, Body, M) :-
	assertz_fact(source_clause_db(Head, Body, M)).

meta_predicate(F, A, PredSpec, M) :-
	meta_pred_db(F, A, PredSpec, M).
meta_predicate(F, A, PredSpec, M) :-
	multifile(M, F, A),
	functor(PredSpec, F, A),
	meta_args(multifile, PredSpec).
meta_predicate(F, A, PredSpec, M) :-
	functor(PredSpec, F, A),
	defines_module(Base, M),
	imports_pred(Base, _, F, A, _, PredSpec, _).

assert_generated(F, A, M) :-
	(generated_db(F, A, M) -> true ; assertz_fact(generated_db(F, A, M))).

% only takes the source code of the specified predicates
process_sentence(Head, Body, Clauses, M) :-
	functor(Head, F, ClauseArity),
	lit_clause_arity(M, F, LitArity, ClauseArity),
	retractall_fact(unused_inline_db(F, LitArity, M, _)),
	functor(PredSpec, F, LitArity),
	retractall_fact(unused_unfold_db(PredSpec, M, _)),
	process_sentence_(Head, Body, Clauses, M, F, LitArity).

is_renamer(Body0,  _) :- var(Body0), !, fail.
is_renamer((_, _), _) :- !, fail.
is_renamer(Body,   Head) :-
	location(Loc),
	functor(Head, F, Arity),
	renamer_db(F, Arity, M) ->
	( retract_fact(unused_renamer_db(F, Arity, M, _)) -> true
	; error_message(Loc, "Renamer ~w/~w must have only one clause",
		[F, Arity])
	),
	(
	    Head =.. [F|Args],
	    ( list(Args, var) -> true
	    ; error_message(Loc,
		    "Header of Renamer ~w/~w not contain only variables",
		    [F, Arity]),
		fail
	    ),
	    varset(Head, Args1),
	    ( length(Args1, Arity) -> true
	    ; error_message(Loc, "Header of Renamer ~w/~w is not linear"),
		fail
	    ),
	    ( varset(Body, Args2), diff_vars(Args1, Args2, []) -> true
	    ;
		error_message(Loc,
		    "Header of Renamer ~w/~w contain singleton variables",
		    [F, Arity]),
		fail
	    )
	),
	!.

process_sentence_(Head, Body, Clauses, M, F, LitArity) :-
	in_inline_module_db(Base, M),
	!,
	inline_module_sentence(Head, Body, Clauses, M, F, LitArity, Base).
process_sentence_(Head, Body0, Clauses, M, F, LitArity) :-
	assert_clause_if_required(Head, F, LitArity, Body0, M),
	(
	    defines_module(Base, M),
	    inline_db(F, LitArity, _, M),
	    \+ no_slice_db(F, LitArity, M),
	    \+ exports_pred(Base, F, LitArity) ->
	    Clauses = [],
	    retractall_fact(assertion_read(Head, M, _, _, _, _, _, _, _))
	;
	    % is too early to begin expansion of F/A predicate: suppose that there
	    % are more clauses
	    varset(Head, Vars),
	    assert_generated(F, LitArity, M),
	    (
		no_slice_db(F, LitArity, M) ->
		Clauses = [(Head :- Body0)]
	    ;

		\+ inline_db(F, LitArity, _, M), is_renamer(Body0, Head)
	    -> generate_specialized_clauses(Body0, Body, renamer(Head),
		    iparams([F/LitArity], _, [], Vars, 1, M), Clauses,
		    Clauses0),
		( functor(Head, FH, AH), functor(Body, FH, AH) ->
		    Clauses0 = [] ; Clauses0 = [(Head :- Body)] )
	    ;
		body_expansion(Body0, iparams([F/LitArity], _, Vars,
			Vars, 1, M), Body, Clauses, [(Head :- Body)])

	    )
	),
	!.

inline_module_sentence(Head, Body, [], M, F, LitArity, Base) :-
	inline_module_db(Base, Alias, PredList, M),
	\+ ( ( use_module_db(Base, no, UMList, M),
		member(F/LitArity, UMList) ) ) ->
	( member(F/LitArity, PredList) ->
	    assert_inline(F, LitArity, Alias, M)
	; true
	),
	assert_clause(Head, Body, M)
    ;
	true.

collapse_dups([],     []).
collapse_dups([P|Ps], Cs) :-
	collapse_dups2(Ps, P, Cs).

collapse_dups2(Ps0, P0, Cs0) :-
	select(P, Ps0, Ps1),
	P == P0,
	!,
	collapse_dups2(Ps1, P0, Cs0).
collapse_dups2(Ps0, P0, [P0|Cs]) :-
	collapse_dups(Ps0, Cs).

vars_of_unfold_arg(yes, _,   Vars,  Vars).
vars_of_unfold_arg(no,  Arg, Vars0, Vars) :-
	varset(Arg, Vars1),
	append(Vars1, Vars, Vars0).

vars_of_unfold_args(N, Specs, Args, Vars0, Vars) :-
	arg(N, Specs, Spec0),
	arg(N, Args,  Arg),
	normalize_spec_down(Spec0, Spec),
	vars_of_unfold_arg(Spec, Arg, Vars0, Vars1),
	N1 is N + 1,
	!,
	vars_of_unfold_args(N1, Specs, Args, Vars1, Vars).
vars_of_unfold_args(_, _, _, Vars, Vars).

transform_args(N, Specs, Lit, Head, Pred, NUVars, Vars0, Vars, TParams0) :-
	arg(N, Specs, Spec0),
	arg(N, Lit,   LArg),
	arg(N, Head,  UArg),
	arg(N, Pred,  Param),
	normalize_spec_down(Spec0, Spec),
	nu_args(Spec0, NUVars, LArg, NUArgs),
	transform_arg(Spec, LArg, UArg, Param, NUArgs, Vars0, Vars1, TParams0,
	    TParams),
	N1 is N + 1,
	!,
	transform_args(N1, Specs, Lit, Head, Pred, NUVars, Vars1, Vars,
	    TParams).
transform_args(_, _, _, _, _, _, Vars, Vars, []).

nu_args(down, NUVars, LArg, NUArgs) :- !, varset(NUVars-LArg,
	    NUArgs).
nu_args(_, NUVars, _, NUVars).

transform_arg(yes, LArg, UArg, LArg, NUVars, Vars, Vars, TParams0, TParams) :-
	partial_unify(LArg, UArg, NUVars, _, []),
	varset(NUVars, NUVars1),
	NUVars1 == NUVars,
	!,
	varset(LArg, TParams1),
	append(TParams1, TParams, TParams0).
transform_arg(_, _, _, Param, _, [Param|Vars], Vars, [Param|TParams], TParams).

gen_new_key(Vars, Pred, M, F, LitArity, Name) :-
	varset(Pred, LVars0),
	intersect_vars(LVars0, Vars, LVars),
	get_idx(idx(Pred, LVars), M, Idx),
	atom_number(AIdx, Idx),
	% atom_concat('inl$', AIdx, Name).
	atom_number(ALitArity, LitArity),
	atom_concat([F, '/', ALitArity, '$', AIdx], Name).

:- data idx_db/1.

get_idx(idx(Pred, Vars0), M, Idx) :-
	prettyvars(Pred),
	sort(Vars0, Vars),
	% hash_term(idx(Pred, Vars), Idx),
	num_term(idx(Pred, Vars), M, Idx),
	assertz_fact(idx_db(Idx)),
	fail.
get_idx(_, _, Idx) :-
	retract_fact(idx_db(Idx)).

:- data num_term_db/3.

num_term(Term, M, Num) :-
	current_fact(num_term_db(Term, M, Num)),
	!.
num_term(Term, M, Num) :-
	(
	    current_fact(num_term_db(_, M, Num0)) ->
	    Num is Num0 + 1
	;
	    Num = 1
	),
	asserta_fact(num_term_db(Term, M, Num)).

fails(A) :-
	nonvar(A),
	fails_(A).

fails_(fail).
fails_(var(A)) :- nonvar(A).
fails_(int(A)) :- \+ int(A).
fails_(integer(A)) :- \+ int(A).
fails_(num(A)) :- \+ num(A).
fails_(number(A)) :- \+ num(A).
fails_(nnegint(A)) :- \+ nnegint(A).
fails_(atm(A)) :- \+ atm(A).
fails_(atom(A)) :- \+ atm(A).
fails_((A, _B)) :- fails(A).

succs(A) :-
	nonvar(A),
	succs_(A).

succs_(true).
succs_(nonvar(A)) :- nonvar(A).
succs_(int(A)) :- integer(A).
succs_(integer(A)) :- integer(A).
succs_(number(A)) :- number(A).
succs_(num(A)) :- nonvar(A), num(A).
succs_(nnegint(A)) :- nonvar(A), nnegint(A).
succs_(atm(A)) :- atom(A).
succs_(atom(A)) :- atom(A).

lit_conj(A, B, B) :-
	succs(A),
	!.
lit_conj(A, B, A) :-
	succs(B),
	!.
lit_conj(A, _, A) :-
	fails(A),
	!.
lit_conj(A, B, (A, B)).

lit_disj(A, B, B) :-
	fails(A),
	!.
lit_disj(A, B, A) :-
	fails(B),
	!.
lit_disj(A, _, C) :-
	nonvar(A),
	A = (B -> C),
	succs(B),
	!.
lit_disj(A, B, (A ; B)).

lit_then(A, _, fail) :-
	fails(A),
	!.
lit_then(A, B, B) :-
	succs(A),
	!.
lit_then(A, B, (A->B)).

lit_neg(A, true) :-
	fails(A),
	!.
lit_neg(A, fail) :-
	succs(A),
	!.
lit_neg(A, \+(A)).

termcount(V, Term, N0, N) :-
	V == Term,
	!,
	N is N0 + 1.
termcount(V, Term, N0, N) :-
	nonvar(Term),
	!,
	termcount_(1, V, Term, N0, N).
termcount(_, _, N, N).

termcount_(I, V, Term, N0, N) :-
	arg(I, Term, Arg),
	I1 is I + 1,
	!,
	termcount(V, Arg, N0, N1),
	termcount_(I1, V, Term, N1, N).
termcount_(_, _, _, N, N).

:- export(varscount/3).
varscount(Term, Vars, Count) :-
	map(Vars, termcount(Term, 0), Count).

add_nuvars(Goal, NUVars, NUVars) :-
	var(Goal),
	!.
add_nuvars(_=_,  NUVars,  NUVars).
add_nuvars(Goal, NUVars0, NUVars) :-
	varset(NUVars0+Goal, NUVars).

selectnu([],     [],     []).
selectnu([V|Vs], [C|Cs], Ss0) :-
	(
	    C > 1 ->
	    Ss0 = [V|Ss]
	;
	    Ss0 = Ss
	),
	selectnu(Vs, Cs, Ss).

add_nuvars_u(A, B, NUVars, NUVarsB) :-
	!,
	varset(A, Vars0),
	varscount(B, Vars0, Count0),
	selectnu(Vars0, Count0, NUVars1),
	varset(NUVars+NUVars1, NUVarsB).

% inst_list(L) :- \+ non_inst(List(L), L).
inst_list(L) :- var(L), !, fail.
inst_list([]).
inst_list([_|L]) :- inst_list(L).

body_expansion(A, _, A) -->
	{var(A)},
	!.
body_expansion((A, B), iparams(Locks, Pending, NUVars, Vars, Level, M), C) -->
	!,
	{varset(Vars+B, VarsA)},
	{add_nuvars_u(A, Vars+B, NUVars, NUVarsA)},
	body_expansion(A, iparams(Locks, Pending, NUVarsA, VarsA, Level, M),
	    NA),
	{varset(Vars+NA, VarsB)},
	{add_nuvars(NA, NUVars, NUVarsB)},
	body_expansion(B, iparams(Locks, Pending, NUVarsB, VarsB, Level, M),
	    NB),
	{lit_conj(NA, NB, C)}.
body_expansion((A; B), iparams(Locks, Pending, NUVars, Vars, Level, M), C) -->
	!,
	{varset(Vars+B,        VarsA)},
	{varset(NUVars+Vars+B, NUVarsA)},
	body_expansion(A, iparams(Locks, Pending, NUVarsA, VarsA, Level, M),
	    NA),
	{varset(Vars+NA,        VarsB)},
	{varset(NUVars+Vars+NA, NUVarsB)},
	body_expansion(B, iparams(Locks, Pending, NUVarsB, VarsB, Level, M),
	    NB),
	{lit_disj(NA, NB, C)}.
body_expansion((A->B), iparams(Locks, Pending, NUVars, Vars, Level, M), C) -->
	!,
	{varset(Vars+B, VarsA)},
	{add_nuvars_u(A, Vars+B, NUVars, NUVarsA)},
	body_expansion(A, iparams(Locks, Pending, NUVarsA, VarsA, Level, M),
	    NA),
	{varset(Vars+NA, VarsB)},
	{add_nuvars(NA, NUVars, NUVarsB)},
	body_expansion(B, iparams(Locks, Pending, NUVarsB, VarsB, Level, M),
	    NB),
	{lit_then(NA, NB, C)}.
body_expansion((X^A), Params, (X^NA)) -->
	!,
	body_expansion(A, Params, NA).
body_expansion((\+ A), Params, C) -->
	!,
	body_expansion(A, Params, NA),
	{lit_neg(NA, C)}.
body_expansion(if(A, B, C), iparams(Locks, Pending, NUVars, Vars, Level, M),
	    if(NA, NB, NC)) -->
	!,
	{varset(Vars+B+C, VarsA)},
	{add_nuvars_u(A, Vars+B+C, NUVars, NUVarsA)},
	body_expansion(A, iparams(Locks, Pending, NUVarsA, VarsA, Level, M),
	    NA),
	{varset(Vars+NA+C,        VarsB)},
	{varset(NUVars+Vars+NA+C, NUVarsB)},
	body_expansion(B, iparams(Locks, Pending, NUVarsB, VarsB, Level, M),
	    NB),
	{varset(Vars+NA+NB,        VarsC)},
	{varset(NUVars+Vars+NA+NB, NUVarsC)},
	body_expansion(C, iparams(Locks, Pending, NUVarsC, VarsC, Level, M),
	    NC).
body_expansion(catch(A, E, B), iparams(Locks, Pending, NUVars, Vars, Level, M),
	    catch(NA, E, NB)) -->
	!,
	{varset(Vars+E+B, VarsA)},
	body_expansion(A, iparams(Locks, Pending, NUVars, VarsA, Level, M),
	    NA),
	{varset(Vars+NA+E,   VarsB)},
	{varset(NUVars+NA+E, NUVarsB)},
	body_expansion(B, iparams(Locks, Pending, NUVarsB, VarsB, Level, M),
	    NB).
body_expansion(intercept(A, E, B), iparams(Locks, Pending, NUVars, Vars, Level,
		M), intercept(NA, E, NB)) -->
	{\+ inline_db(intercept, 3, _, M)},
	!,
	{varset(Vars+E+B, VarsA)},
	body_expansion(A, iparams(Locks, Pending, NUVars, VarsA, Level, M),
	    NA),
	{varset(Vars+NA+E,   VarsB)},
	{varset(NUVars+NA+E, NUVarsB)},
	body_expansion(B, iparams(Locks, Pending, NUVarsB, VarsB, Level, M),
	    NB).
body_expansion(!, _, !) --> !, [].
body_expansion(A, _, true) -->
	{succs(A)}, !, [].
body_expansion(A, _, fail) -->
	{fails(A)}, !, [].
body_expansion(A is E, Params, Goal) -->
	{ground(E), arithexpression(E)},
	!,
	{B is E},
	body_expansion(A=B, Params, Goal).
body_expansion(A=..B, Params, Goal) -->
	{nonvar(A), !, A =.. A0},
	body_expansion(A0=B, Params, Goal).
body_expansion(A=..B, Params, Goal) -->
	{inst_list(B), B = [C|_], atom(C), !, B0 =.. B},
	body_expansion(A=B0, Params, Goal).
body_expansion(A=..B, _,                              A=..B) --> !, [].
body_expansion(A=B,   iparams(_, _, NUVars, _, _, _), Goal) -->
	!,
	{
	    partial_unify(A, B, NUVars, Remainings, []) ->
	    list_to_lits(Remainings, Goal)
	;
	    Goal = fail % Failure detected at compilation time
	}.
body_expansion(call(Goal0), _, call(Goal0)) -->
	{var(Goal0) ; Goal0 = (_:Head), var(Head)},
	!.
body_expansion(call(Goal0), Params, Goal) -->
	!,
	body_expansion(Goal0, Params, Goal).
body_expansion(Goal0, Params, Goal) -->
	{Goal0 =.. [call, Pred|GArgs]},
	!,
	(
	    {var(Pred)} ->
	    {Goal = Goal0}
	;
	    {Pred =.. [F|PArgs]},
	    {comp_args(GArgs, PArgs, Args)},
	    {Goal1 =.. [F|Args]},
	    body_expansion(Goal1, Params, Goal)
	).
body_expansion('$meta_call'(Goal0), Params, Goal) -->
	!,
	body_expansion(Goal0, Params, Goal).
body_expansion(MetaCall, Params, Goal) -->
	{
	    compound_struct(MetaCall, call, [F, Arg|Args]),
	    nonvar(F),
	    callable(F),
	    !,
	    compound_struct(F, F0, Args0),
	    append([Arg|Args0], Args, Args1),
	    compound_struct(Goal0, F0, Args1)
	},
	body_expansion(Goal0, Params, Goal),
	!.
body_expansion(Goal0, Params0, Goal) -->
	{Params0 = iparams(Locks, Pending, NUVars, Vars, Level0, M)},
	{Level is Level0 + 1},
	{Params = iparams(['$wgf$off'|Locks], Pending, NUVars, Vars, Level, M)
	},
	generate_specialized_clauses(Goal0, Goal1, no, Params0),
	(
	    {Goal0 \== Goal1} ->
	    body_expansion(Goal1, Params, Goal)
	;
	    (
		{
		    functor(Goal0, F, A),
		    meta_predicate(F, A, Meta, M),
		    functor(Goal, F, A),
		    functor(Meta, F, A)} ->
		expand_meta_args(Meta, Params, Goal1, Goal)
	    ;
		{Goal1=Goal}
	    )
	),
	!.
body_expansion(Goal, _, Goal) --> [].

comp_args([], PArgs, PArgs).
comp_args([Arg|GArgs], PArgs, [Arg|Args]) :-
	append(PArgs, GArgs, Args).

expand_metas([], _, [], []) --> [].
expand_metas([Meta|Metas], iparams(Locks, Pending, NUVars, Vars, Level, M),
	    [Goal0|Goals0], [Goal|Goals]) -->
	{varset(Vars-Goals0,   Vars1)},
	{varset(NUVars-Goals0, NUVars1)},
	expand_meta_arg(Meta, iparams(Locks, Pending, NUVars1, Vars1,
		Level, M), Goal0, Goal),
	{varset(Vars-Goal0,   Vars2)},
	{varset(NUVars-Goal0, NUVars2)},
	expand_metas(Metas, iparams(Locks, Pending, NUVars2, Vars2, Level, M),
	    Goals0, Goals).

expand_meta_args(Meta, Params, Goal1, Goal) -->
	{
	    compound_struct(Meta,  _, Metas),
	    compound_struct(Goal1, _, Goals1),
	    compound_struct(Goal,  _, Goals)},
	expand_metas(Metas, Params, Goals1, Goals).

expand_meta_arg(addmodule(Meta), Params, Goal0, Goal) -->
	!,
	expand_meta_arg(Meta, Params, Goal0, Goal).
expand_meta_arg(addterm(Meta), Params, Goal0, Goal) -->
	!,
	expand_meta_arg(Meta, Params, Goal0, Goal).
expand_meta_arg(goal, Params, Goal0, Goal) -->
	{nonvar(Goal0)},
	!,
	body_expansion(Goal0, Params, Goal).
expand_meta_arg(clause, Params, Goal0, Goal) -->
	{nonvar(Goal0)},
	!,
	body_expansion(Goal0, Params, Goal).
expand_meta_arg(_, _, Goal, Goal) --> [].

fails_body(Body) :- var(Body), !, fail.
fails_body((Lit, _)) :- !, fails(Lit).
fails_body((Lit, Body)) :- !, succs(Lit), fails_body(Body).
fails_body(Lit) :- fails(Lit).

succs_cut(Body,         _) :- var(Body), !, fail.
succs_cut((Lit, Body0), Body) :- succs(Lit), !, succs_cut(Body0, Body).
succs_cut((Lit, Body),  Body) :- !, Lit == !.
succs_cut(!,            true).

select_applicable([],                         _,    []).
select_applicable([(Head1 :- Body0)|Clauses], Head, AClauses0) :-
	(
	    \+((Head1 = Head, \+ fails_body(Body0))) ->
	    AClauses0 = AClauses,
	    select_applicable(Clauses, Head, AClauses)
	;
	    succs_cut(Body0, Body),
	    % Body0 = (Cut, Body),
	    % Cut == !, % Very specific case: cut is first literal
	    (instance(Head, Head1) ; Clauses == []) ->
	    AClauses0 = [(Head1 :- Body)]
	;
	    Body = Body0,
	    AClauses0 = [(Head1 :- Body)|AClauses],
	    select_applicable(Clauses, Head, AClauses)
	).

:- export(partial_unify/5).

partial_unify(A, B, NUVars) -->
	{A = B, varset(NUVars, NUVars1), NUVars==NUVars1},
	!.
partial_unify(A, B, _) -->
	{var(A)},
	!,
	[A = B].
partial_unify(A, B, _) -->
	{var(B)},
	!,
	[B = A].
partial_unify(A, B, NUVars) -->
	{
	    functor(A, F, N),
	    functor(B, F, N)
	},
	!,
	partial_unify_arg(1, A, B, NUVars).

partial_unify_arg(N, A, B, NUVars) -->
	{
	    arg(N, A, ArgA),
	    arg(N, B, ArgB)
	},
	partial_unify(ArgA, ArgB, NUVars),
	!,
	{N1 is N + 1},
	partial_unify_arg(N1, A, B, NUVars).
partial_unify_arg(_, _, _, _) --> [].

get_lit_expansor(M, Lit, F, LitArity, Pred) :-
	functor(USpec, F, LitArity),
	unfold_db(USpec, M) ->
	meta_unfold_spec(USpec, M, Spec),
	lit_expansor_args(1, Spec, Lit, Pred)
    ;
	true.

normalize_spec_down(yes,  yes).
normalize_spec_down(down, yes).
normalize_spec_down(no,   no).

lit_expansor_arg(yes, Arg, Arg).
lit_expansor_arg(no,  _,   _).

lit_expansor_args(N, Specs, Lit, Pred) :-
	arg(N, Specs, Spec0),
	arg(N, Lit,   Arg),
	arg(N, Pred,  Param),
	normalize_spec_down(Spec0, Spec),
	lit_expansor_arg(Spec, Arg, Param),
	N1 is N + 1,
	!,
	lit_expansor_args(N1, Specs, Lit, Pred).
lit_expansor_args(_, _, _, _).

source_clause(Head, (Head :- Body), M) :-
	current_fact(source_clause_db(Head, Body, M)).

select_clauses(Head, Clauses, M) :-
	findall(Clause, source_clause(Head, Clause, M), Clauses).

get_most_specific_head([],                        _).
get_most_specific_head([(Head :- _Body)|Clauses], SHead) :-
	copy_term(Head, SHead0),
	get_most_specific_head2(Clauses, SHead0, SHead).

get_most_specific_head2([]) --> [].
get_most_specific_head2([(Head :- _Body)|Clauses]) -->
	most_specific_generalization(Head),
	get_most_specific_head2(Clauses).

gen_unfolder(iparams(_, _, NUVars, Vars0, _, M), Goal0, F,
	    LitArity, ClauseArity, SHead, Pred, Params) :-
	functor(Pred0, F, ClauseArity),
	(
	    functor(USpec, F, LitArity),
	    unfold_db(USpec, M) ->
	    meta_unfold_spec(USpec, M, Spec),
	    transform_args(1, Spec, Goal0, SHead, Pred0, NUVars, Vars,
		Vars1, Params1),
	    vars_of_unfold_args(1, Spec, Goal0, Vars1, Vars0),
	    varset(Vars, Vars2),
	    intersect_vars(Params1, Vars2, Params2),
	    collapse_dups(Params2, Params3)
	;
	    compound_struct(Pred0, _, Params3)
	),
	copy_term(Pred0-Params3, Pred-Params).

rename_head_clause((Head0 :- Body), Head1, Head2, (Head :- Body)) :-
	copy_term(Head1-Head2, Head3-Head),
	Head0 = Head3.

expand_goal(Goal0, Renamer, F, LitArity, ClauseArity, SC1, Key,
	    Params0, Head2, Head, Name, SC2) :-
	get_most_specific_head(SC1, SHead0),
	gen_unfolder(Params0, Goal0, F, LitArity, ClauseArity, SHead0,
	    Head2, Args2),
	compound_struct(HeadC2, F, Args2),
	(
	    Head2 == HeadC2 ->
	    Name = F,
	    Args = Args2
	;
	    Renamer = renamer(RHead0),
	    copy_term(RHead0-Goal0, RHead-Goal),
	    Goal = Head2,
	    RHead =.. [Name|Args] ->
	    true
	;
	    Name = Key,
	    Args = Args2
	),
	compound_struct(Head0, Name, Args),
	(
	    SC1 = [(_ :- SBody)], succs_(SBody) ->
	    SC2 = [],
	    Head = true
	;
	    map(SC1, rename_head_clause(Head2, Head0), SC2),
	    Head = Head0
	).

meta_expansion(Pred, Expansion, M) :-
	functor(Pred, F, A),
	meta_predicate(F, A, Meta, M),
	Meta \== 0 ->
	meta_expansion_args(1, Meta, Pred, M, Args, []),
	Expansion =.. [F|Args]
    ;
	Expansion = Pred.

meta_expansion_args(N, Meta, Pred, M) -->
	{arg(N, Meta, Spec)},
	{arg(N, Pred, Arg)},
	meta_expansion_arg(Spec, Arg, M),
	{N1 is N + 1},
	!,
	meta_expansion_args(N1, Meta, Pred, M).
meta_expansion_args(_, _, _, _) --> [].

meta_expansion_arg(addmodule(Spec), Arg, M) -->
	!,
	meta_expansion_arg(Spec, Arg, M), [M].
meta_expansion_arg(addterm(Spec), Arg, M) -->
	!,
	meta_expansion_arg(Spec, Arg, M), [Arg].
meta_expansion_arg(_, Arg, _) --> [Arg].

meta_unfold_spec(UfldSpec, M, MetaUfldSpec) :-
	functor(UfldSpec, F, A),
	meta_predicate(F, A, Meta, M),
	Meta \== 0 ->
	meta_unfold_spec_args(1, Meta, UfldSpec, M, Args, []),
	MetaUfldSpec =.. [F|Args]
    ;
	MetaUfldSpec = UfldSpec.

meta_unfold_spec_args(N, Meta, USpec, M) -->
	{arg(N, Meta,  Spec)},
	{arg(N, USpec, Arg)},
	meta_unfold_spec_arg(Spec, Arg, M),
	{N1 is N + 1},
	!,
	meta_unfold_spec_args(N1, Meta, USpec, M).
meta_unfold_spec_args(_, _, _, _) --> [].

meta_unfold_spec_arg(addmodule(Spec), Arg, M) -->
	!,
	meta_unfold_spec_arg(Spec, Arg, M), [Arg].
meta_unfold_spec_arg(addterm(Spec), Arg, M) -->
	!,
	meta_unfold_spec_arg(Spec, Arg, M), [Arg].
meta_unfold_spec_arg(_, Arg, _) --> [Arg].

lits_to_list(Body) --> {var(Body)}, !, [Body].
lits_to_list((Lit, Body)) --> !, lits_to_list(Lit), lits_to_list(Body).
lits_to_list(Lit) --> [Lit].

handle_cuts_([],    [], _, true) :- !.
handle_cuts_([!|L], R,  S, Body) :- !, handle_cuts_(L, R, S, Body).
handle_cuts_(L,     R,  S, Body) :- handle_cuts__(L, R, S, Body).

handle_cuts__([],    [], L,  Body) :- list_to_lits(L, Body).
handle_cuts__([!|R], [], L0, (Body0->Body1)) :- !,
	list_to_lits(L0, Body0),
	handle_cuts_(R, L, L, Body1).
handle_cuts__([Lit|R], [Lit|L0], L, Body) :-
	handle_cuts__(R, L0, L, Body).

:- export(handle_cuts/2).

:- test handle_cuts(A, B) : (A = (!)) => (B = true) + not_fails.
:- test handle_cuts(A, B) : (A = (a, !)) => (B = (a->true)) + not_fails.
:- test handle_cuts(A, B) : (A = (!, a)) => (B = a) + not_fails.
:- test handle_cuts(A, B) : (A = (a, b, c, !, d, e))
	=> (B = (a, b, c->d, e)) + not_fails.

handle_cuts(Body0, Body) :-
	lits_to_list(Body0, List, []),
	handle_cuts_(List, L, L, Body).

gsc_special_cases(F, LitArity, _Level, Params, Goal0, Goal) :-
	select_clauses(Goal0, SC0, M),
	select_applicable(SC0, Goal0, SC),
	Params = iparams(Locks, _Pending, NUVars, _Vars, Level0, M),
	(
	    SC = [(Head :- Body0)],
	    ( ( inline_db(F, LitArity, _, M) ->
		    retractall_fact(unused_inline_db(F, LitArity, M, _)) )
	    ; Level0 > 1 -> true ) ->
	    get_lit_expansor(M, Goal0, F, LitArity, Head),
	    add_nuvars_u(Goal0, Body0, NUVars, NUVarsB),
	    handle_cuts(Body0, Body),
	    partial_unify(Head, Goal0, NUVarsB, Remainings, [Body]),
	    list_to_lits(Remainings, Goal)
	    % body_expansion(Goal1, Params, Goal, _, _)
	;
	    SC = [] ->
	    Goal = fail,
	    ( member('$wgf$off', Locks) -> true
	    ; warning_goal_failed(Goal0)
	    )
	).

lit_expand(F, LitArity, ClauseArity, M, Goal0, Vars, Key, Head) :-
	functor(Head0, F, ClauseArity),
	get_lit_expansor(M, Goal0, F, LitArity, Head0),
	gen_new_key(Vars, Head0, M, F, LitArity, Key),
	copy_term(Head0, Head).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TO REFORMULATE, USING STATE VARIABLES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rename_goal(Key, Goal0, Goal, M) :- rename_goal_db(Key, Goal0, Goal, M).
record_rename_goal(Key, Head2, Head, M) :-
	assertz_fact(rename_goal_db(Key, Head2, Head, M)).
generated(Name, A, M) :- generated_db(Name, A, M).
record_generated(Name, A, M) :- assert_generated(Name, A, M).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_specialized_clauses(UGoal0, Goal, Renamer, Params,
	    Clauses0, Clauses) :-
	Params = iparams(Locks, Pending, _NUVars, Vars, Level0, M),
	functor(UGoal0, F, LitArity),
	meta_expansion(UGoal0, Goal0, M),
	functor(Goal0, F, ClauseArity),
	Level is Level0 + 1,
	(
	    functor(Head5, F, ClauseArity),
	    \+ source_clause_db(Head5, _, M) ->
	    Goal = UGoal0,
	    Clauses0 = Clauses
	;
	    member(F/LitArity, Locks) ->
	    Goal = UGoal0,
	    Clauses0 = Clauses
	;
	    gsc_special_cases(F, LitArity, Level, Params, Goal0, Goal) ->
	    Clauses0 = Clauses
	;
	    lit_expand(F, LitArity, ClauseArity, M, Goal0, Vars, Key, Head0),
	    (
		member(Key, Locks) ->
		freeze_rename_goal(Key, Pending, Goal0, Goal, M),
		Clauses0 = Clauses
	    ;
		rename_goal(Key, Goal0, Goal, M) ->
		Clauses0 = Clauses
	    ;
		do_generate_specialized_clauses(Goal0, Goal, F, LitArity,
		    ClauseArity, M, Head0, Renamer, Key, Locks, Pending, Level,
		    Params, Clauses0, Clauses)
	    )
	),
	!.
generate_specialized_clauses(Goal, Goal, _, _) --> [],
	% We should never reach this line, all empty cases were considered before:
	{warning_gsc_failed(Goal)}.

do_generate_specialized_clauses(Goal0, Goal, F, LitArity, ClauseArity, M, Head0,
	    Renamer, Key, Locks, Pending, Level, Params, Clauses0, Clauses) :-
	select_clauses(Head0, SC0, M),
	expand_clauses(SC0, iparams([Key|Locks], Pending, [], _, Level, M),
	    SC1, [], Clauses1, Clauses2),
	expand_goal(Goal0, Renamer, F, LitArity, ClauseArity, SC1, Key, Params,
	    Head2, Head, Name, SC20),
	select_applicable(SC20, Head, SC2),
	(member(Key/Freezed, Pending) -> true),
	functor(Head, _, A),
	record_rename_goal(Key, Head2, Head, M),
	(
	    generated(Name, A, M), Renamer == no ->
	    Goal0 = Head2, Head = Goal, Freezed = Key, Clauses = Clauses0
	;
	    record_generated(Name, A, M),
	    Goal0 = Head2, Head = Goal, Freezed = Key,
	    (
		F/ClauseArity == Name/A ->
		(
		    \+ inline_db(F, LitArity, srclocal, M),
		    use_module_db(Base, Alias, List, M),
		    nonvar(List),
		    member(F/LitArity, List) ->
		    Clauses0 = Clauses1, Clauses2 = Clauses
		    % Clauses = Clauses0
		;
		    inline_module_db(Base, Alias, List, M),
		    member(F/LitArity, List) ->
		    process_use_module(use_module(Alias, [F/LitArity]),
			combine_usage_info, Decls, M),
		    Clauses0 = Clauses1,
		    put(Decls, Clauses2, Clauses)
		    % put(Decls, Clauses0, Clauses)
		;
		    Clauses0 = Clauses1,
		    head_decls(Name, A, M, Clauses2, Clauses3),
		    put(SC2, Clauses3, Clauses)
		)
	    ;
		Clauses0 = Clauses1,
		head_decls(Name, A, M, Clauses2, Clauses3),
		put(SC2, Clauses3, Clauses)
	    )
	).

warning_gsc_failed(Goal) :-
	location(Loc),
	warning_message(Loc, "generate_specialized_clauses(~w) failed",
	    [Goal]).

warning_goal_failed(PredName) :-
	location(Loc),
	warning_message(Loc, "~w always fails", [PredName]).

warning_cl_goal_exp(SC2) :-
	location(Loc),
	warning_message(Loc,
	    "The next clauses can not be inserted, because we are in a goal "
	    || "expansion: ~w", [SC2]).

freeze_rename_goal(Key, Pending, Goal0, Goal, M) :-
	(member(Key/X, Pending) -> true ; true),
	freeze(X, ( rename_goal(Key, Goal0, Goal, M) -> true; Goal0 =
		Goal )).

put(SC2, SC3, Tail) :-
	append(SC2, Tail, SC3).

% putt(AC0, AC1, AC0, AC1).

head_decls(F, A, M) -->
	{meta_pred_db(F, A, PredSpec, M)} ->
	[(:- meta_predicate(PredSpec))]
    ;
	[].

expand_clauses([],                   _,      Clauses,  Clauses) --> [].
expand_clauses([(Head :- Body0)|SC], Params, Clauses0, Clauses) -->
	{Params = iparams(Locks, Pending, NUVars, _, Level, M)},
	{varset(Head, Vars)},
	body_expansion(Body0, iparams(Locks, Pending, NUVars, Vars, Level, M),
	    Body),
	{fails(Body) -> Clauses0 = Clauses1
	; Clauses0 = [(Head :- Body)|Clauses1]},
	expand_clauses(SC, Params, Clauses1, Clauses).

cleanup_db(M) :-
	retractall_fact(source_clause_db(_, _, M)),
	retractall_fact(rename_goal_db(_, _, _, M)),
	% retractall_fact(inline_goal_db(_, _, _, M)),
	retractall_fact(inline_db(_, _, _, M)),
	retractall_fact(unused_inline_db(_, _, M, _)),
	retractall_fact(renamer_db(_, _, M)),
	retractall_fact(unused_renamer_db(_, _, M, _)),
	retractall_fact(no_slice_db(_, _, M)),
	retractall_fact(generated_db(_, _, M)),
	retractall_fact(meta_pred_db(_, _, _, M)),
	retractall_fact(num_term_db(_, M, _)),
	retractall_fact(unfold_meta_db(M)),
	retractall_fact(unfold_db(_, M)),
	retractall_fact(use_module_db(_, _, _, M)),
	retractall_fact(in_inline_module_db(_, M)),
	retractall_fact(inline_module_db(_, _, _, M)).

% :- pred compound_struct(Pred, F, Args) :: (term(Pred), atm(F), list(Args)).

/*
compound_struct(Pred, F, Args) :-
	Pred =.. [F|Args].
*/

compound_struct(Pred, F, Args) :-
	var(Pred),
	var(F),
	!,
	compound_struct_error(Pred, F, Args).
compound_struct(Pred, F, Args) :-
	Pred =.. [F|Args].

compound_struct_error(Pred, F, Args) :-
	location(Loc),
	!,
	error_message(Loc, " When executing ~w", [Pred =.. [F|Args]]),
	fail.
