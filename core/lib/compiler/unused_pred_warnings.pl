:- module(unused_pred_warnings, [flat_meta_spec/2, unused_pred_warnings/2,
		assert_upw_assrt/2, assert_upw_pred/2, assert_upw_decl/4,
		record_imports_dependencies/2, cleanup_upw_db/1, upw_assrt/2,
		record_assrt_dependency/6, record_pred_dependency/5,
		record_pred_dependency/4],
	    [assertions, nativeprops, dcg, define_flag]).

:- use_module(library(lists)).
:- use_module(library(hiordlib)).
:- use_module(library(llists)).
:- use_module(library(aggregates)).
:- use_module(library(sort)).
:- use_module(library(terms_vars)).
%:- use_module(library(formulae)).
:- use_module(engine(meta_inc)).
:- use_module(library(compiler/c_itf_internal), [imports_pred/7, exports/5,
		base_name/2, defines_module/2, meta_args/2]).
:- use_module(library(iso_misc), [compound/1]).

:- doc(author, "Edison Mera").

:- doc(module, "This module allows detecting unused modules and
	    predicates.").

:- data head_location/3.
:- data head_decl_db/2.
:- data import_location/5.
:- data import_all_location/3.
:- data dependency/4.
:- data variable_goal/5.
:- data meta_pred_db/4.

define_flag(unused_pred_warnings, [yes, no], no).

mark_meta_head(0,    _) :- !.
mark_meta_head(Meta, Head) :-
	mark_meta_head_args(1, Meta, Head).

mark_meta_head_args(N, Meta, Head) :-
	compound(Meta),
	arg(N, Meta, Spec),
	arg(N, Head, Arg),
	!,
	mark_meta_arg(Spec, Arg),
	N1 is N + 1,
	mark_meta_head_args(N1, Meta, Head).
mark_meta_head_args(_, _, _).

mark_meta_arg(goal, Arg) :-
	!,
	varset(Arg, Vars),
	list(Vars, '='(goal)).
mark_meta_arg(pred(N), Arg) :-
	!,
	varset(Arg, Vars),
	list(Vars, '='(pred(N))).
mark_meta_arg(fact, Arg) :-
	!,
	varset(Arg, Vars),
	list(Vars, '='(fact)).
mark_meta_arg(spec, Arg) :-
	!,
	varset(Arg, Vars),
	list(Vars, '='(spec)).
mark_meta_arg(list(A), Arg) :-
	nonvar(Arg),
	Arg = [H|T],
	!,
	varset(H, VarsH),
	list(VarsH, '='(A)),
	mark_meta_arg(list(A), T).
mark_meta_arg(list(A), Arg) :-
	!,
	varset(Arg, Vars),
	list(Vars, '='(list(A))).
mark_meta_arg(_, _).

record_imports(all, File, _Base, Loc) :-
	!,
	base_name(File, BFile),
	defines_module(BFile, Module),
	assertz_fact(import_all_location(Module, File, Loc)).
record_imports(Imports, File, Base, Loc) :-
	list(Imports, record_import(Base, File, Loc)).

ignore_import(hiord_rt, '$meta_call', 1).

record_import(F/A, Base, File, Loc) :-
	( imports_pred(Base, File, F, A, _, _, EndFile),
	    EndFile \= '.' -> true
	; EndFile = File
	),
	base_name(EndFile, BFile),
	defines_module(BFile, Module),
	\+ ignore_import(Module, F, A),
	assertz_fact(import_location(Module, F, A, File, Loc)).


record_imports_dependencies(Base, M) :-
	atom(Base),
	retract_fact(upw_decl(Imports, File, Base, M, Loc)),
	record_imports(Imports, File, Base, Loc),
	fail.
record_imports_dependencies(_, _).

record_pred_dependency(CRef, M, H, B, Loc) :-
	atom(M),
	retract_fact(upw_pred(CRef, M)) ->
	record_pred_dependency(M, H, B, Loc)
    ;
	true.

record_pred_dependency(M, H, B, Loc) :-
	functor(H, FH, AH),
	record_head_location(FH, AH, Loc),
	\+ \+((
		do_mark_meta(M, H, FH, AH, B),
		record_body_location(H, FH, AH, M, Loc),
		record_body_location(B, FH, AH, M, Loc)
	    )).

% Don't generate warnings for variable meta arguments in literals
% that comes from the head. Kludge: instantiate such variables:
do_mark_meta(M, H, FH, AH, B) :-
	get_meta_pred(M, H, FH, _, AH, Meta),
	mark_meta_head(Meta, H),
	mark_meta_body(B, M).

record_head_location(FH, AH, Loc) :-
	( head_location(FH, AH, _) -> true
	; assertz_fact(head_location(FH, AH, Loc))
	).

record_head_decl(F, A) :-
	( head_decl_db(F, A) -> true
	; assertz_fact(head_decl_db(F, A))
	).

record_body_location(B, FH, AH, M, Loc) :-
	(
	    get_literal(B, M, Loc, L),
	    nonvar(L),
	    callable(L),
	    functor(L, FL, AL),
	    FL/AL \== FH/AH,
	    ( dependency(FH, AH, FL, AL) -> true
	    ; assertz_fact(dependency(FH, AH, FL, AL))
	    ),
	    fail
	;
	    true
	),
	!.

:- data upw_assrt/2.

assert_upw_assrt(Ref, M) :-
	assertz_fact(upw_assrt(Ref, M)).

:- data upw_pred/2.

assert_upw_pred(Ref, M) :-
	assertz_fact(upw_pred(Ref, M)).

:- data upw_decl/5.

assert_upw_decl(use_module(File), Base, M, Loc) :-
	File \== user,
	!,
	assertz_fact(upw_decl(all, File, Base, M, Loc)).
assert_upw_decl(use_module(File, Imports), Base, M, Loc) :-
	File \== user,
	!,
	assertz_fact(upw_decl(Imports, File, Base, M, Loc)).
assert_upw_decl(_, _, _, _).

:- pred record_assrt_dependency/6 + no_choicepoints.
record_assrt_dependency(M, Type, Defined, Loc, H, Props) :-
	functor(H, FH, AH),
	( (Defined == yes ; Type==decl) ->
	    record_head_location(FH, AH, Loc)
	; true ),
	( ( Type == decl ; Type == entry
	    ; current_prolog_flag(unused_pred_warnings, yes) ) ->
	    record_head_decl(FH, AH)
	; true ),
	\+ \+ ( (
		get_meta_pred(M, H, FH, _, AH, Meta),
		mark_meta_head(Meta, H),
		list(Props,       list(mark_meta_body(M))),
		list([[H]|Props], list(record_body_location(FH, AH, M, Loc)))
	    ) ),
	!.

unused_pred_warnings(_,    user(_)) :- !.
unused_pred_warnings(Base, Module) :-
	% show_exports,
	% show_meta,
	all_messages(Base, Module, Messages0, []),
	sort(Messages0, Messages1),
	reverse(Messages1, Messages),
	messages(Messages).

all_messages(Base, M) -->
	dup_all_imports_messages(Base),
	dup_imports_messages,
	{verify_exps(Base, M)},
	exps_messages(M).

exps_messages(M, Messages0, Messages) :-
	variable_goal_min_arity(MinArity),
	unused_messages(MinArity, Messages0, Messages1),
	(
	    Messages0 == Messages1 ->
	    Messages1 = Messages
	;
	    ( head_location(_, A, _)
	    ; import_location(_, _, A, _, _)
	    ),
	    nonvar(A),
	    A >= MinArity ->
	    variable_goal_messages(M, Messages1, Messages)
	;
	    Messages1 = Messages
	).

unused_import_messages(MinArity) -->
	findall(Message,
	    (
		import_location(_, _, A, File, Loc),
		text_arity(MinArity, A, Text),
		unused_import_message(MinArity, File, Loc, Text, Message)
	    )).

unused_import_message(MinArity, File, Loc, Text, Message) :-
	findall(F/A,
	    (
		import_location(_, F, A, File, Loc),
		text_arity(MinArity, A, Text)
	    ), Preds),
	Loc = loc(Source, Ln0, Ln1),
	Message = message_lns(Source, Ln0, Ln1, warning,
	    ['Predicates ', ~~(Preds), ' imported from ', File, ' ', Text]).

unused_messages(MinArity) -->
	unused_import_messages(MinArity),
	unused_messages_(MinArity).

unused_messages_(MinArity) -->
	findall(
	    message_lns(Source, Ln0, Ln1, warning, [Term, ' ', Text]),
	    (
		head_location(F, A, loc(Source, Ln0, Ln1)),
		Term = [](['Predicate ', ~~(F/A)]),
		text_arity(MinArity, A, Text)
	    ;
		import_all_location(_, File, loc(Source, Ln0, Ln1)),
		Term = [](['Module ', File]),
		text_arity(MinArity, 256, Text) % 256=Max arity in Ciao
	    )
	).

text_arity(MinArity, Arity, Text) :-
	(var(Arity) ; Arity < MinArity) -> Text = 'may be unused'
    ;
	Text = 'may be unused (could be used in a meta-argument)'.

cleanup_upw_db(M) :-
	retractall_fact(head_location(_, _, _)),
	retractall_fact(import_location(_, _, _, _, _)),
	retractall_fact(import_all_location(_, _, _)),
	retractall_fact(dependency(_, _, _, _)),
	retractall_fact(variable_goal(_, _, _, _, _)),
	retractall_fact(head_decl_db(_, _)),
	retractall_fact(meta_pred_db(_, _, _, _)),
	retractall_fact(upw_assrt(_, M)),
	retractall_fact(upw_pred(_, M)),
	retractall_fact(upw_decl(_, _, _, M, _)).

variable_goal_messages(M) -->
	findall(message_lns(Source, Ln0, Ln1, warning, [~~(M:F/A),
		    ' argument ', N, ' is a variable meta-argument of type ',
		    PredType, '.']),
	    variable_goal(PredType, loc(Source, Ln0, Ln1), F, A, N)).

variable_goal_min_arity(MinArity) :-
	findall(PredType, variable_goal(PredType, _, _, _, _),
	    PredTypes),
	map(PredTypes, min_meta_arity, 0.Inf, MinArity).

min_meta_arity(PredType, MinArity0, MinArity) :-
	meta_arity(PredType, MinArity1),
	(
	    MinArity0 > MinArity1 ->
	    MinArity = MinArity1
	;
	    MinArity = MinArity0
	).

meta_arity(goal,            0).
meta_arity(clause,          0).
meta_arity(fact,            0.Inf).
meta_arity(spec,            0.Inf).
meta_arity(pred(Arity),     Arity).
meta_arity(list(Meta),      Arity) :- meta_arity(Meta, Arity).
meta_arity(addterm(Meta),   Arity) :- meta_arity(Meta, Arity).
meta_arity(addmodule(Meta), Arity) :- meta_arity(Meta, Arity).

is_being_used(Func, _, _) :- var(Func), !, fail.
is_being_used(Func, _, _) :-
	atom_concat('multifile:', _, Func),
	!.
is_being_used(Func, Arity, Base) :-
	defines_module(_, M),
	atom(M),
	atom_concat(M,   F0, Func),
	atom_concat(':', F,  F0),
	exports(Base, F, Arity0, _, Meta),
	( meta_inc_args(Meta, Arity0, Arity)
	; Arity0 = Arity
	),
	!.
is_being_used(Func, Arity, _) :-
	head_decl_db(Func, Arity),
	!.
% is_being_used(Func, Arity, _, _) :-
% 	variable_goal(fact, _, _, _, _),
% 	is_data_pred(Func, Arity),
% 	!.
% is_being_used(Func, Arity, _, _) :-
% 	is_regtype(Func, Arity),
% 	!.

% is_data_pred(_Func, _Arity) :-
% 	fail.

% show_exports :-
% 	exports(Base, Func0, Arity, T, Meta),
% 	display(exports(Base, Func0, Arity, T, Meta)),
% 	nl,
% 	fail.
% show_exports.

% show_meta :-
% 	meta_pred(Base, F, Arity, Meta),
% 	message(['*** ', meta_pred(Base, F, Arity, Meta)]),
% 	fail.
% show_meta.

verify_exps(Base, M) :-
	current_fact(head_location(FH, AH, _Loc), Ref),
	is_being_used(FH, AH, Base),
	erase(Ref),
	verify_dep(Base, M, FH, AH),
	fail.
verify_exps(_, _).


dup_imports_messages(Messages0, Messages) :-
	findall(Messages1, current_dup_imports(Messages1),
	    Messages2, [Messages]),
	append(Messages2, Messages0).

current_dup_imports([Message|Messages]) :-
	findall(il(ML, F, A), import_location(ML, F, A, _, _), ILs0),
	sort(ILs0, ILs),
	member(il(ML, F, A), ILs),
	(import_location(ML, F, A, File0, Loc0) -> true ; true),
	findall(Message,
	    (
		current_fact(import_location(ML, F, A, File1, Loc), Ref),
		Loc \= Loc0,
		erase(Ref),
		base_name(File1, BFile1),
		defines_module(BFile1, M),
		dup_message(Loc, M, F, A, File0, Message)
	    ), Messages),
	Messages = [_|_],
	base_name(File0, BFile0),
	defines_module(BFile0, M0),
	dup_message_first(Loc0, M0, F, A, Message).

dup_message_first(loc(Src, Ln0, Ln1), M, F, A,
	    message_lns(Src, Ln0, Ln1, warning, ['Predicate ', ~~(M:F/A),
		    ' imported several times, first time here'])).

dup_message(loc(Src, Ln0, Ln1), M, F, A, File,
	    message_lns(Src, Ln0, Ln1, warning, ['Predicate ', ~~(M:F/A),
		    ' imported several times, first from ', File])).

dup_all_imports_messages(Base, Messages0, Messages) :-
	findall(Messages1, current_dup_all_imports(Base, Messages1),
	    Messages2, [Messages]),
	append(Messages2, Messages0).

current_dup_all_imports(Base, [Message|Messages0]) :-
	findall(ML, import_all_location(ML, _, _), ILs0),
	sort(ILs0, ILs),
	member(ML, ILs),
	(import_all_location(ML, File0, Loc0) -> true ; true),
	get_dup_all_messages(ML, Loc0, Messages0, Messages1),
	(Messages0 == Messages1 -> DupAll = no ; DupAll = yes),
	get_already_imported_messages(Base, ML, File0, Messages1),
	( Messages1 == [] -> AlreadyImplemented = no
	; AlreadyImplemented = yes ),
	Messages0 = [_|_],
	msg_txt(DupAll, AlreadyImplemented, Text),
	dup_all_message_first(Loc0, File0, Text, Message).

get_dup_all_messages(ML, Loc0, Messages0, Messages1) :-
	findall(Message,
	    (
		current_fact(import_all_location(ML, File, Loc), Ref),
		Loc \= Loc0,
		erase(Ref),
		dup_all_message(Loc, File, Message)
	    ), Messages0, Messages1).

get_already_imported_messages(Base, ML0, File, Messages1) :-
	findall(Message,
	    (
		current_fact(import_location(ML, F, A, File1, Loc), Ref),
		imports_pred(Base, File, F, A, _, _, EndFile),
		(
		    EndFile == '.' -> ML = ML0
		;
		    base_name(EndFile, BFile),
		    defines_module(BFile, ML)
		),
		erase(Ref),
		base_name(File1, BFile1),
		defines_module(BFile1, M),
		already_imported_message(Loc, M, F, A, File, Message)
	    ), Messages1).

already_imported_message(loc(Src, Ln0, Ln1), M, F, A, File,
	    message_lns(Src, Ln0, Ln1, warning, ['Predicate ', ~~(M:F/A),
		    ' already imported in a :- ', use_module(File),
		    ' declaration'])).

msg_txt(yes, yes, 'Module and predicates of ').
msg_txt(yes, no,  'Module ').
msg_txt(no,  yes, 'Predicates of ').

dup_all_message_first(loc(Src, Ln0, Ln1), File, Text,
	    message_lns(Src, Ln0, Ln1, warning, [Text, ~~(File),
		    ' imported several times, first time here'])).

dup_all_message(loc(Src, Ln0, Ln1), File,
	    message_lns(Src, Ln0, Ln1, warning, ['Module ', ~~(File),
		    ' imported several times'])).

verify_dep(Base, M, FH, AH) :-
	retract_fact(dependency(FH, AH, FL, AL)),
	(
	    retract_fact(head_location(FL, AL, _)),
	    verify_dep(Base, M, FL, AL)
	;
	    (AL1 = AL ; get_meta_pred(M, _L, FL, AL1, AL, _)),
	    current_fact(import_location(ML, F, AL1, _, _), Ref),
	    atom_concat(ML,  ':', ML0),
	    atom_concat(ML0, F,   FL),
	    erase(Ref)
	;
	    current_fact(import_all_location(ML0, File, _Loc), Ref),
	    (
		(AL1 = AL ; get_meta_pred(M, _L, FL, AL1, AL, _)),
		imports_pred(Base, File, F, AL1, _, _, EndFile),
		(
		    EndFile == '.' -> ML = ML0
		;
		    base_name(EndFile, BFile),
		    defines_module(BFile, ML)
		),
		atom_concat(ML,  ':', ML1),
		atom_concat(ML1, F,   FL) -> true
	    ),
	    erase(Ref)
	),
	fail.
verify_dep(_, _, _, _).

get_meta_pred(_M, 'hiord_rt:call'(_P, A), 'hiord:rt:call', 2, 2,
	    'hiord_rt:call'(pred(N), ?)) :-
	!,
	(var(A) -> N = 0 ; functor(A, _, N)).
get_meta_pred(_M, _Pred, Func, Arity0, Arity, Meta) :-
	meta_pred_db(Func, Arity0, Arity, Meta),
	!.
get_meta_pred(M, _Pred, Func, Arity0, Arity, Meta) :-
	(
	    ( atom_concat('multifile:', F, Func),
		( var(Arity0) ->
		    meta_args(M, Meta0),
		    Meta0 \== 0,
		    functor(Meta0, F, Arity0)
		;
		    functor(Meta0, F, Arity0),
		    meta_args(M, Meta0),
		    Meta0 \== 0
		)
	    ;
		(M = Module ; defines_module(_, Module)),
		atom(Module),
		atom_concat(Module,  ':', Module0),
		atom_concat(Module0, F,   Func),
		meta_args(Module, Meta0),
		functor(Meta0, F, Arity0)
	    ),
	    flat_meta_spec(Meta0, Meta),
	    functor(Meta, _, Arity) ->
	    true
	;
	    Meta = 0,
	    Arity0 = Arity
	),
	assertz_fact(meta_pred_db(Func, Arity0, Arity, Meta)).

flat_meta_spec(Func0, Func) :-
	Func0 =.. [F|Args0],
	map(Args0, flat_arg_spec, Args, []),
	Func =.. [F|Args].

flat_arg_spec(addmodule(Meta)) --> !, flat_arg_spec(Meta), [addmodule].
flat_arg_spec(addterm(Meta)) --> !, flat_arg_spec(Meta), [addterm].
flat_arg_spec(Arg) --> [Arg].

mark_meta_body(A,                                           _) :- var(A), !.
mark_meta_body('internals:rt_module_exp'(A, _, _, _, _, A), _) :- !. % Marked!
mark_meta_body('basiccontrol:,'(A, B),                      M) :-
	!,
	mark_meta_body(A, M),
	mark_meta_body(B, M).
mark_meta_body('basiccontrol:;'(A, B), M) :-
	!,
	mark_meta_body(A, M),
	mark_meta_body(B, M).
mark_meta_body('basiccontrol:->'(A, B), M) :-
	!,
	mark_meta_body(A, M),
	mark_meta_body(B, M).
mark_meta_body('basiccontrol:\\+'(A), M) :-
	!,
	mark_meta_body(A, M).
mark_meta_body(L, M) :-
	callable(L),
	functor(L, Func, Arity),
	get_meta_pred(M, L, Func, _, Arity, Meta),
	(
	    Meta == 0 ->
	    true
	;
	    mark_meta_body_args(1, L, M, Meta)
	).

mark_meta_body_args(N, Args, M, Metas) :-
	arg(N, Args,  Arg),
	arg(N, Metas, Spec),
	!,
	(
	    arg_spec(Spec, Arg, Goal),
	    mark_meta_body_spec(Spec, Goal, M) ->
	    true
	;
	    true
	),
	N1 is N + 1,
	mark_meta_body_args(N1, Args, M, Metas).
mark_meta_body_args(_, _, _, _).

mark_meta_body_spec(list(Spec), A, M) :-
	!,
	(
	    nonvar(A),
	    A = [H|T] ->
	    mark_meta_body_spec(Spec, H, M),
	    mark_meta_body_spec(list(Spec), T, M)
	;
	    mark_meta_body(A, M)
	).
mark_meta_body_spec(_, A, M) :-
	mark_meta_body(A, M).

% The next clauses are not required, but are here to improve performance:
get_literal(A,                                       _, _,   A) :- var(A), !.
get_literal('hiord_rt:call'(G, _),                   _, _,   G) :- var(G), !.
get_literal('hiord_rt:call'('PA'(_, Args, G), Args), M, Loc, C) :-
	% Warning: this unifies Args
	get_literal(G, M, Loc, C).
get_literal('basiccontrol:,'(A, B), M, Loc, C) :-
	!,
	(get_literal(A, M, Loc, C) ; get_literal(B, M, Loc, C)).
get_literal('basiccontrol:;'(A, B), M, Loc, C) :-
	!,
	(get_literal(A, M, Loc, C) ; get_literal(B, M, Loc, C)).
get_literal('basiccontrol:->'(A, B), M, Loc, C) :-
	!,
	(get_literal(A, M, Loc, C) ; get_literal(B, M, Loc, C)).
get_literal('basiccontrol:\\+'(A), M, Loc, B) :-
	!,
	get_literal(A, M, Loc, B).
get_literal(P, M, Loc, L) :-
	callable(P),
	get_lit_callable(P, M, Loc, L).

get_lit_callable(P, M, Loc, L) :-
	functor(P, F, A),
	get_meta_pred(M, P, F, _A0, A, Meta),
	Meta \== 0,
	get_literal_from_args(1, F, A, P, Meta, M, Loc, L).
get_lit_callable(P, _, _, P).

get_literal_from_args(N, F, A, P, Meta, M, Loc, L) :-
	arg(N, P, Arg),
	arg(N, Meta, S)
    ->
	(
	    get_literal_from_arg(S, Arg, F, A, N, M, Loc, L)
	;
	    N1 is N + 1,
	    get_literal_from_args(N1, F, A, P, Meta, M, Loc, L)
	)
    ;
	true.

arg_goal(A,       A) :- var(A), !.
arg_goal('$:'(A), A) :- !.
arg_goal(A,       A).

arg_pred(A, A) :- var(A), !.
arg_pred(A, G) :-
	A = '$:'('PA'(_, Args, A1)),
	gnd(Args) ->
	G = A1
    ;
	G = A.

arg_spec(goal,    A, G) :- arg_goal(A, G).
arg_spec(clause,  A, G) :- arg_goal(A, G).
arg_spec(fact,    A, G) :- arg_goal(A, G).
arg_spec(spec,    A, G) :- arg_goal(A, G).
arg_spec(pred(_), A, G) :- arg_pred(A, G).
arg_spec(list(_), A, A).

get_literal_from_arg(S, Arg, F, A, N, M, Loc, L) :-
	arg_spec(S, Arg, G),
	nonvar_goal_spec(S, G, S, F, A, N, Loc),
	get_literal_spec(S, G, F, A, N, M, Loc, L).

get_literal_spec(goal,    G, _, _, _, M, Loc, L) :- get_literal(G, M, Loc, L).
get_literal_spec(clause,  G, _, _, _, M, Loc, L) :- get_literal(G, M, Loc, L).
get_literal_spec(fact,    G, _, _, _, M, Loc, L) :- get_literal(G, M, Loc, L).
get_literal_spec(spec,    G, _, _, _, _, _,   L) :- G = F/N, functor(L, F, N).
get_literal_spec(pred(_), G, _, _, _, M, Loc, L) :- get_literal(G, M, Loc, L).
get_literal_spec(list(S), G, F, A, N, M, Loc, L) :-
	G = [H|T],
	( get_literal_from_arg(S, H, F, A, N, M, Loc, L)
	; get_literal_from_arg(list(S), T, F, A, N, M, Loc, L) ).

nonvar_goal(Goal, GoalType, Func, Arity, NArg, Loc) :-
	var(Goal) ->
	assertz_fact(variable_goal(GoalType, Loc, Func, Arity, NArg)),
	fail
    ;
	true.

nonvar_goal_spec(list(S), G, T, F, A, N, Loc) :- !,
	nonvar_goal_spec(S, G, T, F, A, N, Loc).
nonvar_goal_spec(S, G, T, F, A, N, Loc) :-
	req_record_variable_goal(S),
	nonvar_goal(G, T, F, A, N, Loc).

req_record_variable_goal(goal).
req_record_variable_goal(clause).
req_record_variable_goal(fact).
req_record_variable_goal(pred(_)).
req_record_variable_goal(spec).
