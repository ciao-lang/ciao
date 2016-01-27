:- module(build_foreign_interface,
	[build_foreign_interface/1,
	 rebuild_foreign_interface/1,
	 build_foreign_interface_explicit_decls/2,
	 rebuild_foreign_interface_explicit_decls/2,
	 do_interface/1
	],
	[assertions,
	 basicmodes,
	 dcg,
	 fsyntax
	]).

:- doc(title, "Foreign Language Interface Builder").

:- doc(module, "Low-level utilities for building foreign
interfaces.  End-users should not need to use them, as the Ciao Prolog
Compiler reads the user assertions and calls appropriately the
predicates in this module.").

:- doc(author, "Jose F. Morales").
:- doc(author, "Manuel Carro").

:- use_module(library(write_c)).
:- use_module(library(streams)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(lists)).
:- use_module(library(llists), [flatten/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(system), 
	[delete_file/1,system/2,modif_time0/2,file_exists/1,
         working_directory/2]).
:- use_module(library(messages), 
	[error_message/2,error_message/3,
	 warning_message/2,warning_message/3]).
:- use_module(library(assertions/assrt_lib), 
	[get_code_and_related_assertions/5,
	 cleanup_code_and_related_assertions/0,
	 assertion_read/9]).
:- use_module(library(foreign_compilation), 
	[compiler_and_opts/2,linker_and_opts/2]).
:- use_module(library(compiler/c_itf)).
:- use_module(library(compiler/engine_path), [get_engine_dir/2]).
:- use_module(library(ctrlcclean), [ctrlc_clean/1]).
:- use_module(library(errhandle)).  
:- use_module(engine(internals), [
	product_filename/3,
	find_pl_filename/4
	]).
:- use_module(library(pathnames), [path_splitext/3]).
%:- use_module(library(format), [format/3]).

% --------------------------------------------------------------------------- %

:- pred build_foreign_interface(in(File)) :: sourcename
 # "Reads assertions from @var{File}, generates the gluecode for the Ciao
    Prolog interface, compiles the foreign files and the gluecode file, and
    links everything in a shared object. Checks modification times to
    determine automatically which files must be generated/compiled/linked.".
build_foreign_interface(File) :-
	get_decls(File, Decls),
	build_foreign_interface_explicit_decls(File, Decls).

% --------------------------------------------------------------------------- %

:- pred rebuild_foreign_interface(in(File)) :: sourcename
 # "Like @pred{build_foreign_interface/1}, but it does not check the 
    modification time of any file.".
rebuild_foreign_interface(File) :-
	get_decls(File, Decls), 
	build_foreign_interface_explicit_decls_2(yes, File, Decls).

% --------------------------------------------------------------------------- %

:- pred build_foreign_interface_explicit_decls(in(File),in(Decls)) ::
	sourcename * list(term)
 # "Like @pred{build_foreign_interface/1}, but use declarations in @var{Decls}
    instead of reading the declarations from @var{File}.".
build_foreign_interface_explicit_decls(File, Decls) :-
	build_foreign_interface_explicit_decls_2(no, File, Decls).

% --------------------------------------------------------------------------- %

:- pred rebuild_foreign_interface_explicit_decls(in(File),in(Decls)) ::
	sourcename * list(term)
 # "Like @pred{build_foreign_interface_explicit_decls/1}, but it does not
    check the modification time of any file.".
rebuild_foreign_interface_explicit_decls(File, Decls) :-
	build_foreign_interface_explicit_decls_2(yes, File, Decls).

% --------------------------------------------------------------------------- %

build_foreign_interface_explicit_decls_2(Rebuild, File, Decls) :-
	( do_interface(Decls) ->
	    find_pl_filename(File, PlName, Base, Dir),
	    load_all_ttr(Decls),
	    gluecode(Rebuild, Base, PlName), 
	    compile_and_link(Rebuild, Dir, Base, Decls),
	    clean_all_ttr
	; true
	).

% -----------------------------------------------------------------------------

:- pred do_interface(in(Decls)) :: list(term) # "Given the declarations in
	@var{Decls}, this predicate succeeds if these declarations involve
        the creation of the foreign interface".

do_interface(Decls) :-
	contains1(Decls, use_foreign_library(_)), !.
do_interface(Decls) :-
	contains1(Decls, use_foreign_library(_, _)), !.
do_interface(Decls) :-
	contains1(Decls, use_foreign_source(_)), !.
do_interface(Decls) :-
	contains1(Decls, use_foreign_source(_, _)).

% -----------------------------------------------------------------------------

get_decls(File, Decls) :-
	find_pl_filename(File, PlName, Base, _),
        error_protect(ctrlc_clean(
		process_files_from(PlName, in, module, get_decls_2(Decls),  
                                   false, false, '='(Base)))).

get_decls_2(Base, Decls) :-
	findall(D, decl(Base, D), Decls).

% -----------------------------------------------------------------------------

gluecode(Rebuild, Base, PrologFile) :-
	CFile = ~product_filename(gluecode_c, Base),
	( Rebuild = no -> has_changed(PrologFile, CFile) ; true ), !,
	( Rebuild = yes -> delete_files([CFile]) ; true ), 
	gluecode_2(PrologFile, CFile).
gluecode(_, _, _).

gluecode_2(PrologFile, CFile) :-
	(Module, Assertions) = ~read_assertions(PrologFile),
%	debug_display_assertions(Assertions), 
	Predicates = ~get_foreign_predicates(Assertions), 
	( Predicates = [] ->
	    warning_message("no foreign predicate found in '~w'", [PrologFile])
	; true
	), 
	( gluecode_program(Module, Predicates, Program, []), write_c_to_file(CFile, Program, Module) ->
	    true
	; error_message("generation of the interface gluecode for Prolog file '~w' failed", [PrologFile]),
	  fail
	), !.
gluecode_2(_, CFile) :-
       	delete_files([CFile]), 
	fail.

%debug_display_assertions([]) :- !.
%debug_display_assertions([X|Xs]) :- format(user_error, "[assertion] ~w~n", [X]), debug_display_assertions(Xs).

write_c_to_file(CFile, Program, Module) :-
	open_output(CFile, Stream), 
	( write_c(Program, Module, 0, _) ->
	    close_output(Stream)
	; close_output(Stream),
	  fail
	).

% -----------------------------------------------------------------------------

has_changed(SourceFile, TargetFile) :-
	modif_time0(SourceFile, TS), 
	modif_time0(TargetFile, TT), 
	TS > TT.

% -----------------------------------------------------------------------------

delete_files([]) :- !.
delete_files([F|Fs]) :- delete_if_exists(F), delete_files(Fs).

delete_if_exists(F) :- ( file_exists(F) -> delete_file(F) ; true ).

% -----------------------------------------------------------------------------

read_assertions(PrologFile) := (Module, Assertions) :-
	cleanup_code_and_related_assertions, 
	get_code_and_related_assertions(PrologFile, Module, _, _, _),
	findall(X, read_assertions_2(Module, X), Assertions),
	cleanup_code_and_related_assertions.

read_assertions_2(Module) := assertion(Status, Body, VarNames, Loc) :-
	assertion_read(_, Module, Status, _, Body, VarNames, PrologFile, LB, LE), 
	Loc = loc(PrologFile, LB, LE).

% -----------------------------------------------------------------------------

:- data foreign_predicate_error/0.

get_foreign_predicates([X|Xs]) := [X1|~get_foreign_predicates(Xs)] :- X1 = ~try_assertion(X), !.
get_foreign_predicates([_|Xs]) := ~get_foreign_predicates(Xs) :- !.
get_foreign_predicates([]) := [] :- % Fails if error.
	\+ retract_fact(foreign_predicate_error).

try_assertion(assertion(Status, Body, VarNames, Loc)) := PredDescription :-
	Body = ::(Pr, =>(DP:CP, AP+GP#_)), 
	functor(Pr, PrologName, Arity), 
	PredName = PrologName/Arity, 
	Pr =.. [_|Arguments], 
	(Kind, ForeignName) = ~get_name_and_kind(Loc, PredName, GP),
	( Kind = foreign ->
	    PredDescription = ~get_foreign(Status, Loc, PredName, ForeignName, DP, CP, AP, GP, Arguments, VarNames)
        ; Kind = foreign_low ->
	    PredDescription = foreign_low(PredName, ForeignName)
	; fail
	).

get_name_and_kind(Loc, PredName, GP) := (Kind, ForeignName) :-
	PredName = PrologName/_, 
	( (ForeignName, Kind, GP1) = ~get_name_and_kind_2(PrologName, GP) ->
	    valid_foreign_name(Loc, PredName, ForeignName), 
	    no_more_foreign_name(Loc, PredName, GP1)
	; fail % ok, this is not a foreign predicate
	).

get_name_and_kind_2(DefaultName, GP) := (DefaultName, foreign, GP1) :- select(foreign(_), GP, GP1), !.
get_name_and_kind_2(DefaultName, GP) := (DefaultName, foreign_low, GP1) :- select(foreign_low(_), GP, GP1), !.
get_name_and_kind_2(_, GP) := (ForeignName, foreign, GP1) :- select(foreign(_, ForeignName), GP, GP1), !.
get_name_and_kind_2(_, GP) := (ForeignName, foreign_low, GP1) :- select(foreign_low(_, ForeignName), GP, GP1), !.

no_more_foreign_name(_, _, GP) :-
	\+ member(foreign(_), GP), 
	\+ member(foreign(_, _), GP), 
	\+ member(foreign_low(_), GP), 
	\+ member(foreign_low(_, _), GP), !.
no_more_foreign_name(Loc, PredName, _) :-
	error_message(Loc, "more than one foreign/1,  foreign/2,  foreign_low/1 or foreign_low/2 property in predicate ~w", [PredName]), 
	set_fact(foreign_predicate_error), 
	fail.

valid_foreign_name(_, _, Name) :- atom(Name),  !.
valid_foreign_name(Loc, PredName, _) :-
	error_message(Loc, "invalid foreign/foreign_low function name in predicate ~w", [PredName]), 
        set_fact(foreign_predicate_error), 
	fail.

get_foreign(Status, Loc, PredName, ForeignName, DP, CP, AP, GP, Arguments0, VarNames) := foreign(PredName, GluecodeName, ForeignName, Arguments, ResVar, NeedsCtx) :-
	check_assertions(Status, Loc, PredName, Arguments0, DP, CP, AP, GP, VarNames),
	PredName = PrologName/Arity,
	Arity1 is Arity - 1, 
	numbers_between(0, Arity1, Arguments0), 
	findall(X, (member(Y, GP), Y=size_of(_, A, B), X=size_of(A, B)), SizeLinks), 
	findall(X, (member(Y, GP), Y=do_not_free(_, X)), NoFreeVars),
	findall(X, (member(Y, GP), Y=ttr(_, A, B), X=ttr(A, B)), TTrs), 
	GluecodeName = ~atom_concat('gluecode_', PrologName), 
	Arguments = ~get_arguments(Arguments0, DP, CP, AP, TTrs, SizeLinks, NoFreeVars),
	( member(returns(_, ResVar0), GP) -> ResVar = [ResVar0], returns_in_output_argument(ResVar0, Arguments, Loc, PredName, VarNames) ; ResVar = [] ),
	( member(needs_ciao_ctx, GP) -> NeedsCtx = yes ; NeedsCtx = no ).

get_arguments([X|Xs], DP, CP, AP, TTrs, SizeLinks, NoFreeVars) := [~get_argument(X, DP, CP, AP, TTrs, SizeLinks, NoFreeVars)|~get_arguments(Xs, DP, CP, AP, TTrs, SizeLinks, NoFreeVars)] :- !.
get_arguments([], _, _, _, _, _, _) := [] :- !.

get_argument(N, DP, CP, AP, TTrs, SizeLinks, NoFreeVars) := arg(N, TTr, XN, NoFree) :-
	( member(ttr(N, TTr), TTrs) ->
	    true
	; D = ~get_prop(N, DP),
	  C = ~get_prop(N, CP),
	  A = ~get_prop(N, AP),
	  TTr = ~ttr_match(D, C, A)
	),
	XN = ~sizelink(TTr, N, SizeLinks),
	NoFree = ~nofree(N, NoFreeVars).

get_prop(X, Ps) := P :-
	( member(PX, Ps), arg(1, PX, X), functor(PX, P, 1) ->
	    true
	; P = term
	).

sizelink(TTr, N, SizeLinks) := compound(LengthN) :- _ = ~ttr_compound(TTr), !, contains1(SizeLinks, size_of(N, LengthN)).
sizelink(_, _, _) := single :- !.

nofree(N, NoFreeNs) := yes :- contains1(NoFreeNs, N), !.
nofree(_, _) := no :- !.

check_assertions(Status, Loc, PredName, Arguments, DP, CP, AP, GP, VarNames) :-
	check_all_arguments(Loc, PredName, Arguments, DP), 
	check_all_arguments(Loc, PredName, Arguments, CP), 
	check_all_arguments(Loc, PredName, Arguments, AP), 
	check_list_correctness(Loc, PredName, DP, GP, Arguments, VarNames),
	check_do_not_free_correctness(Loc, PredName, GP, Arguments), 
	check_status(Loc, PredName, Status), 
	check_returns(Loc, PredName, GP, Arguments).

check_all_arguments(_, _, _, []) :- !.
check_all_arguments(Loc, PredName, Arguments, [X|_]) :-
	X =.. [_, Y], 
	nocontainsx(Arguments, Y), 
	!, 
	error_message(Loc, "invalid argument name in predicate ~w", [PredName]), 
        set_fact(foreign_predicate_error), 
	fail.
check_all_arguments(Loc, PredName, Arguments, [_|Xs]) :-
	check_all_arguments(Loc, PredName, Arguments, Xs).

check_returns(Loc, PredName, GP, Arguments) :-
	select(returns(_, Argument), GP, GP0),  !, 
	valid_returns_argument(Loc, PredName, Arguments, Argument), 
	no_more_returns(Loc, PredName, GP0).
check_returns(_, _, _, _).

valid_returns_argument(Loc, PredName, Arguments, Argument) :-
	nocontainsx(Arguments, Argument), 
	error_message(Loc, "returns/2 with invalid argument in predicate ~w", [PredName]), 
	set_fact(foreign_predicate_error), 
	fail.
valid_returns_argument(_, _, _, _).

no_more_returns(_, _, GP) :-
	\+ member(returns(_, _), GP), 
	!.
no_more_returns(Loc, PredName, _) :-
	error_message(Loc, "more than one returns/2 property in predicate ~w", [PredName]), 
        set_fact(foreign_predicate_error), 
	fail.

returns_in_output_argument(ResN, Arguments, Loc, PredName, VarNames) :- member(arg(ResN, TTr, _, _), Arguments), !,
	( _ = ~ttr_ctype_res(TTr) ->
	    true
	; var_name(ResN, VarNames, VarName), 
	  error_message(Loc, "~w is not an output argument in predicate ~w", [VarName, PredName]), 
	  set_fact(foreign_predicate_error), 
	  fail
	).		      

one_list_for_each_size_of(Loc, PredName, DP, GP, Arguments) :-
	member(size_of(_, ListVar, SizeVar), GP), 
	\+ valid_size_of_property(Arguments, ListVar, SizeVar, DP), 
	!, 
	error_message(Loc, "invalid size_of property in predicate ~w", [PredName]), 
        set_fact(foreign_predicate_error), 
        fail.
one_list_for_each_size_of(_, _, _, _, _).

check_list_correctness(Loc, PredName, DP, GP, Arguments, VarNames) :-
	one_list_for_each_size_of(Loc, PredName, DP, GP, Arguments),
	one_size_of_for_each(Loc, PredName, DP, GP, VarNames).

is_c_list_prop(c_uint8_list(ListVar), c_int8, ListVar).
is_c_list_prop(c_int_list(ListVar), c_int, ListVar).
is_c_list_prop(c_double_list(ListVar), c_double, ListVar).

valid_size_of_property(Arguments, ListVar, SizeVar, DP) :-
	\+ nocontainsx(Arguments, ListVar), 
	\+ nocontainsx(Arguments, SizeVar), 
	( is_c_list_prop(ListProp, _CType, ListVar),
	  \+ nocontainsx(DP, ListProp) ->
	    true
	; fail
	),
	\+ nocontainsx(DP, c_size(SizeVar)).

one_size_of_for_each(Loc, PredName, DP, GP, VarNames) :-
	is_c_list_prop(ListProp, _CType, ListVar),
	member(ListProp, DP), 
	findall(Y, (member(size_of(_, Y, _), GP), Y==ListVar), S), 
	nonsingle(S), 
	!, 
	var_name(ListVar, VarNames, VarName), 
	error_message(Loc, "variable ~w in predicate ~w needs a (only one) size_of/3 property", [VarName, PredName]), 
        set_fact(foreign_predicate_error), 
	fail.
one_size_of_for_each(_, _, _, _, _).

var_name(Var, VarNames, Name) :-
	findall(N, (member(N=X, VarNames), X==Var), [Name]).

check_do_not_free_correctness(Loc, PredName, GP, Arguments) :-
	member(do_not_free(_, Var), GP), 
	nocontainsx(Arguments, Var), 
	!, 
	error_message(Loc, "invalid do_not_free/2 property in predicate ~w", [PredName]), 
	fail.
check_do_not_free_correctness(_, _, _, _).

numbers_between(A, B, []) :- A > B,  !.
numbers_between(A, B, [A|Ns]) :-
	A1 is A + 1, 
	numbers_between(A1, B, Ns).

/*
assign_types([], _) := [] :- !.
assign_types([N|Ns], CP) := [arg_type(Type, N)|~assign_types(Ns, CP)] :-
	( member(Prop, CP), Prop =.. [Type, N] ->
	    true
	; Type = term
	).

assign_modes([], _) := [] :- !.
assign_modes([N|Ns], InN) := [A|~assign_modes(Ns, InN)] :-
	( contains1(InN, N) ->
	    A = in(N)
	; A = out(N)
	).
*/

check_status(_, _, true) :- !.
check_status(_, _, trust) :- !.
check_status(Loc, PredName, _) :-
	warning_message(Loc, "assertions of predicate ~w cannot be checked (foreign)", [PredName]).

% -----------------------------------------------------------------------------

gluecode_program(Module, Predicates) -->
	imports, 
	foreign_predicates_interface(Predicates, Module), 
	init(Predicates, Module), 
	end(Predicates, Module).

% -----------------------------------------------------------------------------

:- data ttr_ctype_res/2.
:- data ttr_ctype_call/2.
:- data ttr_ctype_decl/2.
:- data ttr_check/2.
:- data ttr_exception/2.
:- data ttr_to_c/2.
:- data ttr_compound/2.
:- data ttr_call_cref/2.
:- data ttr_from_c/2.
:- data ttr_free/2.

load_ttr_defs([Decl|Decls]) :- !,
	( Decl = ttr_def(X, Ys) -> assert_ttr_def(Ys, X) ; true ), 
	load_ttr_defs(Decls).
load_ttr_defs([]) :- !.

assert_ttr_def([(Y = V)|Ys], X) :- !, assert_ttr_def_2(Y, X, V), assert_ttr_def(Ys, X).
assert_ttr_def([], _) :- !.

assert_ttr_def_2(ctype_res, X, V) :- !, asserta_fact(ttr_ctype_res(X, V)).
assert_ttr_def_2(ctype_call, X, V) :- !, asserta_fact(ttr_ctype_call(X, V)).
assert_ttr_def_2(ctype_decl, X, V) :- !, asserta_fact(ttr_ctype_decl(X, V)).
assert_ttr_def_2(check, X, V) :- !, asserta_fact(ttr_check(X, V)).
assert_ttr_def_2(exception, X, V) :- !, asserta_fact(ttr_exception(X, V)).
assert_ttr_def_2(to_c, X, V) :- !, asserta_fact(ttr_to_c(X, V)).
assert_ttr_def_2(compound, X, V) :- !, asserta_fact(ttr_compound(X, V)).
assert_ttr_def_2(call_cref, X, V) :- !, asserta_fact(ttr_call_cref(X, V)).
assert_ttr_def_2(from_c, X, V) :- !, asserta_fact(ttr_from_c(X, V)).
assert_ttr_def_2(free, X, V) :- !, asserta_fact(ttr_free(X, V)).

:- data ttr_match_0/4.

load_ttr_matchs([Decl|Decls]) :- !,
	( Decl = ttr_match(X, (D, C, A)) -> asserta_fact(ttr_match_0(D, C, A, X)) ; true ), 
	load_ttr_matchs(Decls).
load_ttr_matchs([]) :- !.

ttr_match(D, C, A) := TTr :- TTr = ~ttr_match_0(D, C, A), !.
ttr_match(_, _, _) := '$$any_term$$' :- !.

load_all_ttr(Decls) :-
	load_ttr_matchs(Decls),
	load_ttr_defs(Decls).

clean_all_ttr :-
	retractall_fact(ttr_match_0(_, _, _, _)),
	retractall_fact(ttr_ctype_res(_, _)),
	retractall_fact(ttr_ctype_call(_, _)),
	retractall_fact(ttr_ctype_decl(_, _)),
	retractall_fact(ttr_check(_, _)),
	retractall_fact(ttr_exception(_, _)),
	retractall_fact(ttr_to_c(_, _)),
	retractall_fact(ttr_compound(_, _)),
	retractall_fact(ttr_call_cref(_, _)),
	retractall_fact(ttr_from_c(_, _)),
	retractall_fact(ttr_free(_, _)).

% -----------------------------------------------------------------------------

% TODO: use eng_h_alias?
% TODO: rename ciao_gluecode.h to gluecode.h?
imports --> [include('ciao/ciao_gluecode.h'), format(new_line)].

% -----------------------------------------------------------------------------

include_base_dir := ~atom_concat([~ciao_c_headers_dir]).

% -----------------------------------------------------------------------------

foreign_predicates_interface([], _) --> !.
foreign_predicates_interface([P|Ps], Module) -->
	foreign_predicate_interface(P, Module),
	[format(new_line)],
	foreign_predicates_interface(Ps, Module).

% -----------------------------------------------------------------------------

foreign_predicate_interface(P, _Module) --> { P = foreign_low(_, NativeName) }, !,
	foreign_low_prototype(NativeName).
foreign_predicate_interface(P, Module) -->
	{ P = foreign(PredName, GluecodeName, ForeignName, Arguments, ResVar, NeedsCtx) }, !,
	foreign_prototype(ForeignName, Arguments, ResVar, NeedsCtx), 
	{ interface_function_body(PredName, Module, ForeignName, Arguments, ResVar, NeedsCtx, Body, []) },
	[GluecodeName:function([w:pointer(worker_t)], 'bool_t')#Body].

% -----------------------------------------------------------------------------

foreign_low_prototype(NativeName) --> [NativeName:function([pointer(worker_t)], 'bool_t')].

% -----------------------------------------------------------------------------

foreign_prototype(ForeignName, Arguments, ResVar, NeedsCtx) -->
	{ ResVar = [ResN], select(arg(ResN, ResTTr, _, _), Arguments, Arguments1) ->
	    ResCType = ~ttr_ctype_res(ResTTr)
	; ResCType = void, Arguments1 = Arguments
	},
	{ Args0 = ~foreign_prototype_args(Arguments1) },
	{ NeedsCtx = yes -> Args = [ctx:ciao_ctx|Args0] ; Args = Args0 },
	[ForeignName:function(Args, ResCType)].

foreign_prototype_args([]) := [] :- !.
foreign_prototype_args([A|As]) := [~foreign_prototype_arg(A)|~foreign_prototype_args(As)] :- !.

foreign_prototype_arg(arg(_, TTr, _, _)) := X :- X = ~ttr_ctype_call(TTr), !.
foreign_prototype_arg(_) := ciao_term.

% -----------------------------------------------------------------------------

:- use_module(engine(internals), [module_concat/3]).

interface_function_body(F/A, Module, ForeignName, Arguments, ResVar, NeedsCtx) -->
	% optim_comp: 'string' is required by the C code writer, and MF/A is given as PredName
	% [call('ERR__FUNCTOR', [string(~atom_codes(MF)), A])], 
	{ module_concat(Module, F, MF) },
	[call('ERR__FUNCTOR', [~atom_codes(MF), A])],
	% variable declaration 
	params_apply(Arguments, t_decl),
	params_apply(Arguments, v_decl),
	params_apply(Arguments, u_decl),
	% variable initialization
	[call('CiaoDeclCtx', [ctx])],
	[call('CiaoInitCtx', [ctx])],
	[call(ciao_frame_begin_s, [ctx])],
	params_apply(Arguments, ref),
	% prolog -> c 
	params_apply(~filter_single(Arguments), check),
	params_apply(~filter_compound(Arguments), check),
	params_apply(~filter_single(Arguments), to_c),
	params_apply(~filter_compound(Arguments), to_c),
	% c call
	do_call(ForeignName, Arguments, ResVar, NeedsCtx), 
	% c -> prolog
	params_apply(Arguments, from_c),
	params_apply(Arguments, free),
	params_apply(Arguments, unify),
	[call(ciao_frame_end_s, [ctx])],
	[return('TRUE')].

filter_single([]) := [] :- !.
filter_single([X|Xs]) := [X|~filter_single(Xs)] :- X = arg(_, _, compound(_), _), !.
filter_single([_|Xs]) := ~filter_single(Xs) :- !.

filter_compound([]) := [] :- !.
filter_compound([X|Xs]) := [X|~filter_compound(Xs)] :- X = arg(_, _, single, _), !.
filter_compound([_|Xs]) := ~filter_compound(Xs) :- !.

params_apply([], _) --> !.
params_apply([X|Xs], Action) --> param_apply(Action, X), params_apply(Xs, Action).

param_apply(t_decl, X) --> !, param_apply_t_decl(X).
param_apply(v_decl, X) --> !, param_apply_v_decl(X).
param_apply(u_decl, X) --> !, param_apply_u_decl(X).
param_apply(ref, X) --> !, param_apply_ref(X).
param_apply(check, X) --> !, param_apply_check(X).
param_apply(to_c, X) --> !, param_apply_to_c(X).
param_apply(from_c, X) --> !, param_apply_from_c(X).
param_apply(free, X) --> !, param_apply_free(X).
param_apply(unify, X) --> !, param_apply_unify(X).

param_apply_v_decl(arg(N, TTr, _, _)) --> { CType = ~ttr_ctype_decl(TTr) }, !,
	[(~c(N)):CType].
param_apply_v_decl(_) --> !.

param_apply_u_decl(arg(N, TTr, _, _)) --> { _ = ~ttr_from_c(TTr) }, !,
	[(~u(N)):ciao_term].
param_apply_u_decl(_) --> !.

param_apply_t_decl(arg(N, _, _, _)) --> !,
	[(~t(N)):ciao_term].

param_apply_ref(arg(N, _, _, _)) --> [~t(N) = call(ciao_ref, [ctx, ~x(N)])].

param_apply_check(X) --> { Check = ~check_code(X) }, !,
	[if(logical(\ Check), ~exception_code(X))]. 
param_apply_check(_) --> !.

check_code(arg(N, TTr, _, _)) := call(Check, [ctx, ~t(N)]) :- Check = ~ttr_check(TTr).

exception_code(arg(N, TTr, _, _)) := X :- X = ~exception_code_2(N, ~ttr_exception(TTr)), !.
exception_code(_) := return('FALSE') :- !.

exception_code_2(N, error_in_arg(Type)) := call('ERROR_IN_ARG', [~x(N), N + 1, Type]) :- !.
exception_code_2(_, usage_fault(Msg)) := call('USAGE_FAULT', [Msg]) :- !.

param_apply_to_c(arg(N, TTr, single, _)) --> { ToC = ~ttr_to_c(TTr) }, !,
	[~c(N) = call(ToC, [ctx, ~t(N)])].
param_apply_to_c(arg(N, TTr, compound(_), _)) --> { ToC = ~ttr_to_c(TTr) }, !,
	[~c(N) = call(ToC, [ctx, ~t(N)])].
param_apply_to_c(_) --> !.

param_apply_from_c(arg(N, TTr, XN, _)) --> { FromC = ~ttr_from_c(TTr) }, !,
	[~u(N) = ~from_c_code(FromC, N, XN)].
param_apply_from_c(_) --> !.

from_c_code('=', N, single) := ~c(N) :- !.
from_c_code(FromC, N, single) := call(FromC, [ctx, ~c(N)]) :- !.
from_c_code(FromC, N, compound(LengthN)) := call(FromC, [ctx, ~c(N), ~c(LengthN)]) :- !.

param_apply_free(arg(N, TTr, _, no)) --> { Free = ~ttr_free(TTr) }, !,
	[call(Free, [~c(N)])].
param_apply_free(_) --> !.

param_apply_unify(arg(N, TTr, _, _)) --> { _ = ~ttr_from_c(TTr) }, !,
	[if(logical(\ call(ciao_unify_s, [ctx, ~u(N), ~t(N)])), return('FALSE'))].
param_apply_unify(_) --> !.

do_call(ForeignName, Arguments, ResVar, NeedsCtx) -->
	{ ResVar = [ResN], select(arg(ResN, _, _, _), Arguments, Arguments1) -> true ; Arguments1 = Arguments },
	{ Args0 = ~call_args(Arguments1) },
	{ NeedsCtx = yes -> Args = [ctx|Args0] ; Args = Args0 },
	( { NeedsCtx = no } -> [call('CiaoSetImplicitCtx', [ctx])] ; [] ),
	{ ResVar = [N] -> Call = (~c(N) = call(ForeignName, Args)) ; Call = (call(ForeignName, Args)) },
	[call('GLUECODE_TRY', [Call])].

call_args([]) := [] :- !.
call_args([X|Xs]) := [~call_arg(X)|~call_args(Xs)] :- !.

call_arg(arg(N, TTr, _, _)) := address(~call_arg_v(N, TTr)) :- _ = ~ttr_call_cref(TTr), !.
call_arg(arg(N, TTr, _, _)) := ~call_arg_v(N, TTr) :- !.

call_arg_v(N, TTr) := ~t(N) :- \+ _ = ~ttr_ctype_decl(TTr), !.
call_arg_v(N, _) := ~c(N) :- !.

c(N) := identifier("c~d", [N]).
u(N) := identifier("u~d", [N]).
t(N) := identifier("t~d", [N]).
x(N) := call('X', [N]).

% -----------------------------------------------------------------------------

init(Predicates, Module) -->
	{ define_c_mod_predicates(Predicates, Body, []) },
	[identifier("~w_init", [Module]):
         function([module:pointer(char)], void)#Body, 
         format(new_line)].

define_c_mod_predicates([]) --> !.
define_c_mod_predicates([P|Ps]) -->
	{ P = foreign(PrologName/Arity, GluecodeName, _, _, _, _) ->
	    true
	; P = foreign_low(PrologName/Arity, GluecodeName)
	},
	{ atom_codes(PrologName, PrologNameString) },  
	[call(define_c_mod_predicate, [module, PrologNameString, Arity, GluecodeName])],
	define_c_mod_predicates(Ps).

% -----------------------------------------------------------------------------

end(Predicates, Module) -->
	{ undefine_c_mod_predicates(Predicates, Body, []) },
	[identifier("~w_end", [Module]):
	 function([module:pointer(char)], void)#Body,
	 format(new_line)].

undefine_c_mod_predicates([]) --> !.
undefine_c_mod_predicates([P|Ps]) -->
	{ P = foreign(PrologName/Arity, _, _, _, _, _) ->
	    true
	; P = foreign_low(PrologName/Arity, _) },
	{ atom_codes(PrologName, PrologNameString) },  
	[call(undefine_c_mod_predicate, [module, PrologNameString, Arity])],
	undefine_c_mod_predicates(Ps).

% -----------------------------------------------------------------------------

get_options(Decls, Option, Xs) :-
	OsArchDependantOption =.. [Option, ~get_platform, X], 
	findall(X, member(OsArchDependantOption, Decls), Xs0), 
	Xs0 = [_|_],  % If empty,  try the default options.
	flatten(Xs0, Xs),  !.
get_options(Decls, Option, Xs) :-
	DefaultOption =.. [Option, X], 
	findall(X, member(DefaultOption, Decls), Xs0), 
	flatten(Xs0, Xs),  !.

% -----------------------------------------------------------------------------

compile_and_link(Rebuild, Dir, Base, Decls) :-
	get_foreign_files(Dir, Base, Decls, CFiles, OFiles, SOFile, AFile),
	( Rebuild = yes ->
	    delete_files([SOFile, AFile|OFiles])
	; true
	),
%	format(user_error, "[trace-dir] ~w~n", [Dir]),
	compile_and_link_2(Dir, Decls, CFiles, OFiles, SOFile, AFile).

compile_and_link_2(Dir, Decls, CFiles, OFiles, SOFile, AFile) :-
	get_options(Decls, extra_compiler_opts, ExtraCompilerOpts), 
	compile_foreign(Dir, Decls, ExtraCompilerOpts, CFiles, OFiles), 
	get_options(Decls, extra_linker_opts, ExtraLinkerOpts), 
	get_options(Decls, use_foreign_library, Libs), 
	foreign_link_so(Dir, Decls, ExtraLinkerOpts, Libs, OFiles, SOFile), 
	foreign_link_a(OFiles, AFile),
	!.
compile_and_link_2(_, _, _, OFiles, SOFile, AFile) :-
	delete_files([SOFile, AFile|OFiles]), 
	fail.

% -----------------------------------------------------------------------------

get_foreign_files(Dir, Base, Decls, CFiles, OFiles, SOFile, AFile) :-
        working_directory(OldDir, Dir),
	( get_foreign_files__2(Dir, Base, Decls, CFiles, OFiles, SOFile, AFile) ->
	    Ok = yes
	; Ok = no
	),
        working_directory(_, OldDir),
	Ok = yes.

get_foreign_files__2(Dir, Base, Decls, FFiles, OFiles, SOFile, AFile) :-
	get_options(Decls, use_foreign_source, Files),
	absolute_base_names(Dir,Files, AbsFiles, AbsBases),
	GlueFile = ~product_filename(gluecode_c, Base),
	FFiles = [GlueFile|AbsFiles],
	OFile = ~product_filename(gluecode_o, Base),
	OSuffix = ~atom_concat(~atom_concat('_', ~get_platform), '.o'),
	append_suffix(AbsBases, OSuffix, OFiles0),
	OFiles = [OFile|OFiles0],
	SOFile = ~product_filename(gluecode_so, Base),
	AFile = ~product_filename(gluecode_a, Base).

% -----------------------------------------------------------------------------

absolute_base_names(_, [], [], []) :- !.
absolute_base_names(Dir, [File|Files], [AbsFile|AbsFiles], [AbsBase|AbsBases]) :-
	absolute_file_name(File, [], '.c', Dir,  AbsFile, AbsBase1, _),
	( AbsBase1 = AbsFile ->
	    pathnames:path_splitext(AbsFile, AbsBase, _)
	; AbsBase = AbsBase1
	),
	absolute_base_names(Dir, Files, AbsFiles, AbsBases).

% -----------------------------------------------------------------------------

append_suffix([], _, []) :- !.
append_suffix([A0|As0], Suffix, [A|As]) :-
	A = ~atom_concat(A0, Suffix), 
	append_suffix(As0, Suffix, As).

% -----------------------------------------------------------------------------

compile_foreign(Dir, Decls, ExtraOpts, CFiles, OFiles) :-
%	compiler_and_opts(Compiler, Opts),
        compiler_to_use(Decls, Compiler, Opts), 
	TotalOpts = ~append(Opts, ExtraOpts), 
	CommandHead = ~atom_concat_with_blanks([Compiler, '-c'|TotalOpts]), 
	compile_foreign_2(Dir, CommandHead, CFiles, OFiles).

compile_foreign_2(_, _, [], []) :- !.
compile_foreign_2(Dir, CommandHead, [CFile|CFiles], [OFile|OFiles]) :-
	( has_changed(CFile, OFile) ->
	    include_base_dir(BaseDir), Inc = ~atom_concat('-I', BaseDir),
	    Command = ~atom_concat_with_blanks(['cd ', Dir, ';', CommandHead,
	        Inc, '-o', OFile, CFile]),
%	    format(user_error, "[trace-cf] ~w~n", Command),
	    system(Command, 0)
	; true
	), 
	compile_foreign_2(Dir, CommandHead, CFiles, OFiles).

% -----------------------------------------------------------------------------

add_ciaolib_if_required(L0, L) :- eng_is_sharedlib, !,
	add_ciaolib(L0, L).
add_ciaolib_if_required(L, L).

add_ciaolib(L, ['-L', EngDir, LibOpt|L]) :-
	get_platform(TargetEng),
	get_engine_dir(TargetEng, EngDir),
	LibOpt = ~atom_concat('-l', 'ciaoengine'). % TODO: use config_common:default_eng/1 (but avoid dependency to that module)

foreign_link_so(Dir, Decls, ExtraOpts, Libs0, OFiles, SOFile) :-
	( member(OFile, OFiles), 
	  has_changed(OFile, SOFile) ->
          %  linker_and_opts(Linker, Opts), 
             linker_to_use(Decls, Linker, Opts),
	    Libs = ~add_ciaolib_if_required(~append_prefix(Libs0, '-l')),
	    % Note the order of linker options is important, in
            % pariticular for library archive. See the following links
            % for more informations:
            % https://gcc.gnu.org/onlinedocs/gcc/Link-Options.html
            % https://sourceware.org/binutils/docs-2.24/ld/Options.html#Options
	    %
	    % ExtraOpts is append at the end because it may contains libraries 
	    % (e.g -lgmp that should be resolve last). 
	    flatten([Opts, ['-o', SOFile|OFiles], Libs, ExtraOpts], Args),
	    !,
	    Command = ~atom_concat_with_blanks(['cd ', Dir, ';', Linker|Args]),
%	    format(user_error, "[trace-lf] ~w~n", Command),
	    system(Command, 0)
	; true
	).

foreign_link_a(OFiles, AFile) :-
	% TODO: Missing: output the library dependencies, for linking against this .a
	( member(OFile, OFiles), 
	  has_changed(OFile, AFile) ->
	    flatten(['-c', '-r', AFile, OFiles], Args), !,
	    atom_concat_with_blanks(['ar'|Args], Command),
%	    format(user_error, "[trace - ar] ~w~n", Command),
	    system(Command, 0)
	; true
	).

% -----------------------------------------------------------------------------

append_prefix([], _) := [] :- !.
append_prefix([A|As], Prefix) := [~atom_concat(Prefix, A)|~append_prefix(As, Prefix)] :- !.

% -----------------------------------------------------------------------------

atom_concat_with_blanks(L) := ~atom_concat(~separate_with_blanks(L)).

separate_with_blanks([]) := [] :- !.
separate_with_blanks([A]) := [A] :- !.
separate_with_blanks([A, B|Cs]) := [A, ' '|~separate_with_blanks([B|Cs])] :- !.

% -----------------------------------------------------------------------------

% If there is a per-file compiler declaration, this overrides the
% default compiler.  As we do not know which are the right options for
% this compiler, we let the user choose.

compiler_to_use(Decls, Compiler, Opts):-
        get_options(Decls, use_compiler, NewCompiler),
        (
            NewCompiler = [] ->
            compiler_and_opts(Compiler, Opts)
        ;
            NewCompiler = [Compiler],
            Opts = []
        ).

linker_to_use(Decls, Linker, Opts):-
        get_options(Decls, use_linker, NewLinker),
        (
            NewLinker = [] ->
            linker_and_opts(Linker, Opts)
        ;
            NewLinker = [Linker],
            Opts = []
        ).
