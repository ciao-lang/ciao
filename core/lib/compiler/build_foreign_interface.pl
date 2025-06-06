:- module(build_foreign_interface,
    [build_foreign_interface/1,
     rebuild_foreign_interface/1,
     build_foreign_interface_explicit_decls/2,
     rebuild_foreign_interface_explicit_decls/2,
     do_interface/1
    ],
    [assertions,
     basicmodes,
     hiord,
     dcg,
     fsyntax,
     datafacts]).

% TODO: split into action__archcompile and the frontend

:- doc(title, "Foreign language interface builder").

:- doc(module, "Low-level utilities for building C foreign interfaces.
   End-users should not need to use them, as the Ciao compiler reads
   the user assertions and calls appropriately the predicates in this
   module.").

:- doc(author, "Jose F. Morales").
:- doc(author, "Manuel Carro").

:- use_module(library(aggregates), [findall/3]).

:- use_module(library(write_c)).
:- use_module(library(lists)).
:- use_module(library(llists), [flatten/2]).
:- use_module(engine(stream_basic)).
:- use_module(engine(system_info),
    [get_platform/1, ciao_c_headers_dir/1, eng_is_sharedlib/0]).
:- use_module(library(system), [modif_time0/2]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(messages), 
    [error_message/2,error_message/3,
     warning_message/2,warning_message/3]).
:- use_module(library(assertions/assrt_lib), 
    [get_code_and_related_assertions/5,
     cleanup_code_and_related_assertions/0,
     assertion_read/9]).
:- use_module(library(foreign_compilation), 
    [compiler_and_opts/2,linker_and_opts/2]).
:- use_module(library(compiler/c_itf), [process_files_from/7, decl/2, false/1]).
:- use_module(library(compiler/engine_path), [get_engine_dir/2]).
:- use_module(library(compiler/file_buffer)).
:- use_module(library(ctrlcclean), [ctrlc_clean/1]).
:- use_module(library(errhandle), [error_protect/2]).  
:- use_module(engine(internals), [
    product_filename/3,
    find_pl_filename/4
    ]).
:- use_module(library(pathnames), [path_splitext/3]).
:- use_module(library(format), [format/3]). % (for tracing)

:- use_module(library(compiler/foreign__gluecode)).

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
        gluecode(Decls, Rebuild, Base, PlName), 
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
%do_interface(Decls) :- % (secondary)
%    contains1(Decls, use_foreign_gluecode_header(_)), !.

% -----------------------------------------------------------------------------

% TODO: try to process decls without collecting them in a list (which is slow and breaks indexing)
get_decls(File, Decls) :-
    find_pl_filename(File, PlName, Base, _),
    error_protect(ctrlc_clean(
            process_files_from(PlName, in, module, get_decls_2(Decls),  
                               c_itf:false, c_itf:false, '='(Base))
        ), fail). % TODO: fail or abort?

get_decls_2(Decls, Base) :-
    findall(D, decl(Base, D), Decls).

% -----------------------------------------------------------------------------

gluecode(Decls, Rebuild, Base, PrologFile) :-
    CFile = ~product_filename(gluecode_c, Base),
    ( Rebuild = no -> has_changed(PrologFile, CFile) ; true ), !,
    ( Rebuild = yes -> del_files_nofail([CFile]) ; true ), 
    gluecode_2(Decls, PrologFile, CFile).
gluecode(_, _, _, _).

gluecode_2(Decls, PrologFile, CFile) :-
    (Module, Assertions) = ~read_assertions(PrologFile),
%       debug_display_assertions(Assertions), 
    Predicates = ~get_foreign_predicates(Assertions), 
    ( Predicates = [] ->
        warning_message("no foreign predicate found in '~w'", [PrologFile])
    ; true
    ), 
    get_options(Decls, use_foreign_gluecode_header, GlueHs0), 
    GlueHs = ['ciao/ciao_gluecode.h'|GlueHs0], % TODO: rename ciao_gluecode.h to gluecode.h?
    ( gluecode_program(Module, GlueHs, Predicates, Program, []),
      write_c_to_file(CFile, Program, Module) ->
        true
    ; error_message("generation of the interface gluecode for Prolog file '~w' failed", [PrologFile]),
      fail
    ), !.
gluecode_2(_, _, CFile) :-
    del_files_nofail([CFile]), 
    fail.

%debug_display_assertions([]) :- !.
%debug_display_assertions([X|Xs]) :- format(user_error, "[assertion] ~w~n", [X]), debug_display_assertions(Xs).

write_c_to_file(CFile, Program, Module) :-
    file_buffer_begin(CFile, Buffer, Stream),
    current_output(CO),
    set_output(Stream),
    ( write_c(Program, Module, 0, _) -> OK = yes ; OK = no ),
    set_output(CO),
    ( OK = yes ->
        ( file_buffer_commit(Buffer) ->
            true
        ; fail % TODO: warning?
        )
    ; file_buffer_erase(Buffer),
      fail  % TODO: handle errors?
    ).

% -----------------------------------------------------------------------------

has_changed(SourceFile, TargetFile) :-
    modif_time0(SourceFile, TS), 
    modif_time0(TargetFile, TT), 
    TS > TT.

% -----------------------------------------------------------------------------

% TODO: duplicated
del_files_nofail([]).
del_files_nofail([File|Files]) :-
    del_file_nofail(File),
    del_files_nofail(Files).

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
    % TODO:[xsyntax] new :: and => priorities
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

is_c_list_prop(c_uint8_list(ListVar), c_uint8, ListVar).
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

gluecode_program(Module, GlueHs, Predicates) -->
    includes(GlueHs), 
    [format(new_line)],
    foreign_predicates_interface(Predicates, Module), 
    init(Predicates, Module), 
    end(Predicates, Module).

% -----------------------------------------------------------------------------

% TODO: use eng_h_alias?
includes([]) --> [].
includes([local(H)|Hs]) --> !, [local_include(H)], includes(Hs).
includes([H|Hs]) --> !, [include(H)], includes(Hs).

% -----------------------------------------------------------------------------

include_base_dir := ~ciao_c_headers_dir.

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
        del_files_nofail([SOFile, AFile|OFiles])
    ; true
    ),
    % format(user_error, "[trace-dir] ~w~n", [Dir]),
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
    del_files_nofail([SOFile, AFile|OFiles]), 
    fail.

% -----------------------------------------------------------------------------

:- use_module(library(system), [working_directory/2]).

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
    absolute_base_names(Dir, Files, '.c', AbsFiles, AbsBases),
    GlueFile = ~product_filename(gluecode_c, Base),
    FFiles = [GlueFile|AbsFiles],
    OFile = ~product_filename(gluecode_o, Base),
    append_osuffix(AbsBases, OFiles0),
    OFiles = [OFile|OFiles0],
    SOFile = ~product_filename(gluecode_so, Base),
    AFile = ~product_filename(gluecode_a, Base).

% -----------------------------------------------------------------------------

absolute_base_names(_, [], _Suff, [], []) :- !.
absolute_base_names(Dir, [File|Files], Suff, [AbsFile|AbsFiles], [AbsBase|AbsBases]) :-
    absolute_file_name(File, [], Suff, Dir,  AbsFile, AbsBase1, _),
    ( AbsBase1 = AbsFile ->
        pathnames:path_splitext(AbsFile, AbsBase, _)
    ; AbsBase = AbsBase1
    ),
    absolute_base_names(Dir, Files, Suff, AbsFiles, AbsBases).

% -----------------------------------------------------------------------------

% Get c_object from bases 
append_osuffix([], []) :- !.
append_osuffix([Base|Bases], [OFile|OFiles]) :-
    OFile = ~product_filename(c_object, Base),
    append_osuffix(Bases, OFiles).

% -----------------------------------------------------------------------------

compile_foreign(Dir, Decls, ExtraOpts, CFiles, OFiles) :-
    compiler_to_use(Decls, Compiler, CompOpts), 
    Opts = ~append(CompOpts, Opts1), 
    ciao_c_headers_dir(CiaoHDir),
    Opts1 = [~atom_concat('-I', CiaoHDir)|Opts2],
    ( member(use_foreign_gluecode_header(_), Decls) ->
        % TODO: Needed to locate header when .c is auto-generated (e.g., gluecode)
        Opts2 = [~atom_concat('-I', Dir)|Opts3]
    ; Opts2 = Opts3
    ),
    Opts3 = ExtraOpts,
    compile_foreign_2(Dir, Compiler, Opts, CFiles, OFiles).

compile_foreign_2(_, _, _, [], []) :- !.
compile_foreign_2(Dir, Compiler, Opts, [CFile|CFiles], [OFile|OFiles]) :-
    ( has_changed(CFile, OFile) ->
        flatten(['-c', Opts, '-o', OFile, CFile], Args), !,
        % format(user_error, "[trace-cf] ~w ~w~n", [Compiler, Args]),
        process_call(path(Compiler), Args, [cwd(Dir), status(0)])
    ; true
    ), 
    compile_foreign_2(Dir, Compiler, Opts, CFiles, OFiles).

% -----------------------------------------------------------------------------

add_libciaoengine_if_required(L0, L) :- eng_is_sharedlib, !,
    add_libciaoengine(L0, L).
add_libciaoengine_if_required(L, L).

add_libciaoengine(L, ['-L', EngDir, LibOpt|L]) :-
    get_platform(TargetEng),
    get_engine_dir(TargetEng, EngDir),
    LibOpt = ~atom_concat('-l', 'ciaoengine'). % TODO: use config_common:default_eng/1 (but avoid dependency to that module)

foreign_link_so(Dir, Decls, ExtraOpts, Libs0, OFiles, SOFile) :-
    ( member(OFile, OFiles), 
      has_changed(OFile, SOFile) ->
        linker_to_use(Decls, Linker, Opts),
        Libs = ~add_libciaoengine_if_required(~append_prefix(Libs0, '-l')),
        % Note the order of linker options is important, in
        % pariticular for library archive. See the following links
        % for more informations:
        % https://gcc.gnu.org/onlinedocs/gcc/Link-Options.html
        % https://sourceware.org/binutils/docs-2.24/ld/Options.html#Options
        %
        % ExtraOpts is append at the end because it may contains libraries 
        % (e.g -lgmp that should be resolve last). 
        flatten([Opts, ['-o', SOFile|OFiles], Libs, ExtraOpts], Args), !,
        % format(user_error, "[trace-lf] ~w ~w~n", [Linker, Args]),
        process_call(path(Linker), Args, [cwd(Dir), status(0)])
    ; true
    ).

foreign_link_a(OFiles, AFile) :-
    % TODO: Missing: output the library dependencies, for linking against this .a
    ( member(OFile, OFiles), 
      has_changed(OFile, AFile) ->
        flatten(['-c', '-r', AFile, OFiles], Args), !,
        % format(user_error, "[trace-ar] ~w ~w~n", ['ar', Args]),
        process_call(path('ar'), Args, [status(0)])
    ; true
    ).

% -----------------------------------------------------------------------------

append_prefix([], _) := [] :- !.
append_prefix([A|As], Prefix) := [~atom_concat(Prefix, A)|~append_prefix(As, Prefix)] :- !.

% -----------------------------------------------------------------------------

% If there is a per-file compiler declaration, this overrides the
% default compiler.  As we do not know which are the right options for
% this compiler, we let the user choose.

compiler_to_use(Decls, Compiler, Opts):-
    get_options(Decls, use_compiler, NewCompiler),
    ( NewCompiler = [] ->
        compiler_and_opts(Compiler, Opts)
    ; NewCompiler = [Compiler],
      Opts = []
    ).

linker_to_use(Decls, Linker, Opts):-
    get_options(Decls, use_linker, NewLinker),
    ( NewLinker = [] ->
        linker_and_opts(Linker, Opts)
    ; NewLinker = [Linker],
      Opts = []
    ).

