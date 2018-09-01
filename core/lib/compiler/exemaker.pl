:- module(exemaker,
	    [make_exec/2,
	     force_lazy/1,
	     undo_force_lazy/1,
	     dynamic_search_path/1],
	    [assertions, define_flag, datafacts]).

:- use_module(library(aggregates), [findall/3]).

:- use_module(library(compiler/c_itf_internal),
	    [handle_exc/1,
	     process_file/7,
	     process_files_from/7,
	     false/1,
	     make_po_file/1,
	     base_name/2,
	     file_data/3,
	     module_error/1,
	     defines_module/2,
	     cleanup_c_itf_data/0,
	     processed/2,
	     exports/5,
	     imports_pred/7,
	     def_multifile/4,
	     decl/2]).
:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic)).
:- use_module(engine(messages_basic), [message/2]).
:- use_module(engine(runtime_control), [current_prolog_flag/2]).
:- use_module(engine(meta_inc), [meta_inc_args/3]).
:- use_module(engine(internals),
	    [po_filename/2,
		so_filename/2,
		itf_filename/2]).
:- use_module(library(stream_utils), [open_input/2, close_input/1]).
:- use_module(library(system),
	    [file_exists/1,
		modif_time0/2,
		mktemp/2,
		winpath/2,
		delete_file/1,
		set_exec_mode/2]).
:- use_module(engine(internals), [ciao_root/1]).
:- use_module(engine(system_info), [get_platform/1, get_os/1]).
:- use_module(library(compiler/engine_path), [get_engine_file/2]).
:- use_module(library(compiler/pl2wam)).
:- use_module(library(lists), [member/2]).

:- use_module(library(ctrlcclean), [delete_on_ctrlc/2]).
:- use_module(engine(internals),   [module_concat/3]).
:- use_module(library(foreign_interface/build_foreign_interface)). % JFMC
:- use_module(library(compiler/compressed_bytecode)). % OPA

% Extension for ciao executables in Win32
:- include(win_exec_ext).

define_flag(executables, [static, eagerload, lazyload], eagerload).
define_flag(check_libraries, [on, off], off).
% (See eng_defs:eng_cfg/2 for possible values for self_contained)
define_flag(self_contained, atom, none).
define_flag(compress_exec, [yes, no], no).

:- data ok_lazy/1.

force_lazy(Module) :- asserta_fact(ok_lazy(Module)).

undo_force_lazy(Module) :- retractall_fact(ok_lazy(Module)).

make_exec(Files, ExecName) :-
	catch(make_exec_prot(Files, ExecName), Error, handle_exc(Error)).

make_exec_prot(Files, ExecName) :-
	process_files_from(Files, po, any,
	    treat_file, stopOnlib, skipOnlib, redo_po),
	\+ current_fact(module_error(_)),
	Files = [MainFile|_],
	base_name(MainFile, Base),
	defines_module(Base, Module),
	compute_main_def(Module, Base, MainDef),
	current_prolog_flag(executables, ExecMode),
	compute_objects_loads(ExecMode, ExecFiles, InitLoads),
	create_init(Module, ExecMode, MainDef, InitLoads, InitFile),
	create_exec(ExecName, Base, [InitFile|ExecFiles]),
	create_interfaces, % JFMC
	!,
	delete_temp,
	cleanup_c_itf_data.
make_exec_prot(_, _) :-
	message(user, '{Executable generation aborted}'),
	retractall_fact(needs_interface(_, _)), % JFMC
	cleanup_c_itf_data.

treat_file(Base) :-
	treat_so_lib(Base), % JFMC
	make_po_file(Base).

% Base or Base related files will not be treated if:
%   Base is in /lib and not a module required for boot (see engine(internals))
%   'check_libraries' flag is off
%   'executables' flag is not static (dynamic or lazy)
stopOnlib(Base) :-
	current_prolog_flag(check_libraries, off),
	current_prolog_flag(executables,     Mode), Mode \== static,
	in_lib_not_boot(Base).

% Base (not related files) will not be treated if:
%   Base is in /lib
%   'check_libraries' flag is off
skipOnlib(Base) :-
	current_prolog_flag(check_libraries, off),
	in_lib_or_boot(Base),
	% TODO: collect in a different way
	so_filename(Base, SoName),
	(file_exists(SoName) -> assertz_fact(has_so_file(Base)) ; true).

% ---------------------------------------------------------------------------
% Classification of modules
%
%   lib ---> under /core/lib
%   boot --> needed for booting the system (see engine(internals))
%            Currently everything under engine(_) and some defined by
%            boot_module/1

% TODO: Obtain automatically (e.g., from dependencies of engine(internals))
boot_module(datafacts_rt).
% boot_module(pathnames).
% boot_module(lists).

in_lib_not_boot(Base) :-
	ciao_root(CiaoRoot),
	atom_concat(CiaoRoot, Name, Base),
	( atom_concat('/core/lib', _, Name) -> true % In lib/ or library/
	; fail
	),
	\+ boot_base(Base).

in_lib_or_boot(Base) :-
	boot_base(Base),
	!.
in_lib_or_boot(Base) :-
	ciao_root(CiaoRoot),
	atom_concat(CiaoRoot, Name, Base),
	( atom_concat('/core/lib', _, Name) -> true % In lib/ or library/
	; atom_concat('/core/engine/', _, Name) -> true % In engine/
	; fail
	).

boot_base(Base) :-
	defines_module(Base, Module),
	boot_module(Module),
	!.
boot_base(Base) :-
	ciao_root(CiaoRoot),
	atom_concat(CiaoRoot, Name, Base),
	( atom_concat('/core/engine/', _, Name) -> true % In engine/
	; fail
	).

% ---------------------------------------------------------------------------

redo_po(Base) :-
	po_filename(Base, PoName),
	modif_time0(PoName, PoTime),
	itf_filename(Base, ItfName),
	modif_time0(ItfName, ItfTime),
	PoTime < ItfTime.

:- export(needs_interface/2).
:- data has_so_file/1, needs_interface/2. % JFMC

treat_so_lib(Base) :-
	( findall(X, decl(Base, X), Decls),
	    do_interface(Decls) -> % JFMC
	    assertz_fact(needs_interface(Base, Decls)),
	    assertz_fact(has_so_file(Base))
	; so_filename(Base, SoName),
	    file_exists(SoName) ->
	    assertz_fact(has_so_file(Base))
	; true
	).

compute_main_def(user(_), _,    void) :- !.
compute_main_def(Module,  Base, clause(UserMain, MainGoal)) :-
	(Arity = 0 ; Arity = 1),
	exports(Base, main, Arity, _, _), !,
	functor(Main, main, Arity),
	module_concat(user,   Main, UserMain),
	module_concat(Module, Main, ModMain),
% 	(current_prolog_flag(runtime_checks, no) ->
	MainGoal = ModMain.
% 	;
% 	    module_concat(rtchecks_utils, call_rtc(ModMain), MainGoal)

% 	    module_concat(rtchecks_utils, handle_rtcheck(E),          Handler ),
% 	    module_concat(exceptions,     catch(ModMain, E, Handler), MainGoal)
% 	).
compute_main_def(Module, _, _) :-
	message(error, ['module ', Module,
		' should export main/0 or main/1']), fail.

%%% --- Creating foreign interfaces - JFMC --- %%%

:- data interface_creation_error/0.

:- export(create_interfaces/0).
create_interfaces :-
	retract_fact(needs_interface(Base, Decls)),
	( build_foreign_interface_explicit_decls(Base, Decls) -> true
	; set_fact(interface_creation_error)
	),
	fail.
create_interfaces :-
	\+ retract_fact(interface_creation_error).

%%% --- Computing load type for each module --- %%%

compute_objects_loads(ExecMode, ExecFiles, InitLoads) :-
	findall(Base, processed(Base, po), Bases),
	compute_load_types(ExecMode, Bases),
	compute_exec_data(Bases, ExecFiles, InitLoads).

:- data load_type/2. % BASE should be loaded as TYPE, one of static,
% dynamic, eager or lazy (dynamic is used if eagerload).

compute_load_types(_,      _) :- retractall_fact(load_type(_, _)), fail.
compute_load_types(static, _) :-
	assertz_fact(load_type(_Base, static)).
compute_load_types(eagerload, _) :-
	(dynamic_search_path(_) -> Dyn_type = eager ; Dyn_type = dynamic),
	( processed(Base, po),
	    sta_or_dyn_type(Base, Dyn_type),
	    fail
	; true
	).
compute_load_types(lazyload, Bases) :-
	retractall_fact(requires_file1(_, _)),
	compute_load_deps(Bases),
	sta_eager_lazy(Bases).

base_is_sta(Base) :-
	boot_base(Base).

file_is_dyn(library(_)).

:- data dynamic_search_path/1.

sta_or_dyn_type(Base, Dyn_type) :-
	base_name(File, Base), !,
	( base_is_sta(Base) ->
	    Type = static
	; file_is_dyn(File) ->
	    Type = Dyn_type
	; dynamic_search_path(Fun), functor(File, Fun, _) ->
	    Type = Dyn_type
	; Type = static
	),
	asserta_fact(load_type(Base, Type)).

:- data requires_file1/2. % MODULE1 needs MODULE2 to be loaded
% (maybe transitively)

compute_load_deps([]).
compute_load_deps([Base|Bases]) :-
	base_name(File, Base), !,
	( base_is_sta(Base) -> asserta_fact(load_type(Base, static))
	; file_is_dyn(File) -> true
	; dynamic_search_path(Fun), functor(File, Fun, _) -> true
	; asserta_fact(load_type(Base, static))
	),
	compute_required(Base),
	compute_load_deps(Bases).

compute_required(Base) :-
	requires_file(Base, Base1),
	defines_module(Base1, M1),
	\+ ok_lazy(M1),
	add_requires_file1(Base, Base1),
	fail.
compute_required(_).

requires_file(B, B1) :-
	member(Dyn, [dynamic, data, concurrent]),
	imports_pred(B, IF, _F, _A, Dyn, _, EF),
	(EF = '.' -> base_name(IF, B1) ; base_name(EF, B1)).
requires_file(B, B1) :-
	def_multifile(B,  F, A, _),
	def_multifile(B1, F, A, _),
	B \== B1.

add_requires_file1(B0, B1) :- % Keep relation transitive
	(I = B0 ; requires_file1(I,  B0)),
	(O = B1 ; requires_file1(B1, O)),
	I \== O,
	\+ current_fact(requires_file1(I, O)),
	asserta_fact(requires_file1(I, O)),
	fail.
add_requires_file1(_, _).

sta_eager_lazy([]).
sta_eager_lazy([B|Bs]) :-
	( current_fact(load_type(B, static)) -> true
	; requires_file1(B0, B), load_type(B0, static) ->
	    defines_module(B, M),
	    message(note, ['module ', M, ' will be loaded eagerly.']),
	    asserta_fact(load_type(B, eager))
	; asserta_fact(load_type(B, lazy))
	),
	sta_eager_lazy(Bs).


%%% --- Compiling changed files, computing info to make executable --- %%%

compute_exec_data([],           [],   'basiccontrol:fail').
compute_exec_data([Base|Bases], ExFs, Lds) :- % both .so and .po - JFMC
	defines_module(Base, Module),
	load_type(Base, LdType),
	po_filename(Base, PoName),
	( really_has_so_file(Base) ->
	    ( LdType = static ->
		base_name(File, Base),
		ExFs = [PoName|ExFs_], Lds = 'basiccontrol:,'(
		    'internals:load_so'(Module, Base), Lds_)
	    ; LdType = dynamic ->
		ExFs = ExFs_, Lds = Lds_
	    ; LdType = eager ->
		base_name(File, Base),
		ExFs = ExFs_, Lds = 'basiccontrol:,'('internals:load_so'(
			Module, Base), 'basiccontrol:,'('internals:load_po'(
			    File), Lds_))
	    ; LdType = lazy ->
		make_lo(Module, Base, LoName),
		ExFs = [LoName|ExFs_], Lds = Lds_
	    )
	; ( LdType = static ->
		ExFs = [PoName|ExFs_], Lds = Lds_
	    ; LdType = dynamic ->
		ExFs = ExFs_, Lds = Lds_
	    ; LdType = eager ->
		base_name(File, Base),
		ExFs = ExFs_, Lds = 'basiccontrol:,'('internals:load_po'(File),
		    Lds_)
	    ; LdType = lazy ->
		make_lo(Module, Base, LoName),
		ExFs = [LoName|ExFs_], Lds = Lds_
	    )
	), !,
	compute_exec_data(Bases, ExFs_, Lds_).

really_has_so_file(Base) :-
	has_so_file(Base), !.
really_has_so_file(Base) :-
	so_filename(Base, SoName),
	file_exists(SoName).

%%% --- Making lazyload files --- %%%

make_lo(Module, Base, LoFile) :-
	base_name(File, Base), !,
	verbose_message(['{Making lazyloader file for ', Module]),
	compute_required_loads(Base, Loads0, Pred),
	Loads = ('internals:load_lib_lazy'(Module, File), Loads0),
	temp_filename(LoFile),
	delete_on_ctrlc(LoFile, Ref),
	open(LoFile, write, Out),
	Mode = ql(unprofiled),
	reset_counter(Module),
	set_compiler_mode_out(Mode, Out),
	compile_stumps(Base, Module, Loads, Pred),
	cleanup_compilation_data,
	close(Out),
	erase(Ref),
	verbose_message('}').

compute_required_loads(B0, Loads, Pred) :-
	retract_fact(requires_file1(B0, B)), !,
	defines_module(B, M),
	base_name(File, B), !,
	Loads = 'basiccontrol:,'('internals:load_lib_lazy'(M, File), Loads_),
	compute_required_loads(B0, Loads_, Pred).
compute_required_loads(_B0, Pred, Pred). % Incomplete structure

compile_stumps(Base, Module, Loads, Pred) :-
	define_stump_pred,
	exports(Base, F, A, _Def, Meta),
%% This prevents .so libraries to be lazyloaded
%          Def \== implicit,
	meta_inc_args(Meta, A, A1),
	module_concat(Module, F, MF),
	functor(Pred, MF, A1),
	compile_clause('multifile:stump'(Module, Pred), 'basiccontrol:true'),
	compile_clause(Pred,                            Loads),
	fail.
compile_stumps(_, _, _, _).

define_stump_pred :-
	proc_declaration(multifile, 'multifile:stump'(_, _),
	    'multifile:stump', 2),
	proc_declaration(dynamic,   'multifile:stump'(_, _),
	    'multifile:stump', 2).


%%% --- Making executable file --- %%%


create_init(Module, ExecMode, MainDef, Loads, TmpPoFile) :-
	temp_filename(TmpPoFile),
	delete_on_ctrlc(TmpPoFile, Ref),
	open(TmpPoFile, write, Out),
	verbose_message(['{Compiling auxiliary file ', TmpPoFile]),
	Mode = ql(unprofiled),
	set_compiler_mode_out(Mode, Out),
	compile_clause('internals:main_module'(Module), 'basiccontrol:true'), % used in engine(internals)
	compile_loads(ExecMode, Loads),
	compile_main_def(MainDef),
	cleanup_compilation_data,
	close(Out),
	erase(Ref),
	verbose_message('}').

compile_loads(static,   'basiccontrol:fail') :- !.
compile_loads(lazyload, 'basiccontrol:fail') :- !.
compile_loads(_,        _) :-
	proc_declaration(multifile, 'multifile:$load_libs',
	    'multifile:$load_libs', 0),
	fail.
compile_loads(eagerload, _) :-
	compile_clause('multifile:$load_libs', 'basiccontrol:,'(
		'multifile:$ldlibs'(_), 'basiccontrol:fail')),
	fail.
compile_loads(_, Loads) :-
	Loads \== 'basiccontrol:fail', !,
	compile_clause('multifile:$load_libs', Loads).
compile_loads(_, _).

compile_main_def(void).
compile_main_def(clause(UserMain, ModMain)) :-
	compile_clause(UserMain, ModMain).

create_exec(ExecName, Base, PoFiles) :-
	file_data(Base, PlName, _),
	get_os(OS),
	resolve_execname(ExecName, Base, PlName, OS),
	current_input(Si),
	current_output(So),
	(file_exists(ExecName) -> delete_file(ExecName) ; true),
	delete_on_ctrlc(ExecName, Ref),
	open(ExecName, write, Stream),
	set_output(Stream),
	current_prolog_flag(self_contained, EngCfg),
	copy_header(EngCfg),
	copy_pos(PoFiles, Stream),
	close(Stream),
	erase(Ref),
	generate_batch(OS, ExecName), % Generate batch file if needed
	set_input(Si),
	set_output(So),
	set_exec_mode(PlName, ExecName).

% The chunk of code below tries to untangle the different installation
% possibilities in order to find out which engine has to be
% concatenated to the Ciao executable.
copy_header(none) :- !, % OPA
	% TODO: see ciaoc_aux:eng_exec_header/1
	absolute_file_name(library(compiler/header), '', '', '.', Header, _, _),
	copy_stdout(Header).
copy_header(EngCfg) :-
	% TODO: merge with eng(_,dostat) in builder_cmd.pl
	%   (collect foreign code here, build static engine filling 'eng_addobj')
	get_engine_file(EngCfg, EngPath),
	verbose_message(['{Using ', EngPath, ' for executable}']),
	!, % Found it, go ahead
	copy_stdout(EngPath).
copy_header(EngCfg) :-
	message(error, ['Could not find engine for ', EngCfg]),
	fail.

%% This is called only from Windows: it receives a (Windows) Ciao
%% Prolog executable and generates the corresponding batch file by
%% filling in some values in a skeleton.

generate_batch('Win32', ExecName) :-
	!,
	generate_batch_Win32(ExecName).
generate_batch(_, _).

generate_batch_Win32(ExecName) :-
	exec_ext(ExecExt),
	bat_ext(BatExt),
	( atom_concat(Base, ExecExt, ExecName) ->
	    true
	; Base = ExecName
	),
	get_platform(EngCfg),
	get_engine_file(EngCfg, EngPath),
	winpath(EngPath, EngPathP),
	atom_concat(Base, BatExt, BatName),
	open(BatName, write, Stream),
	!,
	%
	% TODO: look for CIAOENGINE in Windows registry too?
	% TODO: generate as a exec_header
	% TODO: compile as a real .exe stub?
	display(Stream, '@REM Stub automatically generated by ciaoc\n'),
	display(Stream, '@echo off\n'),
	display(Stream, 'setlocal\n'),
	display(Stream, 'set instengine='), % (do not use double-quote here)
	display(Stream, EngPathP),
	display(Stream, '\n'),
	display(Stream, 'set engine=%CIAOENGINE%\n'),
	display(Stream, 'if "x%CIAOENGINE%"=="x" set engine=%instengine%\n'),
	display(Stream, '"%engine%" %* -C -b "%~dpn0'),
	display(Stream, ExecExt),
	display(Stream, '"\n'),
	close(Stream).

generate_batch_Win32(_) :-
	message(warning, ['Unable to create batch file']).

copy_pos(PoFiles, _) :- % OPA
	current_prolog_flag(compress_exec, no), !,
	dump_pos(PoFiles).
copy_pos(PoFiles, Stream) :-
	temp_filename(TmpFile),
	open(TmpFile, write, TmpStreamw),
	set_output(TmpStreamw),
	dump_pos(PoFiles),
	close(TmpStreamw),
	open(TmpFile, read, TmpStreamr),
	set_output(Stream),
	verbose_message(['{Compressing executable}']),
	compressLZ(TmpStreamr),
	close(TmpStreamr).

resolve_execname(ExecName, _, _,  _) :- nonvar(ExecName), !.
resolve_execname(ExecName, B, Pl, OS) :-
	% Pl file has no .pl extension or we are compiling for Win32
	( Pl = B 
	; OS = 'Win32' 
	; current_prolog_flag(self_contained, EngCfg),
	  atom_concat('Win32', _, EngCfg) % Any of Win32
	),
	!,
	exec_ext(Ext),
	atom_concat(B, Ext, ExecName).
resolve_execname(ExecName, B, _, _) :- ExecName = B.

dump_pos([File|Files]) :-
	verbose_message(['{Adding ', File, '}']),
	open(File, read, Stream),
	copyLZ(Stream),
	close(Stream),
	nl,
	dump_pos(Files).
dump_pos([]).

% ---------------------------------------------------------------------------

:- data tmp_file/1.

temp_filename(File) :-
	mktemp('tmpciaoXXXXXX', File),
	assertz_fact(tmp_file(File)).

delete_temp :-
	retract_fact(tmp_file(File)),
	delete_file(File),
	fail.
delete_temp.

% ---------------------------------------------------------------------------

verbose_message(M) :-
	( current_prolog_flag(verbose_compilation, off), !
	; message(user, M)
	).

% ---------------------------------------------------------------------------

:- pred copy_stdout(File): sourcename(File) 
   # "Copies file @var{File} to standard output.".

copy_stdout(File) :-
 	open_input(File, IO),
	repeat,
	  get_byte(Code),
	  ( Code = -1
	  ; put_byte(Code),
	    fail
	  ),
	!,
	close_input(IO).



