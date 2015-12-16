:- module(_, [], [assertions, fsyntax, hiord, dcg]).

:- doc(title,  "Extended interface to Ciao compiler").
:- doc(subtitle, "(compiler, doc generator, etc.)").

:- doc(author, "Jos@'{e} F. Morales").
:- doc(author, "Ciao Deveveloper Team").

:- doc(module, "This is a wrapper around @apl{ciaoc} (and other Ciao
   tools) that extends the functionality of the Ciao compiler:

   @begin{itemize}
   @item Invoke the compiler and documentation generator as a external processes
   @item Batch compilation of collection of modules
   @item Emulator generation and compilation
   @end{itemize}/1).
").

% (improvements)
:- doc(bug, "Add interface to ciaopp").
:- doc(bug, "Add interface to optim_comp").
:- doc(bug, "Better build plans, start several jobs (workers)").

% TODO: Make sure that .po/.itf files of ciao_builder (due to dynamic
%    compilation) are not mixed with the build .po/.itf files.

% TODO: Do not use a dummy file in gen_asr_file_main_rel/1 (assrt_lib
%   cannot document main modules!)

:- use_module(library(lists), [append/3, length/2, select/3]).
:- use_module(library(aggregates), [findall/3]).
%
:- use_module(library(system), [cd/1, working_directory/2, file_exists/1]).
:- use_module(library(system_extra), [using_tty/0]).
:- use_module(library(system_extra), [del_file_nofail/1]).
%
:- use_module(library(pathnames), [path_concat/3, path_split/3]).
:- use_module(library(bundle/paths_extra), [fsR/2]).
%
:- use_module(ciaobld(messages_aux),
	[cmd_message/3, verbose_message/2]).
:- use_module(library(messages), [show_message/3]).
%
:- use_module(ciaobld(builder_aux),
	[ensure_builddir_bin/0,
	 n_and_props/3,
	 n_output/3,
	 n_name/2]).
:- use_module(ciaobld(builder_aux), [root_bundle_source_dir/1]).

% ===========================================================================

:- export(ciaoc/1). % (EXPORTED)
ciaoc := ~bld_cmd_path(build, plexe, 'ciaoc').

:- export(bootstrap_ciaoc/1). % (EXPORTED)
bootstrap_ciaoc := ~fsR(bundle_src(core)/'bootstrap'/'ciaoc.sta').

% ===========================================================================
:- doc(section, "Interface to Compilers"). % including documentation generation

:- use_module(library(process), [process_call/3]).
:- use_module(ciaobld(config_common), [verbose_build/1]).
:- use_module(ciaobld(config_common), [bld_cmd_path/4]).

:- use_module(ciaobld(bundle_configure), [
    set_prolog_flags_from_bundle_flags/1
]).

:- export(invoke_lpdoc/2).
invoke_lpdoc(ConfigFile, Args) :-
	invoke_lpdoc(['-f', ConfigFile|Args]).

lpdoc_exec := ~bld_cmd_path(build, plexe, 'lpdoc').

% TODO: make lpdoc verbose message more descriptive? remove message
%   here (e.g., show basename of directory containing SETTINGS)
:- export(invoke_lpdoc/1).
invoke_lpdoc(Args) :-
	verbose_message("Executing `lpdoc' with arguments ~w", [Args]),
	( verbose_build(yes) -> Args2 = ['-v'|Args] ; Args2 = Args ),
	Dir = ~fsR(builddir_doc(build)),
	localciao_process_call(build, ~lpdoc_exec, Args2, [cwd(Dir)]).

ciaosh_exec := ~bld_cmd_path(build, plexe, 'ciaosh').

:- export(invoke_ciaosh/1).
invoke_ciaosh(Input) :-
	Options = [stdin(file(Input))],
	localciao_process_call(build, ~ciaosh_exec, ['-q', '-f'], Options).

:- export(invoke_ciaosh_batch/1).
% Batch execution of queries using ciaosh and current config prolog flags
% TODO: Create a module instead? (remember to clean_mod/1)
invoke_ciaosh_batch(Cmds) :-
	add_config_prolog_flags(Cmds, Cmds2),
	Options = [stdin(terms(Cmds2))],
	localciao_process_call(build, ~ciaosh_exec, ['-q', '-f'], Options).

add_config_prolog_flags(Cmds, CmdsR) :-
	set_prolog_flags_from_bundle_flags(SetPrologFlags),
	append(SetPrologFlags, Cmds, CmdsR).

:- export(invoke_ciaoc/1).
invoke_ciaoc(Args) :-
	localciao_process_call(build, ~ciaoc, Args, []).

% ===========================================================================
:- doc(section, "Build of Executables and Engines").

% Build the executable for InDir/InFile, store in the build directory
% and select the current version (add a symbolic link).
%
% @var{Opts} indicate the compilation options. The options indicate
% the compiler iteration that is ued:
%
%  - 'bootstrap_ciaoc': initial bootstrap ciaoc
%  - 'final_ciaoc': (default)
%      final ciaoc compiler (fixpoint when self-compiling)
%
% Other options are:
%
%  - 'static': build a static executable (self-contained, so that
%     changes in the compiled system libraries does not affect it)
%
b_make_exec(InDir, InFile, OutFile, Opts) :-
	ensure_builddir_bin,
	FileBuild = ~bld_cmd_path(build, plexe, OutFile),
	( member(static, Opts) ->
	    Static = ['-s']
	; Static = []
	),
	Dir = ~fsR(InDir),
	In = ~atom_concat(InFile, '.pl'),
	Args = ['-x' | ~append(Static, ['-o', FileBuild, In])],
	( member(final_ciaoc, Opts) ->
	    localciao_process_call(build, ~ciaoc, Args, [cwd(Dir)])
	; member(bootstrap_ciaoc, Opts) ->
	    localciao_process_call(bootbuild, ~bootstrap_ciaoc, Args, [cwd(Dir)])
	; throw(bad_opts_in_b_make_exec(Opts))
	).

% TODO: Fix using CIAOCACHEDIR. We have a problem here: we build some
%       libraries with the bootstrap compiler and others with the
%       compiler that is built into ciao_builder. That may give us a
%       lot of problems.
%
%       If we manage to have .po/.itf in a separate directory, we
%       could just have a toplevel as our bootstrap (that is, a system
%       where dynamically loaded code is possible).

% ---------------------------------------------------------------------------

:- export(eng_build/2).
eng_build(EngMainMod, EngOpts0) :-
	% Static dependencies
	( member(static_mods(StatMods), EngOpts0) ->
	    display(user_error, 'WARNING: static_mods(_) assumes that libraries are already compiled!\n'), % TODO: fix, enrich exprs
	    get_static(StatMods, StatBases, AddObjs),
	    EngOpts = [static_bases(StatBases), static_objs(AddObjs)|EngOpts0]
	; EngOpts = EngOpts0
	),
	% Generate source
 	eng_emugen(EngMainMod),
	eng_create_init(EngMainMod, EngOpts),
	% Sysdep configuration
	eng_config_sysdep(EngMainMod, EngOpts),
	% Version info
 	eng_prebuild_version_info(EngMainMod),
	% Build native
 	eng_build_native(EngMainMod).

:- export(eng_clean/1).
% Clean the engine (including .pl sources)
eng_clean(EngMainMod) :-
	% Ensure that byproducts of EngMainMod compilation are
	% generated on next build.
	Base = ~fsR(bundle_src(core)/engine/EngMainMod),
	clean_mod0(Base),
	% Clean the engine build area
	eng_clean_native(EngMainMod).

% TODO: Merge with b_make_exec (this generates the C code for ciaoc
%   using the boostrap compiler; in this branch of Ciao this code is
%   reused for all other Ciao static executables)
%:- export(eng_emugen/1).
eng_emugen(EngMainMod) :-
	Dir = ~fsR(bundle_src(core)/engine),
	In = ~atom_concat(EngMainMod, '.pl'),
        %
	localciao_process_call(bootbuild, ~bootstrap_ciaoc, ['-c', In], [cwd(Dir)]).

:- use_module(engine(internals), [a_filename/2]). % ('.a' static lib)

get_static([], [], []).
get_static([Spec|Specs], [Base|Bases], [FileA|FilesA]) :-
	absolute_file_name(Spec, File),
	atom_concat(Base, '.pl', File),
	a_filename(Base, FileA),
	file_exists(FileA),
	!,
	get_static(Specs, Bases, FilesA).
get_static([_Spec|Specs], Bases, FilesA) :-
	% module does not seem to have foreign code (.a)
	get_static(Specs, Bases, FilesA).

%% % TODO: This is currently disabled:
%% %  - we should ask the compiler instead of searching the whole bundle
%% %  - 
%% % TODO: Implement in a completely different way: ask the compiler for
%% %   that information (it is in the .itf file)
%% % :- use_module(library(source_tree), [current_file_find/3]).
%% 
%% % Find modules with platform specific compiled code
%% :- export(enum_platdep_mod/2).
%% enum_platdep_mod(FileName, FileA) :-
%% 	% TODO: ad-hoc!
%% 	( BaseDir = ~fsR(bundle_src(core)/lib)
%% 	; BaseDir = ~fsR(bundle_src(core)/library)
%% 	; BaseDir = ~fsR(bundle_src(contrib)/library)
%% 	),
%% 	current_platdep_module(BaseDir, FileName, FileA).
%% 
%% current_platdep_module(BaseDir, FileName, FileA) :-
%% 	% TODO: coupled with the foreign interface...
%% 	current_file_find(compilable_module, BaseDir, FileName),
%% 	atom_concat(FileBase, '.pl', FileName),
%% 	a_filename(FileBase, FileA),
%% 	file_exists(FileA).

% ---------------------------------------------------------------------------

:- use_module(ciaobld(ciaoc_aux), [sh_process_call/3]).
:- use_module(ciaobld(config_common),
	[eng_h_alias/2,
	 get_eng_cfg/2,
	 bld_eng_path/4]).

:- use_module(library(pathnames), [path_dirname/2]).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(file_utils), [string_to_file/2]).

% Write Text to F, make sure that the path exists
mkpath_and_write(Text, F) :-
	path_dirname(F, D),
	mkpath(D),
	string_to_file(Text, F).

% ---------------------------------------------------------------------------
% Create engine initialization code

%:- export(eng_create_init/2).
% Generates engine initialization code
eng_create_init(EngMainMod, EngOpts) :-
	F = ~path_concat(~bld_eng_path(cdir, build, EngMainMod), 'eng_static_mod.c'),
	eng_static_mod_c(EngOpts, Text, []),
	mkpath_and_write(Text, F).

eng_static_mod_c(EngOpts) --> { member(static_bases(Bases), EngOpts) }, !,
	stat_mod_inits(Bases).
eng_static_mod_c(_EngOpts) --> [].

stat_mod_inits([]) --> [].
stat_mod_inits([Base|Bases]) --> stat_mod_init(Base), stat_mod_inits(Bases).

% Generate C code to register and initialize the static module
stat_mod_init(Base) -->
	{ path_split(Base, _, Mod) },
	% TODO: identifier mangling/demanging for Mod as C identifier (share with foreign interface)
	% TODO: escape Mod as string
	"{ ",
	"void ", atm(Mod), "_init(char *module);", " ",
	"define_c_static_mod(\"", atm(Mod), "\");",
	atm(Mod), "_init(\"", atm(Mod), "\");",
	" }\n".

% ---------------------------------------------------------------------------
% Execute sysdep configuration step (see config-sysdep.sh)

% NOTE: Part of this code is implemented in builder/sh_code (since it
%   is needed for bootstrap)

:- export(bundle_flags_sh_file/1).
% File where eng_config_sysdep/1 expects the bundle flags (in .sh format)
bundle_flags_sh_file := ~fsR(builddir(build)/'ciao.config_saved_sh').

%:- export(eng_config_sysdep/2).
% Generates 'meta_sh' and perform sysdep configuration (for this
% platform) on the specified engine based on the input configuration
% parameters.
%
% Precondition: the configuration values are loaded
eng_config_sysdep(EngMainMod, EngOpts) :-
	CfgInput = ~bundle_flags_sh_file,
	%
	EngDir = ~bld_eng_path(engdir, build, EngMainMod),
	EngCfg = ~get_eng_cfg(build),
	EngMetaSh = ~path_concat(~bld_eng_path(cfgdir, build, EngMainMod), 'meta_sh'),
	create_eng_meta_sh(EngMainMod, EngOpts, CfgInput, EngMetaSh),
	%
	sh_process_call(~config_sysdep_sh,
	       [EngDir, EngCfg],
	       [cwd(~fsR(bundle_src(core)))]).

config_sysdep_sh := ~fsR(bundle_src(builder)/sh_src/'config-sysdep'/'config-sysdep.sh').

% Create meta_sh for config-sysdep and engine build
% TODO: At least $eng_name and $eng_h_alias should be created at the
%   same time than 'eng_info_mk'; the others are resolved names and
%   cannot; all should be customizable from ciaoengine.pl (in some
%   form).

create_eng_meta_sh(EngMainMod, EngOpts, CfgInput, EngMetaSh) :-
	eng_meta_sh(EngMainMod, EngOpts, CfgInput, Text, []),
	mkpath_and_write(Text, EngMetaSh).

eng_meta_sh(EngMainMod, EngOpts, CfgInput) -->
	% Engine name
	"eng_name=\"", atm(EngMainMod), "\"\n",
	% Sub-directory in include/ for engine headers
	{ eng_h_alias(EngMainMod, HAlias) },
	"eng_h_alias=\"", atm(HAlias), "\"\n",
	% Directory for engine source (not autogenerated)
	{ EngSrc = ~fsR(bundle_src(core)/engine) },
	"eng_srcdir=\"", atm(EngSrc), "\"\n",
	% Use STAT_LIBS (system static libs)
	( { member(add_stat_libs, EngOpts) } ->
	    "eng_use_stat_libs=\"", atm(yes), "\"\n"
	; "eng_use_stat_libs=\"", atm(no), "\"\n"
	),
	% static .a for foreign code
	{ member(static_objs(AddObjs0), EngOpts) ->
            atom_concat_with_blanks(AddObjs0, AddObjs)
	; AddObjs = ''
	},
	"eng_addobj=\"", atm(AddObjs), "\"\n",
	% Input for configuration
	"eng_ciao_config=\"", atm(CfgInput), "\"\n",
	% Additional engine dependencies for sysdep config
	% TODO: look at gsl.hooks.pl
	{ GSLEngDir = ~bld_eng_path(cfgdir, build, 'gsl') }, % NOTE: not an engine
	"gsl_engdir=\"", atm(GSLEngDir), "\"\n".

atm(X) --> { atom_codes(X, Cs) }, str(Cs).

str([]) --> [].
str([C|Cs]) --> [C], str(Cs).

% TODO: duplicated
atom_concat_with_blanks(L) := ~atom_concat(~separate_with_blanks(L)).

separate_with_blanks([]) := [] :- !.
separate_with_blanks([A]) := [A] :- !.
separate_with_blanks([A, B|Cs]) := [A, ' '|~separate_with_blanks([B|Cs])] :- !.

% ---------------------------------------------------------------------------
% TODO: generalize as eng+noarch copy?

:- use_module(library(system), [copy_file/3]).

:- export(promote_bootstrap/1).
:- pred promote_bootstrap(EngMainMod) # "Promote the current
   @tt{ciaoc} and products of @lib{emugen} as the next bootstrap
   compiler (@tt{ciaoc.sta} and absmach code)".

:- use_module(library(compiler/emugen/emugen_common), % (not the package itself)
       [emugen_code_dir/3]).

% TODO: Synchronize with CFILES in builder/src/tests_emugen.bash
% TODO: Synchronize with ':- native_export' in 'ciaoengine.pl'.
%
bootstrap_noarch(~ciaoc, 'ciaoc.sta'). % TODO: customize
% 
bootstrap_code_file('wamloop.c').
bootstrap_code_file('eng_info_mk').
bootstrap_code_file('eng_info_sh').
bootstrap_code_file('instrdefs.h').
bootstrap_code_file('absmachdef.h').

promote_bootstrap(EngMainMod) :-
	date_token(Token), % date token for backups 
	Target = ~fsR(bundle_src(core)/bootstrap),
	promote_bootstrap_(EngMainMod, Token, Target).

promote_bootstrap_(EngMainMod, Token, Target) :-
	( % (failure-driven loop)
	  ( bootstrap_noarch(Orig, Dest0)
	  ; bootstrap_code_file(Orig0), Dest0 = Orig0,
	    emugen_code_dir(EngMainMod, Orig0, Dir),
	    path_concat(Dir, Orig0, Orig)
	  ),
	  path_concat(Target, Dest0, Dest),
	  backup_and_copy(Token, Orig, Dest),
	  fail
	; true
	).

backup_and_copy(Token, Orig, Dest) :-
	backup_name(Dest, Token, DestBak),
        copy_file(Dest, DestBak, [overwrite]),
	copy_file(Orig, Dest, [overwrite]).

% Obtain a backup name by appending a date Token
backup_name(File, Token, FileBak) :-
	atom_concat([File, '-', Token], FileBak).

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(system), [datime/9]).

% Obtain a date token for backups (YmdHMS format)
date_token(DateAtm) :-
	datime(_T, Year, Month, Day, Hour, Min, Sec, _WeekDay, _YearDay),
	DateAtm = ~atom_concat([
	    ~number_to_atm2(Year),
	    ~number_to_atm2(Month),
	    ~number_to_atm2(Day),
	    ~number_to_atm2(Hour),
	    ~number_to_atm2(Min),
	    ~number_to_atm2(Sec)]).

% From natural number to atom, with at least 2 digits
number_to_atm2(X, Y) :-
	number_codes(X, Cs0),
	( X < 10 -> Cs = "0"||Cs0 ; Cs = Cs0 ),
	atom_codes(Y, Cs).

% ---------------------------------------------------------------------------

:- use_module(library(streams), [open_output/2, close_output/1]).
:- use_module(ciaobld(config_common),
	[instype/1,
	 bld_eng_path/4,
	 active_bld_eng_path/3,
	 active_inst_eng_path/3]).

% TODO: rename, move somewhere else, make it optional, add native .exe stubs for win32?
eng_exec_header := ~fsR(bundle_src(ciao)/'core'/'lib'/'compiler'/'header').

:- export(build_eng_exec_header/1).
% Create exec header (based on Unix shebang) for running binaries
% through the specified engine EngMainMod (by default)
build_eng_exec_header(EngMainMod) :-
	( get_os('Win32') ->
	    Eng = ~bld_eng_path(exec, build, EngMainMod) % TODO: why? this seems wrong in at least MSYS2
	; instype(local) ->
	    Eng = ~active_bld_eng_path(exec_anyarch, EngMainMod)
	; Eng = ~active_inst_eng_path(exec_anyarch, EngMainMod)
	),
	%
	verbose_message("exec header assume engine at ~w", [Eng]),
	%
	eng_exec_header(HeaderPath),
	%
	open_output(HeaderPath, Out),
	display('#!/bin/sh\n'),
	% Note: when executed, selects the right engine based on CIAOOS, CIAOARCH (if defined)
	display_list(['INSTENGINE=\"', Eng,
	              '\"${CIAOOS:+${CIAOARCH:+".$CIAOOS$CIAOARCH"}}\n']),
	display('ENGINE=${CIAOENGINE:-${INSTENGINE}}\n'),
	display('exec "$ENGINE" "$@" -C -b "$0"\n'),
	put_code(0xC), % ^L control character
	display('\n'),
	close_output(Out).

:- export(clean_eng_exec_header/1).
% Clean exec header created from @pred{build_eng_exec_header/1}
clean_eng_exec_header(_EngMainMod) :-
	eng_exec_header(HeaderPath),
	del_file_nofail(HeaderPath).

:- use_module(library(system), [winpath/2]).
:- use_module(library(streams), [open_output/2, close_output/1]).

% TODO: use .cmd (winnt) instead of .bat extension?

:- export(create_windows_bat/5).
% Create a .bat exec header for executing OrigCmd (for Windows)
%   Opts: extra arguments for OrigCmd
%   EngOpts: extra arguments for the engine itself
create_windows_bat(EngMainMod, BatCmd, Opts, EngOpts, OrigCmd) :-
	Eng = ~bld_eng_path(exec, build, EngMainMod), % TODO: why not the installed path?
	%
	BatFile = ~bld_cmd_path(build, ext('.bat'), BatCmd),
	OrigFile = ~bld_cmd_path(build, plexe, OrigCmd),
	%
	open_output(BatFile, Out),
	% TODO: missing quotation (e.g., of \")
	display('@"'), display(~winpath(Eng)), display('"'),
	display(' %* '),
        ( Opts = '' -> true ; display(Opts), display(' ') ),
	display('-C '),
        ( EngOpts = '' -> true ; display(EngOpts), display(' ') ),
	display('-b \"'),
	display(OrigFile),
	display('\"'),
	nl,
	close_output(Out).

% ===========================================================================
:- doc(section, "Batch compilation of sets of modules").

:- use_module(library(source_tree), [current_file_find/3]).

% TODO: build_cmds_list/3 does not receive the kind of utility (K)
%       It can only build Prolog applications. Add plug-ins? Or rewrite
%       the special applications in some that that 'b_make_exec' understand?
%       (see 'Use packages to generate script files' in my TODO list)

% TODO: show the same kind of messages that are used when compiling libraries
:- export(build_cmds_list/3).
build_cmds_list(Bundle, Dir, Ps) :-
	( % (failure-driven loop)
	  member(P0, Ps),
	    build_cmd(Bundle, Dir, P0),
	    fail
	; true
	).

build_cmd(Bundle, Dir, P0) :-
	n_and_props(P0, P, Props),
	n_name(Props, Name),
	n_output(P, Props, Output),
	( member(static, Props) ->
	    % make static executable
	    Opts = [static]
	; Opts = []
	),
	( member(shscript, Props) ->
	    true % TODO: invoke custom build predicate
	; member(bootstrap_ciaoc, Props) ->
	    % use bootstrap ciaoc
	    % TODO: it should use 'build' configuration
	    cmd_message(Bundle, "building '~w' (~s) using bootstrap compiler", [Output, Name]),
	    b_make_exec(Dir, P, Output, [bootstrap_ciaoc|Opts])
	; % member(final_ciaoc, Props) ->
	  % use final_ciaoc (default)
	  % TODO: it should use 'build' configuration
	  % TODO: Add options for other ciaoc iterations
	  cmd_message(Bundle, "building '~w' (~s)", [Output, Name]),
	  b_make_exec(Dir, P, Output, [final_ciaoc|Opts])
	).

:- use_module(library(sort), [sort/2]).
	    
:- export(build_libs/2).
build_libs(Bundle, BaseDir) :-
	% (CompActions: list of compiler actions (gpo, gaf))
	build_mod_actions(CompActions),
	( BaseDir = '.' ->
	    cmd_message(Bundle, "compiling libraries", [])
	; cmd_message(Bundle, "compiling '~w' libraries", [BaseDir])
	),
	compile_modules(compilable_module, BaseDir, CompActions).

:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).
build_mod_actions(Actions) :-
	( yes = ~get_bundle_flag(ciao:gen_asr) ->
	    % Generate 'asr' files during compilation
	    Actions = [do_gaf|Actions0]
	; Actions = Actions0
	),
	Actions0 = [do_gpo].

% TODO: is Filter needed now?
compile_modules(Filter, BaseDir, CompActions) :-
	findall(m(Dir, File, FileName),
	  compilable_modules(Filter, BaseDir, Dir, File, FileName), ModulesU),
	sort(ModulesU, Modules),
	show_duplicates(Modules),
	compile_module_list(Modules, BaseDir, CompActions).

compilable_modules(Filter, BaseDir, Dir, File, FileName) :-
	current_file_find(Filter, BaseDir, FileName),
	path_split(FileName, Dir, File).

show_duplicates(Modules) :-
	sort_by_file(Modules, Modules2),
	dump_duplicates(Modules2).

% TODO: This can be improved!
dump_duplicates(Modules) :-
	dump_duplicates_(Modules, [_|Modules]).

dump_duplicates_([], _).
dump_duplicates_([m(PrevFile, _, PrevFileName)|PrevModules], [m(File, _, FileName)|Modules]) :-
	( File == PrevFile ->
	    show_message(error, "Module ~w already defined in ~w", [FileName, PrevFileName])
	; true
	),
	dump_duplicates_(PrevModules, Modules).

sort_by_file(Modules, Modules2) :-
	sort_by_file_(Modules, Modules1),
	sort(Modules1, Modules2).

sort_by_file_([], []).
sort_by_file_([m(Dir, File, FileName)|Modules],
	    [m(File, Dir, FileName)|Modules2]) :-
	sort_by_file_(Modules, Modules2).

:- export(compile_module_list/3).
% TODO: integrate ciaoc_batch_call.pl in ciaoc (or create another executable)
compile_module_list(Modules, BaseDir, CompActions) :-
	tty(UsingTTY),
	invoke_ciaosh_batch([
	  use_module(ciaobld(ciaoc_batch_call), [compile_mods/4]),
	  compile_mods(Modules, CompActions, BaseDir, UsingTTY)
	]).

tty(X) :-
	( using_tty ->
	    X = using_tty
	; X = no_tty
	).

% ===========================================================================
:- doc(section, "Testing").

:- export(runtests_dir/2).
% Call unittests on directory Dir (recursive) of bundle Bundle
runtests_dir(Bundle, Dir) :-
	AbsDir = ~fsR(bundle_src(Bundle)/Dir),
	exists_and_compilable(AbsDir),
	!,
	cmd_message(Bundle, "running '~w' tests", [Dir]),
	invoke_ciaosh_batch([
          use_module(library(unittest), [run_tests_in_dir_rec/2]),
	  run_tests_in_dir_rec(AbsDir, [rtc_entry])
	]).
runtests_dir(_, _).

:- export(exists_and_compilable/1).
exists_and_compilable(Dir) :-
	path_concat(Dir, 'NOCOMPILE', NCDir),
	( file_exists(Dir), \+ file_exists(NCDir) -> true
	; fail
	).

% ===========================================================================

:- use_module(library(system), [using_windows/0]).

:- export(sh_process_call/3).
% Execute a sh script
% TODO: sh scripts should be reimplemented 
sh_process_call(Script, Args, Opts) :-
	( using_windows -> % (e.g., MinGW, assumes sh.exe is in path)
	    process_call(path(sh), [Script|Args], Opts)
	; process_call(Script, Args, Opts)
	).

:- export(cpx_process_call/3).
% Execute a Ciao bytecode executable
% ('CIAOENGINE' env var must be specified in a env(_) option in Opts)
cpx_process_call(CiaoExec, Args, Opts) :-
	using_windows, !,
        % '#!/...' is not supported in non-POSIX systems
	( member(env(Env), Opts),
          member('CIAOENGINE'=CiaoEngine, Env) -> true
        ; throw(error(unknown_ciaoengine, cpx_process_call/3))
        ),
        append(Args, ['-C', '-b', CiaoExec], Args2),
        process_call(CiaoEngine, Args2, Opts).
cpx_process_call(CiaoExec, Args, Opts) :-
	process_call(CiaoExec, Args, Opts).

:- use_module(ciaobld(config_common), [default_eng/1, localciao_env/3]).

:- export(localciao_process_call/4).
% Like cpx_process_call/3 but setups the environment for executing the
% binary from a given builddir
localciao_process_call(BldId, Cmd, Args, Opts) :-
	Env = ~localciao_env(BldId, ~default_eng),
	( select(env(Env0), Opts, Opts1) ->
	    Env2 = ~append(Env, Env0)
	; Env2 = Env, Opts1 = Opts
	),
	Opts2 = [env(Env)|Opts1],
	cpx_process_call(Cmd, Args, Opts2).

% ===========================================================================
:- doc(section, "Version info for engine").

:- use_module(library(bundle/bundle_info), [root_bundle/1]).
:- use_module(library(bundle/bundle_info), [bundle_version/2, bundle_patch/2]).
:- use_module(ciaobld(bundle_hash), [bundle_commit_info/3]).

% TODO: see generate_version_auto/2

%:- export(eng_prebuild_version_info/1).
% Prepare version.c and version.h with version information.
%
% TODO: Write the part that generates the commit/version info in
%   Prolog -- write a dummy version for bootstrapping
%
eng_prebuild_version_info(EngMainMod) :-
	root_bundle(Bundle),
	gen_eng_version_h(Bundle, EngMainMod),
	gen_eng_version_c(Bundle, EngMainMod).

gen_eng_version_h(Bundle, EngMainMod) :-
	H = ~path_concat(~bld_eng_path(hdir, build, EngMainMod), ~eng_h_alias(EngMainMod)),
	VerH = ~path_concat(H, 'version.h'),
	( file_exists(VerH) -> % TODO: update file if contents change
	    true
	; string_version_h(Bundle, TextH, []),
	  mkpath_and_write(TextH, VerH)
	).

gen_eng_version_c(Bundle, EngMainMod) :-
	VerC = ~path_concat(~bld_eng_path(cdir, build, EngMainMod), 'version.c'),
	( file_exists(VerC) -> % TODO: update file if contents change
	    true
	; string_version_c(Bundle, TextC, []),
	  mkpath_and_write(TextC, VerC)
	).

string_version_h(Bundle) -->
	{ CommitDate = ~bundle_commit_info(Bundle, date) },
	{ CommitDesc = ~bundle_commit_info(Bundle, desc) },
	"#define CIAO_VERSION_STRING \"Ciao \" \"", atm(CommitDesc), "\" \" (\" \"", atm(CommitDate), "\" \")\"\n".

string_version_c(Bundle) -->
	{ Version = ~bundle_version(Bundle) },
	{ Patch = ~bundle_patch(Bundle) },
	{ CommitBranch = ~bundle_commit_info(Bundle, branch) },
	{ CommitId = ~bundle_commit_info(Bundle, id) },
	{ CommitDate = ~bundle_commit_info(Bundle, date) },
	{ CommitDesc = ~bundle_commit_info(Bundle, desc) },
	"char *ciao_version = \"", atm(Version), "\";\n",
	"char *ciao_patch = \"", atm(Patch), "\";\n",
	"char *ciao_commit_branch = \"", atm(CommitBranch), "\";\n",
	"char *ciao_commit_id = \"", atm(CommitId), "\";\n",
	"char *ciao_commit_date = \"", atm(CommitDate), "\";\n",
	"char *ciao_commit_desc = \"", atm(CommitDesc), "\";\n".

% ===========================================================================
:- doc(section, "Engine build and clean (C code)").

%:- export(eng_build_native/1).
% Commands for building engines
% (creates the engine build area implicitly)
eng_build_native(EngMainMod) :-
	build_engine_(EngMainMod, build, []).

:- export(eng_clean_native/1).
% Clean the engine build area
eng_clean_native(EngMainMod) :-
	build_engine_(EngMainMod, clean, []).

build_engine_(EngMainMod, Target, Env0) :-
	EngDir = ~bld_eng_path(engdir, build, EngMainMod),
	EngCfg = ~get_eng_cfg(build),
	% (input: look at build_engine.sh)
	Env = ['BLD_ENGDIR' = EngDir,
	       'ENG_CFG' = EngCfg|Env0],
	root_bundle_source_dir(CiaoSrc),
	path_concat(CiaoSrc, 'builder/sh_src/build_engine.sh', BuildEngineSH), % TODO: hardwired path
	sh_process_call(BuildEngineSH, [Target], [env(Env)]).

% ===========================================================================
:- doc(section, "Cleaning").

:- use_module(library(source_tree), [remove_dir/1]).
:- use_module(engine(internals),
	[po_filename/2,
	 itf_filename/2]).

% Special clean targets for builddir
:- export(builddir_clean/1).
builddir_clean(bin) :-
	remove_dir(~fsR(builddir(build)/bin)).
builddir_clean(pbundle) :-
	remove_dir(~fsR(builddir(build)/pbundle)).
builddir_clean(config) :-
	del_file_nofail(~fsR(builddir(build)/'ciao.config_saved')),
	del_file_nofail(~fsR(builddir(build)/'ciao.config_saved_sh')).
builddir_clean(all) :-
	remove_dir(~fsR(builddir(build))).

% Clean bundlereg
:- export(builddir_clean_bundlereg/0).
builddir_clean_bundlereg :-
	root_bundle_source_dir(CiaoSrc),
	path_concat(CiaoSrc, 'core/lib/bundlereg__auto', Dir),
	remove_dir(Dir).

% Clean (compilation files in) a directory tree (recursively)
:- export(clean_tree/1).
clean_tree(Dir) :-
	clean_aux(clean_tree, [Dir]).

% Clean compilation files for a individual module (Base is file without .pl suffix)
:- export(clean_mod/1).
clean_mod(Base) :-
	clean_aux(clean_mod, [Base]).

% TODO: see profiler_auto_conf:clean_module/1
% TODO: complete, replace previous
clean_mod0(Base) :-
	del_file_nofail(~po_filename(Base)),
	del_file_nofail(~itf_filename(Base)).

% TODO: reimplement in Prolog
clean_aux(Command, Args) :-
	CfgInput = ~bundle_flags_sh_file,
	Env = ['ENG_CIAO_CONFIG' = CfgInput], % (for 'clean_mod')
	root_bundle_source_dir(CiaoSrc),
	atom_concat(CiaoSrc, '/builder/sh_src/clean_aux.sh', CleanSH),
	sh_process_call(CleanSH, [Command|Args], [env(Env)]).
