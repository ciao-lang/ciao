:- module(_, [], [assertions, fsyntax, hiord, dcg]).

:- doc(title,  "Build code using the Ciao compiler").
:- doc(author, "Jos@'{e} F. Morales").
:- doc(author, "The Ciao Developer Team").

:- doc(module, "This is a wrapper around @apl{ciaoc} (and other Ciao
   tools) that extends the functionality of the Ciao compiler:

   @begin{itemize}
   @item Invoke the compiler as a external processes
   @item Batch compilation of collection of modules
   @item Emulator generation and compilation
   @end{itemize}.
").

% (improvements)
:- doc(bug, "Add interface to ciaopp").
:- doc(bug, "Add interface to optim_comp").
:- doc(bug, "Better build plans, start several jobs (workers)").

% TODO: Make sure that .po/.itf files of ciao_builder (due to dynamic
%    compilation) are not mixed with the build .po/.itf files.

% TODO: Do not use a dummy file in gen_asr_file_main_rel/1 (assrt_lib
%   cannot document main modules!)

:- use_module(library(lists), [member/2, append/3]).
:- use_module(library(aggregates), [findall/3]).
%
:- use_module(library(system_extra), [
	ignore_nosuccess/1,
	del_file_nofail/1,
	create_rel_link/2]).
%
:- use_module(library(pathnames), [path_concat/3, path_split/3, path_get_relative/3]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3, bundle_path/4]).
%
:- use_module(ciaobld(messages_aux),
	[normal_message/2, verbose_message/2]).
:- use_module(library(messages), [show_message/3]).
%
:- use_module(ciaobld(builder_aux),
	[ensure_builddir/2,
	 remove_dir_nofail/1]).

% ===========================================================================
:- doc(section, "Interface to Compilers").

ciaoc := ~cmd_path(core, plexe, 'ciaoc').
bootstrap_ciaoc := ~bundle_path(core, 'bootstrap/ciaoc.sta').

:- use_module(ciaobld(config_common), [cmd_path/4, libcmd_path/4]).
:- use_module(ciaobld(cpx_process), [cpx_process_call/3]).

:- use_module(ciaobld(bundle_configure), [
    set_prolog_flags_from_bundle_flags/1
]).

ciaosh_exec := ~cmd_path(core, plexe, 'ciaosh').

:- export(invoke_ciaosh_batch/1).
% Batch execution of queries using ciaosh and current config prolog flags
% TODO: Create a module instead? (remember to clean_mod/1)
invoke_ciaosh_batch(Cmds) :-
	add_config_prolog_flags(Cmds, Cmds2),
	Options = [stdin(terms(Cmds2))],
	cpx_process_call(~ciaosh_exec, ['-q', '-f'], Options).

add_config_prolog_flags(Cmds, CmdsR) :-
	set_prolog_flags_from_bundle_flags(SetPrologFlags),
	append(SetPrologFlags, Cmds, CmdsR).

:- export(invoke_ciaoc/1).
invoke_ciaoc(Args) :-
	cpx_process_call(~ciaoc, Args, []).

:- export(invoke_boot_ciaoc/2).
invoke_boot_ciaoc(Args, Opts) :-
	cpx_process_call(~bootstrap_ciaoc, Args, [boot|Opts]).

% ===========================================================================
:- doc(section, "Build of Executables").

% Build the executable for InDir/InFile, store in the build area for the
% specified Bundle and select the current version (add a symbolic link).
%
% @var{Opts} indicate the compilation options. The options indicate
% the compiler iteration that is used:
%
%  - 'bootstrap_ciaoc': initial bootstrap ciaoc
%  - 'final_ciaoc': (default)
%      final ciaoc compiler (fixpoint when self-compiling)
%  - 'libexec': command not run directly by humans
%
% Other options are:
%
%  - 'static': build a static executable (self-contained, so that
%     changes in the compiled system libraries does not affect it)
%  - 'standalone': standalone executable (self-contained, including
%     engine)
%
b_make_exec(Bundle, InFile, OutFile, Opts) :-
	( member(libexec, Opts) ->
	    ensure_builddir(Bundle, 'libexec'),
	    FileBuild = ~libcmd_path(Bundle, plexe, OutFile)
	; ensure_builddir(Bundle, 'bin'),
	  FileBuild = ~cmd_path(Bundle, plexe, OutFile)
	),
	path_split(InFile, Dir, Base),
	In = ~atom_concat(Base, '.pl'),
	ciaoc_args(Opts, FileBuild, In, Args, []),
	( member(final_ciaoc, Opts) ->
	    cpx_process_call(~ciaoc, Args, [cwd(Dir)])
	; member(bootstrap_ciaoc, Opts) ->
	    cpx_process_call(~bootstrap_ciaoc, Args, [boot, cwd(Dir)])
	; throw(bad_opts_in_b_make_exec(Opts))
	).

ciaoc_args(Opts, FileBuild, In) -->
	['-x'], % TODO: always?
	( { member(static, Opts)} -> ['-s']
	; { member(standalone, Opts) } -> ['-S']
	; []
	),
	['-o', FileBuild],
	[In].

% TODO: Fix using CIAOCACHEDIR. We have a problem here: we build some
%       libraries with the bootstrap compiler and others with the
%       compiler that is built into ciao_builder. That may give us a
%       lot of problems.
%
%       If we manage to have .po/.itf in a separate directory, we
%       could just have a toplevel as our bootstrap (that is, a system
%       where dynamically loaded code is possible).

% ---------------------------------------------------------------------------
%% Create bundle archives

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
%% 	( BaseDir = ~bundle_path(core, 'lib')
%% 	; BaseDir = ~bundle_path(core, 'library')
%% 	; BaseDir = ~bundle_path(..., ...)
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
:- doc(section, "Bootstrap management").
% TODO: generalize as eng+noarch copy?

:- use_module(library(system), [copy_file/3]).
:- use_module(ciaobld(eng_defs), [emugen_code_dir/3]).

:- export(promote_bootstrap/1).
:- pred promote_bootstrap(Eng) # "Promote the current
   @tt{ciaoc} and products of @lib{emugen} as the next bootstrap
   compiler (@tt{ciaoc.sta} and absmach code)".

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

promote_bootstrap(Eng) :-
	date_token(Token), % date token for backups 
	Target = ~bundle_path(core, 'bootstrap'), % TODO: bootstrap dir is hardwired
	promote_bootstrap_(Eng, Token, Target).

promote_bootstrap_(Eng, Token, Target) :-
	( % (failure-driven loop)
	  ( bootstrap_noarch(Orig, Dest0)
	  ; bootstrap_code_file(Orig0), Dest0 = Orig0,
	    emugen_code_dir(Eng, Orig0, Dir),
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
:- doc(section, "Engine headers for bytecode executables").

:- use_module(engine(io_basic)).
:- use_module(engine(messages_basic), [display_list/1]).
:- use_module(library(stream_utils), [open_output/2, close_output/1]).

:- use_module(ciaobld(install_aux), [final_ciao_root/1]). % TODO: need relocate?
:- use_module(ciaobld(eng_defs), [eng_mainmod/2]).

% TODO: rename, move somewhere else, make it optional, add native .exe stubs for win32?
eng_exec_header := ~bundle_path(core, 'lib/compiler/header').

:- export(build_eng_exec_header/1).
% Create exec header (based on Unix shebang) for running binaries:
%  - assumes some default CIAOROOT or gets CIAOROOT from environment
%  - locates engine at that CIAOROOT or gets CIAOENGINE from environment
%  - assumes default OS and ARCH or gets it from CIAOOS+CIAOARCH
%
build_eng_exec_header(Eng) :-
	EngMainMod = ~eng_mainmod(Eng),
	final_ciao_root(FinalCiaoRoot),
	verbose_message("exec header assume engine ~w and CIAOROOT at ~w", [EngMainMod, FinalCiaoRoot]),
	%
	eng_exec_header(HeaderPath),
	%
	% TODO: use wr_template from .hooks.pl?
	open_output(HeaderPath, Out),
	display('#!/bin/sh\n'),
	display_list([
            'r=${CIAOROOT:-\"', FinalCiaoRoot, '\"}\n',
            'e=\"$r/build/eng/', EngMainMod, '/objs/', EngMainMod, '\"${CIAOOS:+${CIAOARCH:+".$CIAOOS$CIAOARCH"}}\n',
	    'e=${CIAOENGINE:-$e}\n',
	    'exec "$e" "$@" -C -b "$0"\n'
	]),
	put_code(0xC), % ^L control character
	display('\n'),
	close_output(Out).

:- export(clean_eng_exec_header/1).
% Clean exec header created from @pred{build_eng_exec_header/1}
% TODO: customize location
clean_eng_exec_header(_Eng) :-
	eng_exec_header(HeaderPath),
	del_file_nofail(HeaderPath).

:- use_module(library(system), [winpath/2]).
:- use_module(library(stream_utils), [open_output/2, close_output/1]).
:- use_module(ciaobld(eng_defs), [eng_path/3]).

% TODO: use .cmd (winnt) instead of .bat extension?

:- export(create_windows_bat/6).
% Create a .bat exec header for executing OrigCmd (for Windows)
%   Opts: extra arguments for OrigCmd
%   EngExecOpts: extra arguments for the engine executableitself
create_windows_bat(Eng, BatCmd, Opts, EngExecOpts, OrigBundle, OrigCmd) :-
	EngBin = ~eng_path(exec, Eng), % TODO: why not the installed path?
	%
	BatFile = ~cmd_path(OrigBundle, ext('.bat'), BatCmd),
	OrigFile = ~cmd_path(OrigBundle, plexe, OrigCmd),
	%
	open_output(BatFile, Out),
	% TODO: missing quotation (e.g., of \")
	display('@"'), display(~winpath(EngBin)), display('"'),
	display(' %* '),
        ( Opts = '' -> true ; display(Opts), display(' ') ),
	display('-C '),
        ( EngExecOpts = '' -> true ; display(EngExecOpts), display(' ') ),
	display('-b \"'),
	display(OrigFile),
	display('\"'),
	nl,
	close_output(Out).

% ===========================================================================
:- doc(section, "Batch compilation of sets of modules").

:- use_module(library(source_tree), [current_file_find/3]).
:- use_module(library(system_extra), [using_tty/0]).

% TODO: cmd_build/1 can only build Prolog applications. Add plug-ins?
%       Or rewrite the special applications in some that that
%       'b_make_exec' understand?  (see 'Use packages to generate
%       script files' in my TODO list)

% TODO: show the same kind of messages that are used when compiling libraries
:- export(cmd_build/1).
cmd_build(cmd_def(Bundle, In, Output, Props)) :-
	prop_opts(Props, Opts, []),
	( member(libexec, Opts) -> What = libexec ; What = command ),
	( member(shscript, Props) ->
	    true % TODO: invoke custom build predicate
	; member(bootstrap_ciaoc, Props) ->
	    % use bootstrap ciaoc
	    % TODO: it should use 'build' configuration
	    normal_message("compiling ~w (~w) [using bootstrap compiler]", [Output, What]),
	    b_make_exec(Bundle, In, Output, [bootstrap_ciaoc|Opts])
	; % member(final_ciaoc, Props) ->
	  % use final_ciaoc (default)
	  % TODO: it should use 'build' configuration
	  % TODO: Add options for other ciaoc iterations
	  normal_message("compiling ~w (~w)", [Output, What]),
	  b_make_exec(Bundle, In, Output, [final_ciaoc|Opts])
	).

% From cmd_build/1 Props to b_make_exec/4 Opts
prop_opts(Props) -->
	prop_opt(static, Props),
	prop_opt(standalone, Props),
	prop_opt(libexec, Props).

prop_opt(Opt, Props) -->
	( { member(Opt, Props) } -> [Opt] ; [] ).

% TODO: REMOVE, add cmd options instead
:- export(cmd_build_copy/4).
% Copy a custom binary From at binary directory of builddir as Name
cmd_build_copy(Bundle, Kind, From, Name) :-
	File = ~cmd_path(Bundle, Kind, Name),
	copy_file(From, File, [overwrite]).

% TODO: REMOVE, add cmd options instead
:- export(cmd_build_link/4).
% 'Dest' will point to 'Src'
cmd_build_link(Bundle, Kind, Src, Dest) :-
	From = ~cmd_path(Bundle, Kind, Src),
	To = ~cmd_path(Bundle, Kind, Dest),
	ignore_nosuccess(create_rel_link(From, To)).

:- use_module(library(sort), [sort/2]).
	    
:- export(build_libs/2).
build_libs(Bundle, BaseDir) :-
	% Relative dir for messages
	BundleDir = ~bundle_path(Bundle, '.'),
	( BaseDir = BundleDir ->
	    RelDir = '.'
	; ( path_get_relative(BundleDir, BaseDir, RelDir) -> true
	  ; RelDir = BaseDir
	  )
	),
	% (CompActions: list of compiler actions (gpo, gaf))
	build_mod_actions(CompActions),
	compile_modules(compilable_module, BaseDir, RelDir, CompActions).

:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).
build_mod_actions(Actions) :-
	( yes = ~get_bundle_flag(builder:gen_asr) ->
	    % Generate 'asr' files during compilation
	    Actions = [do_gaf|Actions0]
	; Actions = Actions0
	),
	Actions0 = [do_gpo].

% TODO: is Filter needed now?
compile_modules(Filter, BaseDir, RelDir, CompActions) :-
	findall(m(Dir, File, FileName),
	  compilable_modules(Filter, BaseDir, Dir, File, FileName), ModulesU),
	sort(ModulesU, Modules),
	show_duplicates(Modules),
	compile_module_list(Modules, BaseDir, RelDir, CompActions).

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

%:- export(compile_module_list/4).
% TODO: integrate ciaoc_batch_call.pl in ciaoc (or create another executable)
compile_module_list(Modules, BaseDir, RelDir, CompActions) :-
	tty(UsingTTY),
	invoke_ciaosh_batch([
	  use_module(ciaobld(ciaoc_batch_call), [compile_mods/5]),
	  compile_mods(Modules, CompActions, BaseDir, RelDir, UsingTTY)
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
	runtests_dir(Bundle, Dir, [rtc_entry]).

:- export(runtests_dir/3).
% Call unittests on directory Dir (recursive) of bundle Bundle
runtests_dir(Bundle, Dir, Opts) :-
	AbsDir = ~bundle_path(Bundle, Dir),
	exists_and_compilable(AbsDir),
	!,
	normal_message("running tests at ~w/", [Dir]),
	invoke_ciaosh_batch([
          use_module(library(unittest), [run_tests_in_dir_rec/2]),
	  run_tests_in_dir_rec(AbsDir, Opts)
	]).
runtests_dir(_, _, _).

:- use_module(library(system), [file_exists/1]).

:- export(exists_and_compilable/1).
exists_and_compilable(Dir) :-
	path_concat(Dir, 'NOCOMPILE', NCDir),
	( file_exists(Dir), \+ file_exists(NCDir) -> true
	; fail
	).

% ===========================================================================
:- doc(section, "Cleaning").

% Clean (compilation files in) a directory tree (recursively)
:- export(clean_tree/1).
clean_tree(Dir) :-
	clean_aux(clean_tree, [Dir]).

% Clean compilation files for a individual module (Base is file without .pl suffix)
:- export(clean_mod/1).
clean_mod(Base) :-
	clean_aux(clean_mod, [Base]).

:- use_module(library(sh_process), [sh_process_call/3]).

% TODO: reimplement in Prolog
clean_aux(Command, Args) :-
	OS = ~get_bundle_flag(core:os),
	Arch = ~get_bundle_flag(core:arch),
	Env = ['CIAOOS' = OS, 'CIAOARCH' = Arch], % (for 'clean_mod')
	sh_process_call(~clean_aux_sh, [Command|Args], [env(Env)]).

:- use_module(engine(internals), [ciao_root/1]).

% TODO: could I use bundle_path(builder, 'sh_src/clean_aux.sh')? (perhaps it is not registered)
clean_aux_sh := Path :-
	ciao_root(CiaoRoot),
	Path = ~path_concat(CiaoRoot, 'builder/sh_src/clean_aux.sh').

% ---------------------------------------------------------------------------

:- use_module(engine(internals),
	[po_filename/2,
	 itf_filename/2]).

:- export(clean_mod0/1).
% TODO: see profiler_auto_conf:clean_module/1
% TODO: complete, replace sh version
clean_mod0(Base) :-
	del_file_nofail(~po_filename(Base)),
	del_file_nofail(~itf_filename(Base)).

