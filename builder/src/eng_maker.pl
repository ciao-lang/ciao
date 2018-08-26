:- module(_, [], [assertions, fsyntax, hiord, dcg]).

:- doc(title,  "Engine maker").

:- doc(author, "Jos@'{e} F. Morales").
:- doc(author, "Ciao Deveveloper Team").

:- doc(module, "This module implements a minimalistic build system for
   the engine, which includes an incremental build system for C code,
   some of them automatically generated from @lib{emugen}. The output
   can be both native executables, dynamic, and/or static libraries.

   See @lib{eng_defs} and @tt{build_engine.sh} for more details.").

:- use_module(library(lists), [member/2]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(system), [file_exists/1]).
:- use_module(library(system_extra), [del_file_nofail/1]).
%
:- use_module(library(pathnames), [path_concat/3, path_split/3]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
%
:- use_module(library(sh_process), [sh_process_call/3]).
:- use_module(ciaobld(messages_aux), [normal_message/2]).
:- use_module(ciaobld(ciaoc_aux), [invoke_boot_ciaoc/2, clean_mod0/1]).
:- use_module(ciaobld(eng_defs),
	[eng_mainmod/2,
	 eng_mainbase/2,
	 eng_srcdirs/2,
	 eng_opts/2,
	 eng_h_alias/2,
	 eng_cfg/2,
	 active_eng_name/3, % (only for message)
	 eng_path/3]).

% ===========================================================================
:- doc(section, "Build of Engines").

:- use_module(engine(io_basic), [display/2]). % TODO: use messages lib

:- export(eng_build/1).
eng_build(Eng0) :-
	% Static dependencies
	eng_add_static_objs(Eng0, Eng),
	normal_message("compiling ~w (engine)", [~active_eng_name(exec, Eng)]),
	% Generate source
 	eng_emugen(Eng),
	eng_create_init(Eng),
	% Sysdep configuration
	eng_config_sysdep(Eng),
	% Version info
 	eng_prebuild_version_info(Eng),
	% Build native
 	eng_build_native(Eng).

% Add objects for static modules
eng_add_static_objs(Eng0, Eng) :-
	Eng0 = eng_def(Bundle, EngMainSpec, EngOpts0),
	( member(static_mods(StatMods), EngOpts0) ->
	    display(user_error, 'WARNING: static_mods(_) assumes that libraries are already compiled!\n'), % TODO: fix, enrich exprs
	    get_static(StatMods, StatBases, AddObjs),
	    EngOpts = [static_bases(StatBases), static_objs(AddObjs)|EngOpts0]
	; EngOpts = EngOpts0
	),
	Eng = eng_def(Bundle, EngMainSpec, EngOpts).

:- export(eng_clean/1).
% Clean the engine (including .pl sources)
eng_clean(Eng) :-
	% Ensure that byproducts of engine module compilation are
	% generated on next build.
	Base = ~eng_mainbase(Eng),
	clean_mod0(Base),
	% Clean the engine build area
	eng_clean_native(Eng).

% TODO: Merge with b_make_exec (this generates the C code for ciaoc
%   using the boostrap compiler; in this branch of Ciao this code is
%   reused for all other Ciao static executables)
%:- export(eng_emugen/1).
eng_emugen(Eng) :-
	Base = ~eng_mainbase(Eng),
	In = ~atom_concat(Base, '.pl'),
	path_split(In, Dir, _),
	invoke_boot_ciaoc(['-c', In], [cwd(Dir)]).

:- use_module(engine(stream_basic), [absolute_file_name/2]).
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

% ---------------------------------------------------------------------------
% Create engine initialization code

%:- export(eng_create_init/1).
% Generates engine initialization code
eng_create_init(Eng) :-
	F = ~path_concat(~eng_path(cdir, Eng), 'eng_static_mod.c'),
	EngOpts = ~eng_opts(Eng),
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
:- doc(section, "Sysdep configuration step (see config-sysdep.sh)").

% NOTE: Part of this code is implemented in builder/sh_code (since it
%   is needed for bootstrap)

:- use_module(engine(internals), [get_bundlereg_dir/2]).

:- export(bundle_flags_sh_file/1).
% File where eng_config_sysdep/1 expects the bundle flags (in .sh format)
% (see bundle_flags_file/2)
bundle_flags_sh_file := Path :-
	ciao_root(CiaoRoot),
	get_bundlereg_dir(CiaoRoot, BundleRegDir),
	path_concat(BundleRegDir, 'core.bundlecfg_sh', Path). % TODO: hardwired 'core'

%:- export(eng_config_sysdep/1).
% Generates 'meta_sh' and perform sysdep configuration (for this
% platform) on the specified engine based on the input configuration
% parameters.
%
% Precondition: the configuration values are loaded
%
% Output:
%   - <bld_engcfg>/config_mk: config for C compiler (for engine/Makefile)
%   - <bld_engcfg>/config_sh: sysdep config for engine (includes config for C compiler too)
%
eng_config_sysdep(Eng) :-
	CfgInput = ~bundle_flags_sh_file,
	EngDir = ~eng_path(engdir, Eng),
	EngCfg = ~eng_cfg(Eng),
	EngMetaSh = ~path_concat(~eng_path(cfgdir, Eng), 'meta_sh'),
	create_eng_meta_sh(Eng, CfgInput, EngMetaSh),
	sh_process_call(~config_sysdep_sh,
	       [EngDir, EngCfg],
	       [cwd(~bundle_path(core, '.'))]). % TODO: why? 'core' hardwired?

config_sysdep_sh := ~bundle_path(builder, 'sh_src/config-sysdep/config-sysdep.sh').

% Create meta_sh for config-sysdep and engine build
% TODO: At least $eng_name and $eng_h_alias should be created at the
%   same time than 'eng_info_mk'; the others are resolved names and
%   cannot; all should be customizable from ciaoengine.pl (in some
%   form).

create_eng_meta_sh(Eng, CfgInput, EngMetaSh) :-
	eng_meta_sh(Eng, CfgInput, Text, []),
	mkpath_and_write(Text, EngMetaSh).

% TODO: Document: many dirs with .c and .h are "materalized" into a single code view during build_engine.sh
eng_meta_sh(Eng, CfgInput) -->
	{ EngMainMod = ~eng_mainmod(Eng) },
	{ EngSrcDirs = ~eng_srcdirs(Eng) },
	{ EngOpts = ~eng_opts(Eng) },
	% engine name
	sh_def('eng_name', EngMainMod),
	% sub-directory in include/ for engine headers
	{ eng_h_alias(Eng, HAlias) },
	sh_def('eng_h_alias', HAlias), % TODO: allow many? (alias=path:alias=...)
	% directory for engine source (not autogenerated)
	% TODO: separate with ':' like paths?
	{ EngSrcPath = ~atom_concat_with_blanks(EngSrcDirs) },
	sh_def('eng_srcpath', EngSrcPath),
	% use STAT_LIBS (system static libs)
	( { member(add_stat_libs, EngOpts) } ->
	    sh_def('eng_use_stat_libs', yes)
	; sh_def('eng_use_stat_libs', no)
	),
	% cross compilation
	( { member(cross(CrossOs, CrossArch), EngOpts) } ->
	    sh_def('eng_cross_os', CrossOs),
	    sh_def('eng_cross_arch', CrossArch)
	; []
	),
	% default built-in CIAOROOT
	% TODO: add also directories for patching paths for so/dylibs?
	( { member(default_ciaoroot(DefCiaoRoot), EngOpts) } ->
	    sh_def('eng_default_ciaoroot', DefCiaoRoot)
	; []
	),
	% static .a for foreign code
	{ member(static_objs(AddObjs0), EngOpts) ->
            AddObjs = ~atom_concat_with_blanks(AddObjs0)
	; AddObjs = ''
	},
	sh_def('eng_addobj', AddObjs),
	% extra configuration (config_sh) for static foreign code
	{ member(static_cfgs(AddCfgs0), EngOpts) ->
            AddCfgs = ~atom_concat_with_blanks(~cfgdirs(AddCfgs0))
	; AddCfgs = ''
	},
	sh_def('eng_addcfg', AddCfgs),
	% Input for configuration
	sh_def('eng_core_config', CfgInput).

cfgdirs([], []).
cfgdirs([at_bundle(B, Spec)|Xs], [Y|Ys]) :-
	Eng = eng_def(B, Spec, []), % TODO: not necessarily an engine! fix
	Y = ~eng_path(cfgdir, Eng),
	cfgdirs(Xs, Ys).

% TODO: Missing escape (if needed)
sh_def(Var,Val) -->
	atm(Var),
	"=\"", atm(Val),
	"\"\n".

atm(X) --> { atom_codes(X, Cs) }, str(Cs).

str([]) --> [].
str([C|Cs]) --> [C], str(Cs).

% TODO: duplicated
atom_concat_with_blanks(L) := ~atom_concat(~separate_with_blanks(L)).

separate_with_blanks([]) := [] :- !.
separate_with_blanks([A]) := [A] :- !.
separate_with_blanks([A, B|Cs]) := [A, ' '|~separate_with_blanks([B|Cs])] :- !.

% ===========================================================================
:- doc(section, "Version info for engine").

:- use_module(library(bundle/bundle_info), [bundle_version/2]).
:- use_module(library(version_strings), [version_split_patch/3]).
:- use_module(ciaobld(bundle_hash), [bundle_commit_info/3]).

% TODO: see generate_version_auto/2

%:- export(eng_prebuild_version_info/1).
% Prepare version.c and version.h with version information.
%
% TODO: Write the part that generates the commit/version info in
%   Prolog -- write a dummy version for bootstrapping
%
eng_prebuild_version_info(Eng) :-
	VerBundle = core, % bundle used to extract version
	gen_eng_version_h(VerBundle, Eng),
	gen_eng_version_c(VerBundle, Eng).

gen_eng_version_h(VerBundle, Eng) :-
	H = ~path_concat(~eng_path(hdir, Eng), ~eng_h_alias(Eng)),
	VerH = ~path_concat(H, 'version.h'),
	( file_exists(VerH) -> % TODO: update file if contents change
	    true
	; version_h(VerBundle, TextH, []),
	  mkpath_and_write(TextH, VerH)
	).

gen_eng_version_c(VerBundle, Eng) :-
	VerC = ~path_concat(~eng_path(cdir, Eng), 'version.c'),
	( file_exists(VerC) -> % TODO: update file if contents change
	    true
	; version_c(VerBundle, TextC, []),
	  mkpath_and_write(TextC, VerC)
	).

version_h(Bundle) -->
	"#define CIAO_VERSION_STRING \"Ciao \" ",
	version_h_(Bundle),
	"\n".

% TODO: Do not include commit info? Make version optional?
version_h_(Bundle) -->
	{ CommitDate = ~bundle_commit_info(Bundle, date) },
	{ CommitDesc = ~bundle_commit_info(Bundle, desc) },
	{ \+ CommitDesc = 'Unknown' },
	!,
	"\"", atm(CommitDesc), "\" \" (\" \"", atm(CommitDate), "\" \")\"".
version_h_(Bundle) -->
	% No commit info
	{ Version = ~bundle_version(Bundle) },
	"\"", atm(Version), "\"".

version_c(Bundle) -->
	{ Version = ~bundle_version(Bundle) },
	{ version_split_patch(Version, VersionNopatch, Patch) },
	{ CommitBranch = ~bundle_commit_info(Bundle, branch) },
	{ CommitId = ~bundle_commit_info(Bundle, id) },
	{ CommitDate = ~bundle_commit_info(Bundle, date) },
	{ CommitDesc = ~bundle_commit_info(Bundle, desc) },
	% TODO: Use just version? (do not define ciao_patch, commit info, etc.)
	"char *ciao_version = \"", atm(VersionNopatch), "\";\n",
	"char *ciao_patch = \"", atm(Patch), "\";\n",
	"char *ciao_commit_branch = \"", atm(CommitBranch), "\";\n",
	"char *ciao_commit_id = \"", atm(CommitId), "\";\n",
	"char *ciao_commit_date = \"", atm(CommitDate), "\";\n",
	"char *ciao_commit_desc = \"", atm(CommitDesc), "\";\n".

% ===========================================================================
:- doc(section, "Interface to SH implementation").

% Commands for building engines
% (creates the engine build area implicitly)
eng_build_native(Eng) :-
	build_engine_(Eng, build, []).

% Clean the engine build area
eng_clean_native(Eng) :-
	build_engine_(Eng, clean, []).

build_engine_(Eng, Target, Env0) :-
	EngDir = ~eng_path(engdir, Eng),
	EngCfg = ~eng_cfg(Eng),
	% (input: look at build_engine.sh)
	Env = ['BLD_ENGDIR' = EngDir,
	       'ENG_CFG' = EngCfg|Env0],
	sh_process_call(~build_engine_sh, [Target], [env(Env)]).

:- use_module(engine(internals), [ciao_root/1]).

% TODO: could I use bundle_path(builder, 'sh_src/build_engine.sh')? (it should be registered)
build_engine_sh := Path :-
	ciao_root(CiaoRoot),
	Path = ~path_concat(CiaoRoot, 'builder/sh_src/build_engine.sh').

% ---------------------------------------------------------------------------
:- doc(section, "Detect OS and architecture").
% TODO: This may not need to execute sh code (we can precompute on bootstrap)

:- export(sysconf_os/1).
% Detect the current operating system.
sysconf_os(OS) :-
	get_sysconf(['--os'], OS).

% (See scan_bootstrap_opts.sh)

:- export(sysconf_arch/3).
% Detect the current architecture. Select 32 or 64 bits if M32 or M64
% is 'yes', respectively.
sysconf_arch(M32, M64, Arch) :-
	get_sysconf(['--arch'], Arch0),
	( M32 = yes -> arch32(Arch0, Arch)
	; M64 = yes -> arch64(Arch0, Arch)
	; Arch = Arch0
	).

arch32('Sparc64', 'Sparc') :- !.
arch32('x86_64', 'i686') :- !.
arch32('ppc64', 'ppc') :- !.
arch32(Arch, Arch). % assume 32-bit

arch64('Sparc64', 'Sparc64') :- !.
arch64('x86_64', 'x86_64') :- !.
arch64('ppc64', 'ppc64') :- !.
arch64(_, empty). % force error % TODO: emit error instead?

ciao_sysconf_sh := ~bundle_path(builder, 'sh_src/config-sysdep/ciao_sysconf').

get_sysconf(Args, Val) :-
	sh_process_call(~ciao_sysconf_sh, Args,
	                [stderr(stdout), stdout(string(Val0)), status(_)]),
	atom_codes(Val, Val0).

% ---------------------------------------------------------------------------

:- use_module(library(pathnames), [path_dirname/2]).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(stream_utils), [string_to_file/2]).

% Write Text to F, make sure that the path exists
mkpath_and_write(Text, F) :-
	path_dirname(F, D),
	mkpath(D),
	string_to_file(Text, F).

