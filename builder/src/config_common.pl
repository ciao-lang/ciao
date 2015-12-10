:- module(config_common, [], [assertions, fsyntax]).

% TODO: Not the right name now
:- doc(title, "Commonly used config flags").
:- doc(author, "Ciao Development Team").
:- doc(author, "Jose F. Morales").

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(pathnames), [path_concat/3]).

:- use_module(library(bundle/paths_extra), [fsR/2]).
:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).

% ===========================================================================

:- export(verbose_build/1).
verbose_build := ~get_bundle_flag(ciao:verbose_build).

% ===========================================================================
% The URL and directory for our main distribution site

% (Not configurable)

:- export(home_url_str/1).
home_url_str := "http://ciao-lang.org/".
% TODO: Wrong
:- export(packages_dir_str/1).
packages_dir_str := "packages/master/".

% ===========================================================================

:- export(perms/1).
% Define this to be the permissions for installed execs/dirs and data files:
perms(perms(rwX, rwX, rX)).

% ===========================================================================

:- export(with_docs/1).
with_docs := ~get_bundle_flag(ciao:with_docs).

% ===========================================================================

% Default engine main module
% TODO: Generalize a-la optim_comp for arbitrary native code execs
% TODO: Generalize with options, flags, etc.
:- export(default_eng/1).
default_eng('ciaoengine').

:- export(eng_h_alias/2).
% Alias for .h files (for #include directives from C)
% (E.g., "#include <ciao/...>")
eng_h_alias(ciaoengine, 'ciao') :- !. % TODO: Fix, hardwired!
eng_h_alias(EngMainMod, EngMainMod).

% ===========================================================================

% NOTE: Synchronize with build_engine.sh definitions for bootstrap

:- use_module(engine(system_info), [get_so_ext/1, get_a_ext/1]).

%:- export(rel_eng_path/4).
%
% Directory layout for an engine build:
%   hdir: C headers
%   cdir: C source files 
%   cfgdir: sysdep config files
%   objdir: sysdep compiled object files
%   engdir: root directory for engine build
%   exec: engine as an executable % TODO: add to some engine bindir?
%   lib_so: engine as a C shared library % TODO: missing in installation
%   lib_a: engine as a C static library % TODO: missing in installation
% (for engine activation)
%   objdir_anyarch: compiled object files root
%   exec_anyarch: engine as an executable -- link to active architecture % TODO: add to some engine bindir?
%
% TODO: Add exec_sh and exec_bat (for exec_headers)
rel_eng_path(D, BldId, EngMainMod) := Path :-
	eng_exec(D, EngMainMod, BaseD, K, Base),
	!,
	Path = ~concat_ext(K, ~path_concat(~rel_eng_path2(BaseD, BldId), Base)).
rel_eng_path(D, BldId, _) := ~rel_eng_path2(D, BldId).

rel_eng_path2(cfgdir, BldId) := ~path_concat('cfg', ~get_eng_cfg(BldId)) :- !.
rel_eng_path2(objdir, BldId) := ~path_concat('objs', ~get_eng_cfg(BldId)) :- !.
rel_eng_path2(D, _) := ~rel_eng_path1(D).

rel_eng_path1(engdir) := ''.
rel_eng_path1(cdir) := 'src'.
rel_eng_path1(hdir) := 'include'.
rel_eng_path1(objdir_anyarch) := 'objs'.

eng_exec(exec, M, objdir, exec, M).
eng_exec(lib_so, M, objdir, ext(~get_so_ext), ~atom_concat('lib', M)).
eng_exec(lib_a, M, objdir, ext(~get_a_ext), ~atom_concat('lib', M)).

% ---------------------------------------------------------------------------

% Identifier for this build (platform + debug)
:- export(get_eng_cfg/2).
get_eng_cfg(BldId) := EngCfg :-
	OsArch = ~get_platform, % TODO: fix cross-compilation (this is the runtime value)
	( BldId = bootbuild ->
	    EngCfg = OsArch % (special case, see 'boot_eng_cfg' in builder_boot.sh)
	; DebugLevel = ~debug_level,
	  ( DebugLevel = 'nodebug' -> EngCfg = OsArch
	  ; EngCfg = ~atom_concat([OsArch, '-', DebugLevel])
	  )
	).

debug_level := ~get_bundle_flag(core:debug_level).

% ===========================================================================
% Paths for installation

:- export(instype/1).
instype := ~get_bundle_flag(ciao:instype).

:- export(instciao_prefix/1).
% Prefix for installation directories
% (e.g., if /usr/local, binaries will be installed in /usr/local/bin)
instciao_prefix := ~get_bundle_flag(ciao:install_prefix).

:- export(instciao_bindir/1).
% Directory for installation of binaries
% (default <prefix>/bin)
instciao_bindir := ~get_bundle_flag(ciao:install_bindir).

% Base directory for the Ciao store dir (for bundles)
% (default <prefix>/lib)
instciao_libdir := ~get_bundle_flag(ciao:install_libdir).

:- export(instciao_storedir/1).
% Directory for installing bundles
% E.g., <prefix>/lib/ciao
instciao_storedir := ~path_concat(~instciao_libdir, 'ciao').

% Directory for installation of bundle Bundle (under storedir)
% E.g., <prefix>/lib/ciao/<bundle+ver>
%
% NOTE: May conflict with installed engines
%
:- export(instciao_bundledir/2).
instciao_bundledir(Bundle) := R :-
	R = ~concat_ver(Bundle, ~path_concat(~instciao_storedir, Bundle)).

% ===========================================================================
% Paths for build/instalation items

:- export(bld_eng_path/4).
% Engine directory layout in build area
bld_eng_path(D, BldId, EngMainMod) := Path :-
	EngDir = ~fsR(builddir(BldId)/eng/EngMainMod),
	Rel = ~rel_eng_path(D, BldId, EngMainMod),
	( Rel = '' -> Path = EngDir
	; Path = ~path_concat(EngDir, Rel)
	).

:- export(inst_eng_path/4).
% Engine directory layout in global installs
%
% TODO: Missing installation of lib_a, lib_so, and probably some of the configuration files?
%
inst_eng_path(D, Bundle, EngMainMod) := Path :-
	% E.g., for <prefix>/lib/ciao/ciaoengine-1.15
	InstEngDir = ~concat_ver(Bundle, ~path_concat(~instciao_storedir, EngMainMod)),
	Rel = ~rel_eng_path(D, build, EngMainMod),
	( Rel = '' -> Path = InstEngDir
	; Path = ~path_concat(InstEngDir, Rel)
	).

:- export(bld_cmd_path/4).
% Executable path in build area
bld_cmd_path(BldId, Kind, File) := Path :-
	Path = ~concat_ext(Kind, ~fsR(builddir_bin(BldId)/File)).

:- export(inst_cmd_path/4).
% Executable path in global installs
% (e.g., <prefix>/bin/ciaoc-1.15)
inst_cmd_path(Bundle, Kind, File) := Path :-
	Path = ~concat_verk(Bundle, Kind, ~path_concat(~instciao_bindir, File)).

% TODO: install binaries to storedir, e.g., <prefix>/lib/ciao/core-1.15/bin/ and
%   deprecate the uses of this predicate
:- export(cmdname_ver/5).
% cmdname_ver(+UseVers, +Bundle, +Cmd, +K, -Name):
%   Name of a command Cmd from a Bundle, with version (if UseVers=yes)
%
% Version is added if instype(global) and the bundle is known.
cmdname_ver(yes, Bundle, Cmd, K, CmdName) :-
	instype(global),
	'$bundle_id'(Bundle), % (Bundle exists)
	CmdName0 = ~concat_verk(Bundle, K, Cmd),
	!,
	CmdName = CmdName0.
cmdname_ver(_UseVers, _Bundle, Cmd, K, CmdName) :-
	CmdName = ~concat_ext(K, Cmd).

% ---------------------------------------------------------------------------

:- export(active_bld_eng_path/3).
% Paths for the active engine and multi-platform engine selection
% (in global installation).
%
% TODO: define properly the 'activation' operation
active_bld_eng_path(D, EngMainMod) := Path :-
	Name = ~active_eng_name(D, EngMainMod),
	Path = ~path_concat(~bld_eng_path(objdir_anyarch, build, EngMainMod), Name).

:- export(active_inst_eng_path/3).
% Paths for the active engine and multi-platform engine selection
% (in global installation).
%
% TODO: define properly the 'activation' operation
active_inst_eng_path(D, EngMainMod) := Path :-
	Name = ~active_eng_name(D, EngMainMod),
	Path = ~path_concat(~instciao_storedir, Name).

% Active engine name (for multi-platform build and installation)
active_eng_name(D, EngMainMod) := Name :-
	( D = exec ->         % E.g., ciaoengine.<OSARCH>
	    Name0 = ~atom_concat([EngMainMod, '.', ~get_eng_cfg(build)])
	; D = exec_anyarch -> % E.g., ciaoengine
	    Name0 = EngMainMod
	; fail
	),
	Name = ~concat_ext(exec, Name0).

:- export(active_cmd_path/3).
% Executable path in global installs for the active version
% (e.g., <prefix>/bin/ciaoc)
active_cmd_path(Kind, File) := Path :-
	Path = ~concat_ext(Kind, ~path_concat(~instciao_bindir, File)).

% ---------------------------------------------------------------------------

:- use_module(engine(internals), ['$bundle_prop'/2, '$bundle_id'/1]).

concat_verk(Bundle, Kind, B) := R :-
	R = ~concat_ext(Kind, ~concat_ver(Bundle, B)).

% Obtain 'A-Ver' where Ver is the version of Bundle.
concat_ver(Bundle, A) := ~atom_concat([A, '-', Vers]) :-
	'$bundle_prop'(Bundle, version(Vers)).

% Obtain 'A'+'Ext' where Ext is the default extension (For the current
% architecture) for each kind:
%
%  - 'plexe': ciao executables
%  - 'exec': native executables
%  - 'shscript': shell scripts
%  - 'ext(Ext)': custom extension 'Ext'
%
concat_ext(plexe, X) := ~atom_concat(X, ~get_ciao_ext).
concat_ext(exec, X) := ~atom_concat(X, ~get_exec_ext).
concat_ext(shscript, X) := X.
concat_ext(ext(Ext), X) := ~atom_concat(X, Ext).

% ---------------------------------------------------------------------------

:- export(localciao_env/3). % TODO: merge
:- pred localciao_env(BldId, EngMainMod, Env) # "@var{Env} is the
   environment (used by @pred{process_call/3}) that activates the Ciao
   build at @var{BldId} using the engine @var{EngMainMod}".

localciao_env(BldId, EngMainMod) := Env :-
	Env = ['CIAOALIASPATH' = '',
	       'CIAOLIB' = ~fsR(bundle_src(core)),
	       'CIAOHDIR' = ~bld_eng_path(hdir, BldId, EngMainMod),
	       'CIAOENGINE' = ~bld_eng_path(exec, BldId, EngMainMod)].

