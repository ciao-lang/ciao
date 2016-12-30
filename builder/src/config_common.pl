:- module(config_common, [], [assertions, fsyntax]).

:- doc(title, "Commonly used config flags").
:- doc(author, "Ciao Development Team").
:- doc(author, "Jose F. Morales").

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(pathnames), [path_concat/3]).

:- use_module(library(bundle/bundle_paths), [bundle_path/3, bundle_path/4]).
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
% Enable docs grade (lpdoc must be available)
with_docs := ~get_bundle_flag(ciao:with_docs) :- !, '$bundle_id'(lpdoc).
with_docs := no.

% ===========================================================================

% Default engine
% TODO: Generalize a-la optim_comp for arbitrary native code execs
% TODO: Generalize with options, flags, etc.
:- export(default_eng_def/1).
default_eng_def(Eng) :-
	Eng = eng_def(core, 'engine/ciaoengine', []).

% Boot engine
:- export(boot_eng_def/1).
boot_eng_def(Eng) :-
	Eng = eng_def(core, 'engine/ciaoengine', [boot]).

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
	R = ~path_concat(~instciao_storedir, ~concat_ver(Bundle, Bundle)).

% ===========================================================================
% Paths for build/instalation items

:- export(bld_cmd_path/4).
% Executable path in build area
bld_cmd_path(Bundle, Kind, File) := Path :-
	BinDir = ~bundle_path(Bundle, builddir, 'bin'),
	Path = ~concat_ext(Kind, ~path_concat(BinDir, File)).

:- export(inst_cmd_path/4).
% Executable path in global installs
% (e.g., <prefix>/bin/ciaoc-1.15)
inst_cmd_path(Bundle, Kind, File) := Path :-
	BinDir = ~instciao_bindir,
	Path = ~path_concat(BinDir, ~concat_ext(Kind, ~concat_ver(Bundle, File))).

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
	CmdName0 = ~concat_ver(Bundle, Cmd),
	!,
	CmdName = ~concat_ext(K, CmdName0).
cmdname_ver(_UseVers, _Bundle, Cmd, K, CmdName) :-
	CmdName = ~concat_ext(K, Cmd).

:- use_module(library(pathnames), [path_split/3]).
:- use_module(library(system), [file_exists/2]).

:- export(cmd_path/4).
% Executable path in local build area or installed (if the running
% builder is globally installed)
cmd_path(_Bundle, Kind, File) := Path :-
	builder_in_global, % TODO: not for bundles in (non-installed) workspaces
	!,
	% E.g., '.../lib/ciao/core-M.N' -> '.../bin/...'
	% TODO: use inst_cmd_path instead?
	ciao_lib_dir(LibDir),
	path_split(LibDir, Dir0, _),
	path_split(Dir0, Dir1, _),
	path_split(Dir1, Dir2, _),
	path_concat(Dir2, 'bin', Path0),
	path_concat(Path0, File, Path1),
	Path = ~concat_ext(Kind, Path1).
cmd_path(Bundle, Kind, File) := Path :-
	Path = ~bld_cmd_path(Bundle, Kind, File).

:- export(local_ciaolib/1).
% CIAOLIB value for local build area or installed (if the running
% builder is globally installed)
% TODO: trust the engine built-in value?
local_ciaolib := Path :- builder_in_global, !,
	ciao_lib_dir(Path).
local_ciaolib := Path :-
	Path = ~bundle_path(core, '.').

:- export(builder_in_global/0).
% (heuristic to detect running from a global installation)
% TODO: see bundlereg_load.pl
builder_in_global :-
	ciao_lib_dir(LibDir),
	path_concat(LibDir, 'bundlereg', BundleRegDir0),
	file_exists(BundleRegDir0, 0).

% ---------------------------------------------------------------------------

:- export(active_cmd_path/3).
% Executable path in global installs for the active version
% (e.g., <prefix>/bin/ciaoc)
active_cmd_path(Kind, File) := Path :-
	Path = ~concat_ext(Kind, ~path_concat(~instciao_bindir, File)).

% ---------------------------------------------------------------------------

:- use_module(engine(internals), ['$bundle_prop'/2, '$bundle_id'/1]).
:- use_module(library(version_strings), [version_split_patch/3]).

% TODO: check if this helps
:- data version_nopatch_/2.
version_nopatch(Bundle, V) :-
	( version_nopatch_(Bundle, V0) -> V = V0
	; '$bundle_prop'(Bundle, version(Version)) ->
	    version_split_patch(Version, V0, _),
	    assertz_fact(version_nopatch_(Bundle, V0)),
	    V = V0
	; fail
	).

:- export(concat_ver/3).
% Obtain 'A-Ver' where Ver is the version of Bundle.
concat_ver(Bundle, A) := ~atom_concat([A, '-', ~version_nopatch(Bundle)]).

:- export(concat_ext/3).
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



