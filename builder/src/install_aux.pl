:- module(_, [], [assertions, fsyntax, hiord]).

:- doc(title, "Installation").
:- doc(author, "Ciao Development Team").

:- doc(module, "This module implements basic operations for installing
   and uninstalling the contents of a bundle (source and binaries).

   The installation copy files from the build area to the installation
   area (@tt{inst_ciao_root}) and (optionally and if needed) creates
   activation symbolic links from standard installation directories
   (e.g., @tt{/usr/local/bin}).

   It supports stagged installations to temporary locations (see
   @pred{rootprefix/1}).").

:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3, bundle_path/4]).
:- use_module(library(messages), [warning_message/2]).
:- use_module(library(source_tree), [copy_file_tree/4]).
:- use_module(library(system_extra), [mkpath/2, create_rel_link/2]).
:- use_module(library(system_extra), [ignore_nosuccess/1, warn_on_nosuccess/1]).
:- use_module(library(system), [delete_directory/1]).

:- use_module(ciaobld(builder_aux), [remove_dir_nofail/1]). % TODO: system?
:- use_module(ciaobld(config_common), [concat_ext/3, cmd_path/4]).

% ===========================================================================
:- doc(section, "Paths for installation").

:- use_module(engine(internals), ['$bundle_prop'/2]).
:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).

:- export(perms/1).
% Define this to be the permissions for installed execs/dirs and data files:
perms(perms(rwX, rwX, rX)).

:- export(instype/1).
instype := ~get_bundle_flag(builder:instype).

%:- export(inst_ciao_root_base/1).
% Base directory for CIAOROOT in global installation
% (default <prefix>/ciao)
inst_ciao_root_base := ~notnull(~get_bundle_flag(builder:install_ciaoroot_base)).

notnull('') := _ :- !, throw(bug_invalid_null_atom).
notnull(X) := X.

% TODO: allow customization!
% TODO: cache?
:- export(inst_ciao_root/1).
% CIAOROOT for global installation (separted by the system version)
% (aka INST_CIAOROOT)
% E.g., <inst_ciao_root_base>/VERS
inst_ciao_root := Path :-
	Path0 = ~inst_ciao_root_base,
	( % TODO: allow 'master' or 'HEAD'
          '$bundle_prop'(core, version(Version)) ->
	    path_concat(Path0, Version, Path)
	; throw(core_no_version)
	).

:- export(inst_bundle_path/3).
% (like bundle_path/3 but relative to INST_CIAOROOT)
inst_bundle_path(Bundle, Rel) := R :-
	R0 = ~path_concat(~inst_ciao_root, Bundle),
	( Rel = '.' ->
	    R = R0
	; R = ~path_concat(R0, Rel)
	).

:- export(inst_builddir_path/2).
% Subpath of INST_CIAOROOT/build
inst_builddir_path(Rel) := R :-
	R0 = ~path_concat(~inst_ciao_root, 'build'),
	( Rel = '.' ->
	    R = R0
	; R = ~path_concat(R0, Rel)
	).

:- export(inst_cmd_path/4).
% Executable path in global installations
% (e.g., INST_CIAOROOT/build/bin/ciaoc)
inst_cmd_path(_Bundle, Kind, File) := Path :-
	BinDir = ~inst_builddir_path('bin'),
	Path = ~path_concat(BinDir, ~concat_ext(Kind, File)).

% ===========================================================================
:- doc(section, "Final paths (depending on instype value)").

:- use_module(engine(internals), [ciao_root/1]).

:- export(final_ciao_root/1).
final_ciao_root := Path :-
	( instype(local) ->
	    Path = ~ciao_root
	; Path = ~inst_ciao_root
	).

:- export(final_cmd_path/4).
final_cmd_path(Bundle, Kind, File) := Path :-
	( instype(local) ->
	    Path = ~cmd_path(Bundle, Kind, File)
	; Path = ~inst_cmd_path(Bundle, Kind, File)
	).

:- export(final_bundle_path/3).
final_bundle_path(Bundle, Rel) := Path :-
	( instype(local) ->
	    Path = ~bundle_path(Bundle, Rel)
	; Path = ~inst_bundle_path(Bundle, Rel)
	).

:- export(final_builddir_path/3).
final_builddir_path(Bundle, Rel) := Path :-
	( instype(local) ->
	    Path = ~bundle_path(Bundle, builddir, Rel)
	; Path = ~inst_builddir_path(Rel) % TODO: Bundle ignored! This may be a problem if we have a global installation that installs bundles locally
	).

% ===========================================================================
:- doc(section, "Paths for activation").

:- export(active_bindir/1).
% Directory for activation of binaries
% (default <prefix>/bin)
active_bindir := ~notnull(~get_bundle_flag(builder:bindir)).

:- export(active_docdir/2).
% Paths for doc activation
active_docdir(manl) := ~notnull(~get_bundle_flag(builder:mandir)).
active_docdir(info) := ~notnull(~get_bundle_flag(builder:infodir)).
% (other docs are kept in builddir, see link_docformat/1)

:- export(active_cmd_path/3).
% Executable path in global installations for the active version
% (e.g., <prefix>/bin/ciaoc -> INST_CIAOROOT/build/bin/ciaoc)
active_cmd_path(Kind, File) := Path :-
	Path = ~concat_ext(Kind, ~path_concat(~active_bindir, File)).

% ---------------------------------------------------------------------------
% Support for versioned executable names (currently deprecated)

%% :- use_module(engine(internals), ['$bundle_prop'/2, '$bundle_id'/1]).
%% :- use_module(library(version_strings), [version_split_patch/3]).
%%
%% :- data version_nopatch_/2.
%% version_nopatch(Bundle, V) :-
%% 	( version_nopatch_(Bundle, V0) -> V = V0
%% 	; '$bundle_prop'(Bundle, version(Version)) ->
%% 	    version_split_patch(Version, V0, _),
%% 	    assertz_fact(version_nopatch_(Bundle, V0)),
%% 	    V = V0
%% 	; fail
%% 	).
%%
%% :- export(concat_ver/3).
%% % Obtain 'A-Ver' where Ver is the version of Bundle.
%% concat_ver(Bundle, A) := ~atom_concat([A, '-', ~version_nopatch(Bundle)]).

% ===========================================================================
:- doc(section, "Installation/uninstallation of directory layout and registry files").

:- export(install_bin_dirs/1).
install_bin_dirs(Bundle) :-
	instdir_install(dir(~inst_ciao_root_base)),
	instdir_install(dir(~inst_ciao_root)),
	instdir_install(dir(~inst_bundle_path(Bundle, '.'))).

:- export(uninstall_bin_dirs/1).
uninstall_bin_dirs(Bundle) :-
	instdir_uninstall(dir_rec(~inst_bundle_path(Bundle, '.'))),
	instdir_uninstall(dir_if_empty(~inst_ciao_root)),
	instdir_uninstall(dir_if_empty(~inst_ciao_root_base)).

% ---------------------------------------------------------------------------
% Bundle registry and flags installation

% (this rewrites bundlereg with updated paths)

:- use_module(engine(internals), [bundlereg_filename/3]).
:- use_module(engine(internals), [get_bundlereg_dir/2]).
:- use_module(library(bundle/bundle_flags),
	[bundle_flags_file/2, bundlecfg_filename/3]).

:- use_module(ciaobld(manifest_compiler), [make_bundlereg/4]). % TODO: implement relocate?

:- export(install_bundlereg/1).
install_bundlereg(Bundle) :-
	% Create a bundlereg for bundle at @var{BundleDir} pointing at
	% @var{FinalBundleDir}
	BundleDir = ~bundle_path(Bundle, '.'),
	FinalBundleDir = ~inst_bundle_path(Bundle, '.'),
	%
	BundleRegDir = ~inst_bundlereg_dir,
	instdir_install(dir(BundleRegDir)),
	%
	RegFile = ~bundlereg_filename(Bundle, BundleRegDir),
	make_bundlereg(Bundle, BundleDir, FinalBundleDir, ~rootprefixed(RegFile)),
	%
	CfgFile = ~bundle_flags_file(Bundle),
	InsCfgFile = ~bundlecfg_filename(Bundle, BundleRegDir),
	instdir_install(file(CfgFile, InsCfgFile)).

:- export(uninstall_bundlereg/1).
uninstall_bundlereg(Bundle) :-
	BundleRegDir = ~inst_bundlereg_dir,
	RegFile = ~bundlereg_filename(Bundle, BundleRegDir),
	InsCfgFile = ~bundlecfg_filename(Bundle, BundleRegDir),
	instdir_uninstall(file(RegFile)),
	instdir_uninstall(file(InsCfgFile)).

%:- export(inst_bundlereg_dir/1).
% bundlereg dir for global instype
inst_bundlereg_dir(BundleRegDir) :-
	inst_ciao_root(Wksp),
	get_bundlereg_dir(Wksp, BundleRegDir).

% ===========================================================================
:- doc(section, "Installation/uninstallation of files and directories").

:- use_module(ciaobld(builder_flags), [get_builder_flag/2]).

:- export(rootprefix/1).
:- pred rootprefix(DestDir) # "@var{DestDir} is the prefix that is
   prepended to each (un)install target (useful for packaged bundle
   creation)".

% TODO: rename to install_destdir
rootprefix(R) :-
	( get_builder_flag(destdir, Value) ->
	    R = Value
	; R = ''
	).

%:- export(rootprefixed/2).
% Add rootprefix (flag 'destdir') to the given path (for installation)
rootprefixed(Path0) := Path :-
	% (note: Path0 is an absolute path, do not use path_concat/3)
	Path = ~atom_concat(~rootprefix, Path0).

:- export(instdir_install/1).
% Creates a directory in the installation area
instdir_install(dir(Dir0)) :-
	Dir = ~rootprefixed(Dir0),
	( mkpath(Dir, ~perms) -> % TODO: owner?
	    true
	; throw(error_msg("Could not create ~w", [Dir]))
	).
% (copy all)
instdir_install(dir_rec(FromDir, ToDir)) :-
	copy_file_tree(installable_precomp(full),
	               FromDir, ~rootprefixed(ToDir), ~perms).
% (copy all except .po and .itf)
instdir_install(src_dir_rec(FromDir, ToDir)) :-
	copy_file_tree(installable_precomp(src),
	               FromDir, ~rootprefixed(ToDir), ~perms).
%
instdir_install(bin_file(From, To0)) :-
	To = ~rootprefixed(To0),
	install_bin_file(From, To).
%
instdir_install(file(From, To0)) :-
	To = ~rootprefixed(To0),
	install_file(From, To).
%
instdir_install(lib_file(Bundle, Path)) :-
	From = ~bundle_path(Bundle, Path),
	To = ~rootprefixed(~inst_bundle_path(Bundle, Path)),
	install_file(From, To).
%
instdir_install(cmd_copy_and_link(Kind, Bundle, File)) :-
	% install a copy and an activation link
	instdir_install(cmd_copy(Kind, Bundle, File)),
	instdir_install(cmd_link_as(Kind, Bundle, File, File)).
%
instdir_install(cmd_copy(Kind, Bundle, File)) :-
	From = ~cmd_path(Bundle, Kind, File),
	To = ~rootprefixed(~inst_cmd_path(Bundle, Kind, File)),
	instdir_install(dir(~inst_builddir_path('bin'))),
	install_bin_file(From, To).
%
instdir_install(cmd_link_as(Kind, Bundle, Src, Dest)) :-
	% TODO: move to 'activation' operation?
	From = ~rootprefixed(~inst_cmd_path(Bundle, Kind, Src)),
	To = ~rootprefixed(~active_cmd_path(Kind, Dest)),
	instdir_install(dir(~active_bindir)),
	ignore_nosuccess(create_rel_link(From, To)).
%
% TODO: separate 'ciao-config' exec to get options for CC/LD?
% TODO: control engine 'activation' operation?
%
instdir_install(eng_contents(Eng)) :- !,
	% Install engine (including C headers)
	instdir_install(dir(~inst_eng_path(engdir, Eng))), % TODO: set_file_perms or set_exec_perms?
	%
	LocalEng = ~eng_path(exec, Eng),
	InstEng = ~inst_eng_path(exec, Eng),
	% Install exec
	instdir_install(dir(~inst_eng_path(objdir_anyarch, Eng))), % TODO: set_file_perms or set_exec_perms?
	instdir_install(dir(~inst_eng_path(objdir, Eng))), % TODO: set_file_perms or set_exec_perms?
	instdir_install(bin_file(LocalEng, InstEng)),
	% Install headers
        HDir = ~eng_path(hdir, Eng),
	InstEngHDir = ~inst_eng_path(hdir, Eng),
	instdir_install(src_dir_rec(HDir, InstEngHDir)).
instdir_install(eng_active(Eng)) :- !, % Activate engine (for multi-platform)
	eng_active_inst(Eng).

:- export(instdir_uninstall/1).
instdir_uninstall(cmd_copy_and_link(Kind, Bundle, File)) :-
	instdir_uninstall(cmd_link(Kind, File)),
	instdir_uninstall(cmd_copy(Kind, Bundle, File)).
%
instdir_uninstall(cmd_link(Kind, File)) :-
	instdir_uninstall(file(~active_cmd_path(Kind, File))).
%
instdir_uninstall(cmd_copy(Kind, Bundle, File)) :-
	instdir_uninstall(file(~inst_cmd_path(Bundle, Kind, File))).
%
instdir_uninstall(file(File)) :-
	del_file_nofail(~rootprefixed(File)).
%
instdir_uninstall(dir_rec(Dir)) :-
	safe_remove_dir_nofail(Dir).
%
instdir_uninstall(src_dir_rec(Dir)) :-
	safe_remove_dir_nofail(Dir).
%
instdir_uninstall(dir(Dir)) :-
	warn_on_nosuccess(delete_directory(~rootprefixed(Dir))).
%
instdir_uninstall(dir_if_empty(Dir)) :-
	ignore_nosuccess(delete_directory(~rootprefixed(Dir))).
%
instdir_uninstall(lib_file(Bundle, Path)) :-
	To = ~inst_bundle_path(Bundle, Path),
	instdir_uninstall(file(To)).
instdir_uninstall(eng_contents(Eng)) :- !,
	% Uninstall engine
	instdir_uninstall(file(~inst_eng_path(exec, Eng))),
        % Uninstall C headers
	InstEngHDir = ~inst_eng_path(hdir, Eng),
	instdir_uninstall(src_dir_rec(InstEngHDir)),
	%
	instdir_uninstall(dir_if_empty(~inst_eng_path(objdir, Eng))),
	instdir_uninstall(dir_if_empty(~inst_eng_path(objdir_anyarch, Eng))),
	instdir_uninstall(dir_if_empty(~inst_eng_path(engdir, Eng))).
instdir_uninstall(eng_active(Eng)) :- !,
	% TODO: only if it coincides with the active version (better, do unactivation before)
	instdir_uninstall(file(~active_inst_eng_path(exec, Eng))),
	instdir_uninstall(file(~active_inst_eng_path(exec_anyarch, Eng))).

:- use_module(library(pathnames), [path_get_relative/3]).

% Remove dir recursively (with some additional safety checks)
safe_remove_dir_nofail(Dir) :-
	( path_get_relative(~inst_ciao_root, Dir, _) ->
	    % Inside inst_ciao_root
	    remove_dir_nofail(~rootprefixed(Dir))
	; % TODO: use a install_manifest.txt file (common practice)
          warning_message("Refusing to remove directories recursively outside the Ciao installation base: ~w", [Dir])
	).

:- use_module(ciaobld(eng_defs), [active_bld_eng_path/3]).

:- export(eng_active_bld/1).
% Create links for multi-platform engine selection (for build)
eng_active_bld(Eng) :-
	% E.g., ciaoengine.<OSARCH> -> <OSARCH>/ciaoengine
	A = ~eng_path(exec, Eng),
	B = ~active_bld_eng_path(exec, Eng),
	ignore_nosuccess(create_rel_link(A, B)),
	% Link for active exec_anyarch (E.g., ciaoengine -> ciaoengine.<OSARCH>)
	C = ~active_bld_eng_path(exec_anyarch, Eng),
	ignore_nosuccess(create_rel_link(B, C)).

% Like eng_active_bld/1, but for installed engines
eng_active_inst(Eng) :-
	% Link for active exec (E.g., ciaoengine.<OSARCH> -> ciaoengine-1.15/objs/<OSARCH>/ciaoengine) % TODO: 'activation' as a different operation?
	A = ~rootprefixed(~inst_eng_path(exec, Eng)),
	B = ~rootprefixed(~active_inst_eng_path(exec, Eng)),
	ignore_nosuccess(create_rel_link(A, B)),
	% Link for active exec_anyarch (E.g., ciaoengine -> ciaoengine.<OSARCH>)
	C = ~rootprefixed(~active_inst_eng_path(exec_anyarch, Eng)),
	ignore_nosuccess(create_rel_link(B, C)).

% ---------------------------------------------------------------------------
% (special for engines)
% TODO: generalize a-la OptimComp to executables with native code
%   (which can also be bytecode loaders, etc.)

:- use_module(ciaobld(eng_defs), [
	eng_path/3,
	inst_eng_path/3,
	active_inst_eng_path/3]).

% ---------------------------------------------------------------------------

:- use_module(library(system_extra), [
    del_file_nofail/1,
    set_file_perms/2,
    set_exec_perms/2]).
:- use_module(library(system), [copy_file/3]).

install_file(From, To) :-
	del_file_nofail(To),
	copy_file(From, To, [overwrite]),
	warn_on_nosuccess(set_file_perms(To, ~perms)).

install_bin_file(From, To) :-
	del_file_nofail(To),
	copy_file(From, To, [overwrite]),
	warn_on_nosuccess(set_exec_perms(To, ~perms)).

% ===========================================================================
:- doc(section, "Installation/uninstallation of documentation").

:- use_module(library(pathnames), [path_split/3]).
:- use_module(library(system), [file_exists/1]).
:- use_module(library(source_tree),
	[copy_file_or_dir/2, remove_file_or_dir/1]).

:- use_module(ciaobld(lpdoc_aux),
	[bld_manual_path/4,
	 act_manual_path/4,
	 inst_manual_path/4]).

% % TODO: (not needed)
% versioned_manual_base(Bundle, Base) := R :-
% 	( V = ~bundle_version(Bundle) ->
% 	    R = ~atom_concat([Base, '-', V])
% 	; R = Base
% 	).

:- export(install_doc/3).
% Install manual `ManualBase` (name and version) from given `Bundle`
% and format `DocFormat`. Do nothing if manual is not generated.
% NOTE: needed even if ~instype = local (see register_doc/2)
install_doc(Bundle, ManualBase, DocFormat) :-
	From = ~bld_manual_path(Bundle, ManualBase, DocFormat),
	( file_exists(From) ->
	    ( instype(global) ->
	        % Copy
	        Target = ~rootprefixed(~inst_manual_path(Bundle, ManualBase, DocFormat)),
		path_split(Target, TargetDir, _),
	        mkpath(TargetDir, ~perms),
		copy_file_or_dir(From, TargetDir), % Assumes that filename is the same!
		( link_docformat(DocFormat) ->
		    ActTarget = ~rootprefixed(~act_manual_path(Bundle, ManualBase, DocFormat)),
		    % Create activation symlink
		    path_split(ActTarget, ActTargetDir, _),
		    mkpath(ActTargetDir, ~perms),
		    ignore_nosuccess(create_rel_link(Target, ActTarget)),
		    % Register
		    register_doc(DocFormat, ActTarget)
		; true
		),
		% Register Target (it has a different Info 'dir' file)
		register_doc(DocFormat, Target)
	    ; % Register From (it has a different Info 'dir' file)
	      register_doc(DocFormat, From)
	    )
	; true % warning(['File ', From, ' not generated yet. Skipping copy'])
	).

link_docformat(manl).
link_docformat(info).

:- export(uninstall_doc/3).
% Uninstall manual `ManualBase` (name and version) from given `Bundle`
% and format `DocFormat`. Do nothing if manual is not generated.
% NOTE: needed even if ~instype = local (see unregister_doc/2)
uninstall_doc(Bundle, ManualBase, DocFormat) :-
	( instype(global) ->
	    ActTarget = ~rootprefixed(~act_manual_path(Bundle, ManualBase, DocFormat)),
	    ( link_docformat(DocFormat), file_exists(ActTarget) ->
	        % Unregister
	        unregister_doc(DocFormat, ActTarget)
	    ; true
	    ),
	    %
	    Target = ~rootprefixed(~inst_manual_path(Bundle, ManualBase, DocFormat)),
	    ( file_exists(Target) ->
	        % Unregister Target (it has a different Info 'dir' file)
	        unregister_doc(DocFormat, Target),
		% Remove if needed
	        remove_file_or_dir(Target)
	    ; true
	    )
	; % Unregister From (it has a different Info 'dir' file)
	  From = ~bld_manual_path(Bundle, ManualBase, DocFormat),
	  ( file_exists(From) ->
	      unregister_doc(DocFormat, From)
	  ; true
	  )
	).

:- use_module(ciaobld(info_installer)).

register_doc(info, Target) :- !,
	% Add to the Info 'dir' file
	path_split(Target, TargetDir, _), % assume 'dir' file is here
	dirfile_install_info(Target, TargetDir).
register_doc(_, _).

unregister_doc(info, Target) :- !,
	% Remove from the Info 'dir' file
	path_split(Target, TargetDir, _), % assume 'dir' file is here
	dirfile_uninstall_info(Target, TargetDir).
unregister_doc(_, _).

