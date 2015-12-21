:- module(_, [], [assertions, fsyntax, hiord]).

:- doc(title,  "Auxiliary Predicates for Builder").
:- doc(author, "Ciao Development Team").

:- use_module(library(pathnames),
	[path_split/3, path_dirname/2, path_basename/2, path_concat/3]).

:- use_module(library(bundle/bundle_flags)).
:- use_module(library(bundle/paths_extra), [fsR/2]).
:- use_module(ciaobld(messages_aux), [cmd_message/3]).
:- use_module(ciaobld(messages_aux), [verbose_message/2]).

% ===========================================================================

% TODO: move to internals.pl?

% Source directory for Ciao at boot time (based on ciao_lib_dir/1
% for the bootstrap system).
% TODO: Simplify; add an environment variable during bootstrap?
:- export(root_bundle_source_dir/1).
root_bundle_source_dir(Dir) :-
	ciao_lib_dir(CiaoLibDir),
	atom_concat(CiaoLibDir, '/..', Dir0),
	fixed_absolute_file_name(Dir0, Dir).

% ===========================================================================

:- use_module(library(bundle/bundlereg_gen), [lookup_bundle_root/2]).
:- use_module(library(bundle/bundle_info), [root_bundle/1]).
:- use_module(engine(internals), ['$bundle_id'/1]).

:- export(bundle_at_dir/2).
% Lookup the root or registered bundle at Dir or any of the parent directories
bundle_at_dir(Dir, Id) :-
	( lookup_bundle_root(Dir, BundleDir) ->
	    true
	; format(user_error, "ERROR: Not a bundle (or any of the parent directories).~n", []),
	  halt(1)
	),
	( dir_to_bundle(BundleDir, Target) ->
	    Id = Target
	; format(user_error, "ERROR: Bundle at ~w is not registered~n", [BundleDir]),
	  halt(1)
	),
	Id = Target.

dir_to_bundle(BundleDir, Id) :-
	root_bundle_source_dir(BundleDir),
	!,
	root_bundle(Id).
dir_to_bundle(BundleDir, Id) :-
	'$bundle_id'(Target),
	Dir = ~fsR(bundle_src(Target)),
	Dir == BundleDir,
	!,
	Id = Target.

% ===========================================================================

:- doc(section, "Invokation of external tools").
% TODO: make verbose messages optional

:- use_module(library(logged_process), [quoted_process_call/3]).

ciaocmd := ~bld_cmd_path(build, plexe, 'ciao'). % (supercommand)

gmake := ~get_bundle_flag(ciao:gmake_cmd).

:- export(invoke_gmake/2).
invoke_gmake(Dir, Args) :-
	verbose_message("Executing `make' on `~w' with arguments ~w", [Dir, Args]),
	Env = ['CIAOCMD' = ~ciaocmd],
	Options = [cwd(Dir), env(Env)],
	quoted_process_call(~gmake, Args, Options).

:- export(invoke_ant/2).
% Compilation of foreign Java code through Apache Ant (http://ant.apache.org/)
% TODO: Make Ant command configurable?
invoke_ant(Dir, Args) :-
	verbose_message("Executing `ant' on `~w' with arguments ~w", [Dir, Args]),
	Options = [cwd(Dir)],
	quoted_process_call(path(ant), Args, Options).

% ===========================================================================

:- doc(section, "Filesystem operations for builddir and storedir").

:- use_module(library(system), [copy_file/3, using_windows/0]).
:- use_module(library(system_extra),
	[del_file_nofail/1, '-'/1, '--'/1]).
:- use_module(library(source_tree), [remove_dir/1]).
:- use_module(library(system_extra), [mkpath/2]).
:- use_module(ciaobld(config_common), [perms/1]).

% ---------------------------------------------------------------------------

:- doc(subsection, "Build staging area (builddir)").

:- export(ensure_builddir/0).
% Prepare the build directory
ensure_builddir :-
 	mkpath(~fsR(builddir(build)), ~perms). % owner?

:- export(ensure_builddir_doc/0).
% Prepare the build subdirectory for docs
ensure_builddir_doc :-
 	mkpath(~fsR(builddir_doc(build)), ~perms). % owner?

:- export(ensure_builddir_bin/0).
% Prepare the build subdirectory for binaries
ensure_builddir_bin :-
 	mkpath(~fsR(builddir_bin(build)), ~perms). % owner?

:- export(builddir_bin_copy/3).
% Copy binary at binary directory of builddir
builddir_bin_copy(Kind, Dir, Base) :-
	File = ~bld_cmd_path(build, Kind, Base),
	copy_file(~fsR(Dir/Base), File, [overwrite]).

:- export(builddir_bin_copy_as/3).
% Copy a custom binary From at binary directory of builddir as Name
builddir_bin_copy_as(Kind, From, Name) :-
	File = ~bld_cmd_path(build, Kind, Name),
	copy_file(From, File, [overwrite]).

:- export(builddir_bin_link_as/3).
% 'Dest' will point to 'Src-Ver'
builddir_bin_link_as(Kind, Src, Dest) :-
	From = ~bld_cmd_path(build, Kind, Src),
	To = ~bld_cmd_path(build, Kind, Dest),
	create_link(From, To).

% ---------------------------------------------------------------------------

:- doc(subsection, "Installation area (bindir, storedir, etc.)").

:- use_module(ciaobld(config_common),
	[instciao_bindir/1,
	 instciao_storedir/1,
	 instciao_bundledir/2]).
:- use_module(library(bundle/bundle_params), [bundle_param_value/2]).
:- use_module(library(system), [delete_directory/1]).
:- use_module(library(source_tree), [copy_file_tree/4]).
:- use_module(library(glob), [glob/3]).
:- use_module(library(messages), [show_message/3]).

:- use_module(ciaobld(config_common), [
	bld_cmd_path/4,
	inst_cmd_path/4,
	active_cmd_path/3]).

:- export(rootprefix/1).
:- pred rootprefix(DestDir) # "@var{DestDir} is the prefix that is
   prepended to each (un)install target (useful for packaged bundle
   creation)".

% TODO: rename to install_destdir
rootprefix(R) :-
	( bundle_param_value(ciao:destdir, Value) ->
	    R = Value
	; R = ''
	).

:- export(rootprefixed/2).
% Add rootprefix (bundle param ciao:destdir) to the given path (for installation)
rootprefixed(Path0) := Path :-
	% (note: Path0 is an absolute path, do not use path_concat/3)
	Path = ~atom_concat(~rootprefix, Path0).

:- export(storedir_install/1).
% Creates a directory in the installation area
storedir_install(dir(Dir0)) :-
	Dir = ~rootprefixed(~fsR(Dir0)),
	( mkpath(Dir, ~perms) -> % TODO: owner?
	    true
	; show_message(error, "Could not create ~w", [Dir]),
	  fail
	).
% (copy all)
storedir_install(dir_rec(FromDir, ToDir)) :-
	copy_file_tree(installable_precomp(full),
	               ~fsR(FromDir), ~rootprefixed(~fsR(ToDir)), ~perms).
% (copy all except .po and .itf)
storedir_install(src_dir_rec(FromDir, ToDir)) :-
	copy_file_tree(installable_precomp(src),
	               ~fsR(FromDir), ~rootprefixed(~fsR(ToDir)), ~perms).
%
storedir_install(copy_and_link(Kind, Bundle, File)) :-
	storedir_install(copy(Kind, Bundle, File)),
	storedir_install(link_as(Kind, Bundle, File, File)).
%
storedir_install(copy(Kind, Bundle, File)) :-
	From = ~bld_cmd_path(build, Kind, File),
	To = ~rootprefixed(~inst_cmd_path(Bundle, Kind, File)),
	storedir_install(dir(~instciao_bindir)),
	install_file(From, To).
%
storedir_install(file_exec(From0, To0)) :-
	From = ~fsR(From0),
	To = ~rootprefixed(~fsR(To0)),
	install_file(From, To).
%
storedir_install(file_noexec(From0, To0)) :- % TODO: add Kind...
	From = ~fsR(From0),
	To = ~rootprefixed(~fsR(To0)),
	install_file(From, To).
%
storedir_install(link_as(Kind, Bundle, Src, Dest)) :-
	% TODO: move to 'activation' operation?
	From = ~inst_cmd_path(Bundle, Kind, Src),
	path_basename(From, FromBase),
	To = ~rootprefixed(~active_cmd_path(Kind, Dest)),
	create_link(FromBase, To).
%
storedir_install(file_link_as(From0, To0)) :-
	From = ~fsR(From0),
	To = ~rootprefixed(~fsR(To0)),
	create_link(From, To).
% at_bundle:
%   install a copy of File in <install_bundledir>/File and
%   install a link from <install_bundledir>/File to <install_storedir>
% to_abspath:
%   install a copy of File to abspath and
%   install a link from abspath to <install_storedir>
storedir_install(lib_file_copy_and_link(Props, Bundle, Path, File)) :-
	% ( ~instype = global -> true ; throw(install_requires_global) ),
	From = ~fsR(Path/File),
	( member(at_bundle, Props) ->
	    To = ~rootprefixed(~fsR(~instciao_bundledir(Bundle)/(File))),
	    % TODO: weak, compute relative path instead
	    LinkFrom = ~path_concat(~path_basename(~path_dirname(To)), File)
	; member(to_abspath(To0), Props) -> % TODO: compute relative path instead?
	    To = ~rootprefixed(~fsR(To0)),
	    LinkFrom = ~fsR(To0)
	; throw(wrong_props)
	),
	%
	install_file(From, To),
	%
	( member(storedir_link, Props) ->
	    PlainTo = ~fsR(~instciao_storedir/(File)),
	    create_link(LinkFrom, ~rootprefixed(~fsR(PlainTo)))
	; true
	).
% TODO: show the same kind of messages that are used when compiling libraries
storedir_install(cmds_list(Bundle, Ps)) :-
	( % (failure-driven loop)
	  member(P0, Ps),
	    storedir_install(cmd(Bundle, P0)),
	    fail
	; true
	).
%
storedir_install(cmd(Bundle, P0)) :-
	n_and_props(P0, P, Props),
	n_name(Props, Name),
	n_kind(Props, K),
	n_output(P, Props, Output),
	cmd_message(Bundle, "installing '~w' (~s)", [Output, Name]),
	storedir_install(copy_and_link(K, Bundle, Output)).
% Install contents of EngMainMod engine build for installation:
%  - engine+arch
%  - a link engine+arch->engine to storedir
%  - C headers
%
% TODO: create 'ciao-config' exec to get options for CC/LD; add an 'activation' operation
%
storedir_install(eng_contents(Bundle, EngMainMod)) :- !,
	storedir_install(dir(~inst_eng_path(engdir, Bundle, EngMainMod))), % TODO: set_file_perms or set_exec_perms?
	%
	LocalEng = ~bld_eng_path(exec, build, EngMainMod),
	InstEng = ~inst_eng_path(exec, Bundle, EngMainMod),
	% Install exec
	storedir_install(dir(~inst_eng_path(objdir_anyarch, Bundle, EngMainMod))), % TODO: set_file_perms or set_exec_perms?
	storedir_install(dir(~inst_eng_path(objdir, Bundle, EngMainMod))), % TODO: set_file_perms or set_exec_perms?
	storedir_install(file_exec(LocalEng, InstEng)),
	% Install headers
        HDir = ~bld_eng_path(hdir, build, EngMainMod),
	InstEngHDir = ~inst_eng_path(hdir, Bundle, EngMainMod),
	storedir_install(src_dir_rec(HDir, InstEngHDir)).
storedir_install(eng_active(Bundle, EngMainMod)) :- !, % Activate engine (for multi-platform)
	% (see eng_active_bld/1)
	% Link for active exec (E.g., ciaoengine.<OSARCH> -> ciaoengine-1.15/objs/<OSARCH>/ciaoengine) % TODO: 'activation' as a different operation?
	InstEng = ~inst_eng_path(exec, Bundle, EngMainMod),
	ActiveEng = ~active_inst_eng_path(exec, EngMainMod),
	storedir_install(file_link_as(InstEng, ActiveEng)),
	% Link for active exec_anyarch (E.g., ciaoengine -> ciaoengine.<OSARCH>)
	ActiveEngAnyArch = ~active_inst_eng_path(exec_anyarch, EngMainMod),
	storedir_install(file_link_as(~path_basename(ActiveEng), ActiveEngAnyArch)).

:- export(storedir_uninstall/1).
storedir_uninstall(link(Kind, File)) :-
	storedir_uninstall(file(~active_cmd_path(Kind, File))).
%
storedir_uninstall(copy(Kind, Bundle, File)) :-
	storedir_uninstall(file(~inst_cmd_path(Bundle, Kind, File))).
%
storedir_uninstall(copy_and_link(Kind, Bundle, File)) :-
	storedir_uninstall(link(Kind, File)),
	storedir_uninstall(copy(Kind, Bundle, File)).
%
storedir_uninstall(file(File)) :-
	del_file_nofail(~rootprefixed(~fsR(File))).
%
storedir_uninstall(dir_rec(Dir)) :-
	% TODO: Add some sanity check here to avoid filesystem havoc
	try_remove_dir(Dir).
%
storedir_uninstall(src_dir_rec(Dir)) :-
	% TODO: Add some sanity check here to avoid filesystem havoc
	try_remove_dir(Dir).
%
storedir_uninstall(dir(Dir)) :-
	-delete_directory(~rootprefixed(~fsR(Dir))).
%
storedir_uninstall(dir_if_empty(Dir)) :-
	--delete_directory(~rootprefixed(~fsR(Dir))).
%
storedir_uninstall(lib_file_copy_and_link(Props, Bundle, File)) :-
	% ( ~instype = global -> true ; throw(uninstall_requires_global) ),
	( member(storedir_link, Props) ->
	    PlainTo = ~fsR(~instciao_storedir/(File)),
	    storedir_uninstall(file(PlainTo))
	; true
	),
	( member(at_bundle, Props) ->
	    To = ~fsR(~instciao_bundledir(Bundle)/(File))
	; member(to_abspath(To0), Props) ->
	    To = ~fsR(To0)
	; throw(wrong_props)
	),
	storedir_uninstall(file(To)).
% TODO: show the same kind of messages that are used when compiling libraries
storedir_uninstall(cmds_list(Bundle, Ps)) :-
	( % (failure-driven loop)
	  member(P0, Ps),
	    storedir_uninstall(cmd(Bundle, P0)),
	    fail
	; true
	).
%
storedir_uninstall(cmd(Bundle, P0)) :-
	n_and_props(P0, P, Props),
	n_name(Props, Name),
	n_kind(Props, K),
	n_output(P, Props, Output),
	cmd_message(Bundle, "uninstalling '~w' (~s)", [Output, Name]),
	storedir_uninstall(copy_and_link(K, Bundle, Output)).
% Uninstall EngMainMod engine
storedir_uninstall(eng_contents(Bundle, EngMainMod)) :- !,
	storedir_uninstall(file(~inst_eng_path(exec, Bundle, EngMainMod))),
        % Uninstall C headers
	InstEngHDir = ~inst_eng_path(hdir, Bundle, EngMainMod),
	storedir_uninstall(src_dir_rec(InstEngHDir)),
	%
	storedir_uninstall(dir_if_empty(~inst_eng_path(objdir, Bundle, EngMainMod))),
	storedir_uninstall(dir_if_empty(~inst_eng_path(objdir_anyarch, Bundle, EngMainMod))),
	storedir_uninstall(dir_if_empty(~inst_eng_path(engdir, Bundle, EngMainMod))).
storedir_uninstall(eng_active(_Bundle, EngMainMod)) :- !,
	% TODO: only if it coincides with the active version (better, do unactivation before)
	storedir_uninstall(file(~active_inst_eng_path(exec, EngMainMod))),
	storedir_uninstall(file(~active_inst_eng_path(exec_anyarch, EngMainMod))).

:- use_module(ciaobld(config_common), [active_bld_eng_path/3]).

:- export(eng_active_bld/1).
% Create links for multi-platform engine selection (for build)
eng_active_bld(EngMainMod) :-
	BldEng = ~bld_eng_path(exec, build, EngMainMod),
	ActiveEngAnyArch = ~active_bld_eng_path(exec_anyarch, EngMainMod),
	ActiveEng = ~active_bld_eng_path(exec, EngMainMod),
	% E.g., ciaoengine.<OSARCH> -> <OSARCH>/ciaoengine
	create_link(~path_rel2(BldEng), ActiveEng),
	% Link for active exec_anyarch (E.g., ciaoengine -> ciaoengine.<OSARCH>)
	create_link(~path_basename(ActiveEng), ActiveEngAnyArch).

% TODO: weak, compute relative path instead
% ../a/b/c -> b/c
path_rel2(Path, Rel) :-
	path_split(Path, Dir, Base2),
	path_basename(Dir, Base1),
	path_concat(Base1, Base2, Rel).

% (shared)
%:- export(create_link/2).
create_link(From, To) :-
	del_file_nofail(To),
	% TODO: better solution? windows lacks proper symlinks
        ( using_windows ->
            --copy_file(From, To, [overwrite])
        ; --copy_file(From, To, [overwrite, symlink])
        ).
        % TODO: do not set perms on a symbolic link (the source may
        %       not exist, at it happens in RPM generation)
%	-set_file_perms(To, ~perms).

install_file(From, To) :-
	del_file_nofail(To),
	copy_file(From, To, [overwrite]),
	-set_exec_perms(To, ~perms).

try_remove_dir(Dir) :-
	Dir2 = ~rootprefixed(~fsR(Dir)),
	( file_exists(Dir2) ->
	    remove_dir(Dir2)
	; true
	).

% Descriptions for commands (e.g., standalone utilities, etc.)
% TODO: improve

:- export(n_and_props/3).
n_and_props(P0, P, Props) :-
	( P0 = P-Props -> true ; P = P0, Props = [] ).

:- export(n_name/2).
n_name(Props, Name) :-
	( member(name=Name, Props) ->
	    true
	; Name = "command" % default name
	).

:- export(n_output/3).
n_output(P, Props, Name) :-
	( member(output=Name0, Props) ->
	    Name = Name0
	; Name = P
	).

n_kind(Props, Kind) :-
	( member(kind=Kind, Props) -> true ; Kind=plexe ).

% ---------------------------------------------------------------------------
% (special for engines)
% TODO: generalize a-la OptimComp to executables with native code
%   (which can also be bytecode loaders, etc.)

% TODO: Make sure that CIAOHDIR points to the right place when the engine 
%   is installed in instype=global

:- use_module(ciaobld(config_common), [
	bld_eng_path/4,
	inst_eng_path/4,
	active_inst_eng_path/3]).

% ===========================================================================

:- doc(section, "Instantiating Template Files with Parameters").

:- use_module(library(text_template), [eval_template_file/3]).
:- use_module(library(system_extra), ['-'/1]).
:- use_module(library(system_extra), [set_file_perms/2, set_exec_perms/2]).

:- export(wr_template/4).
% Generate files based on text templates
% TODO: improve
wr_template(at(OutDir), Dir, File, Subst) :-
	In = ~atom_concat(~fsR(Dir/File), '.skel'),
	Out = ~fsR(OutDir/File),
	eval_template_file(In, Subst, Out).
wr_template(origin, Dir, File, Subst) :-
	In = ~atom_concat(~fsR(Dir/File), '.skel'),
	Out = ~fsR(Dir/File),
	eval_template_file(In, Subst, Out).
wr_template(k(Kind), Dir, File, Subst) :-
	In = ~atom_concat(~fsR(Dir/File), '.skel'),
	Out = ~bld_cmd_path(build, Kind, File),
	eval_template_file(In, Subst, Out),
	( kind_exec_perms(Kind) ->
	    -set_exec_perms(Out, ~perms)
	; true
	).

kind_exec_perms(shscript).

% ===========================================================================

:- use_module(engine(internals), ['$bundle_prop'/2]).	
:- use_module(library(bundle/bundle_info), [bundle_version/2, bundle_patch/2]).

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(format), [format/3]).
:- use_module(library(write), [portray_clause/2]).
:- use_module(library(system), [file_exists/1]).
:- use_module(library(system_extra), [datime_string/1]).

:- export(generate_version_auto/2).
% Create a file (File) with a version/1 fact indicating the current
% version of Bundle, build time, and compiler version.

% TODO: generalize for all bundles; change modiftime only if there are changes
%       move, include other options (for runtime)?

generate_version_auto(_Bundle, File) :-
	file_exists(File), % TODO: update file if contents change
	!.
generate_version_auto(Bundle, File) :-
	Version = ~bundle_version(Bundle),
	Patch = ~bundle_patch(Bundle),
	atom_codes(Date, ~datime_string),
	%
	CVersion = ~bundle_version(core),
	CPatch = ~bundle_patch(core),
	%
	VersionAtm = ~atom_concat([
	  Version, '.', Patch, ': ', Date, ' (compiled with Ciao ', CVersion, '.', CPatch, ')'
        ]),
	open(File, write, O),
	format(O, "%% Do not edit - automatically generated!\n", []),
        portray_clause(O, version(VersionAtm)),
	close(O).


