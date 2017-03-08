:- module(_, [], [assertions, fsyntax, hiord]).

% TODO: redistribute predicates from this module?
:- doc(title,  "Auxiliary Predicates for Builder").
:- doc(author, "Ciao Development Team").

:- use_module(library(pathnames), [path_dirname/2, path_concat/3]).
:- use_module(library(bundle/bundle_flags)).
:- use_module(library(bundle/bundle_paths), [bundle_path/3, bundle_path/4]).
:- use_module(ciaobld(messages_aux), [normal_message/2]).
:- use_module(ciaobld(messages_aux), [verbose_message/2]).
:- use_module(ciaobld(config_common), [cmd_path/4]).
:- use_module(library(messages), [warning_message/2]).

% ===========================================================================

:- use_module(ciaobld(manifest_compiler), [lookup_bundle_root/2]).
:- use_module(ciaobld(bundle_scan), [root_bundle/1]).
:- use_module(engine(internals), ['$bundle_id'/1]).
:- use_module(engine(internals), [ciao_root/1]).

:- export(bundle_at_dir/2).
% Lookup the root or registered bundle at Dir or any of the parent directories
bundle_at_dir(Dir, Bundle) :-
	( lookup_bundle_root(Dir, BundleDir) ->
	    true
	; throw(error_msg("Not a bundle (or any of the parent directories).", []))
	),
	( dir_to_bundle(BundleDir, Bundle0) ->
	    Bundle = Bundle0
	; throw(error_msg("Bundle at ~w is not registered.", [BundleDir]))
	).

dir_to_bundle(BundleDir, Bundle) :-
	ciao_root(CiaoRoot),
	BundleDir = CiaoRoot,
	!,
	root_bundle(Bundle).
dir_to_bundle(BundleDir, Bundle) :-
	'$bundle_id'(Bundle0),
	Dir = ~bundle_path(Bundle0, '.'),
	Dir == BundleDir,
	!,
	Bundle = Bundle0.

% ---------------------------------------------------------------------------

:- use_module(engine(internals), [ciao_path/1]).
:- use_module(library(pathnames), [path_get_relative/3]).

:- export(ciao_path_at_dir/2).
ciao_path_at_dir(Dir, Path) :-
	( lookup_ciao_path(Dir, Path0) ->
	    Path = Path0
	; throw(error_msg("Directory (or any of the parent directories) not in CIAOPATH.", []))
	).

% Detect the workspace for the given File (a directory or normal
% file), using ciao_path/1 or the CIAOROOT directory.
lookup_ciao_path(File, Path) :-
	fixed_absolute_file_name(File, '.', Path0),
	( lookup_ciao_path_(Path0, Path1) ->
	    Path1 = Path
	; fail
	).

lookup_ciao_path_(Path0, Path) :-
	( ciao_path(Path)
	; ciao_root(Path)
	),
	( Path0 = Path -> true
	; path_get_relative(Path, Path0, _) % Path0 is relative to Path
	).

% ===========================================================================

:- doc(section, "Invokation of external tools").
% TODO: make verbose messages optional

:- use_module(library(logged_process), [quoted_process_call/3]).

%ciaocmd := ~cmd_path(core, plexe, 'ciao'). % (supercommand)
% TODO: unfortunately 'ciao' supercommand is still a shell script; fix it so that it runs in Win32 without MSYS2
ciaocmd := ~cmd_path(core, shscript, 'ciao'). % (supercommand)

gmake := ~get_bundle_flag(builder:gmake_cmd).

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
:- doc(section, "Build area (builddir)").

:- use_module(library(source_tree), [remove_dir/1]).
:- use_module(library(system_extra), [mkpath/1]).

:- export(ensure_builddir/2).
% Prepare a build (sub)directory (Rel can be '.' or a relative path)
ensure_builddir(Bundle, Rel) :-
 	mkpath(~bundle_path(Bundle, builddir, Rel)).

% Special clean targets for builddir
% TODO: Clean per bundle? (e.g., for bin/ it is complex, similar to uninstall)
:- export(builddir_clean/2).
builddir_clean(Bundle, bundlereg) :- !,
	remove_dir_nofail(~bundle_path(Bundle, builddir, 'bundlereg')).
builddir_clean(Bundle, config) :- !,
	% TODO: this only works for (~root_bundle)
	del_file_nofail(~bundle_path(Bundle, builddir, 'bundlereg/ciao.bundlecfg')),
	del_file_nofail(~bundle_path(Bundle, builddir, 'bundlereg/ciao.bundlecfg_sh')).
builddir_clean(Bundle, bin) :- !,
	remove_dir_nofail(~bundle_path(Bundle, builddir, 'bin')).
builddir_clean(Bundle, pbundle) :- !,
	remove_dir_nofail(~bundle_path(Bundle, builddir, 'pbundle')).
builddir_clean(Bundle, doc) :- !,
	remove_dir_nofail(~bundle_path(Bundle, builddir, 'doc')).
builddir_clean(Bundle, all) :-
	remove_dir_nofail(~bundle_path(Bundle, builddir, '.')).

% ===========================================================================

:- doc(section, "Build Template Files with Parameters").

:- use_module(library(text_template), [eval_template_file/3]).
:- use_module(library(system_extra), [warn_on_nosuccess/1]).
:- use_module(library(system_extra), [set_file_perms/2, set_exec_perms/2]).

:- export(wr_template/4).
% Generate files based on text templates
% TODO: improve
wr_template(at(OutDir), Dir, File, Subst) :-
	In = ~path_concat(Dir, ~atom_concat(File, '.skel')),
	Out = ~path_concat(OutDir, File),
	eval_template_file(In, Subst, Out).
wr_template(origin, Dir, File, Subst) :-
	In = ~path_concat(Dir, ~atom_concat(File, '.skel')),
	Out = ~path_concat(Dir, File),
	eval_template_file(In, Subst, Out).
wr_template(as_cmd(Bundle, Kind), Dir, File, Subst) :-
	In = ~path_concat(Dir, ~atom_concat(File, '.skel')),
	Out = ~cmd_path(Bundle, Kind, File),
	eval_template_file(In, Subst, Out),
	( kind_exec_perms(Kind) ->
	    warn_on_nosuccess(set_exec_perms(Out, perms(rwX, rwX, rX)))
	; true
	).

kind_exec_perms(shscript).

% ===========================================================================

:- use_module(library(bundle/bundle_info), [bundle_version/2]).

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(format), [format/3]).
:- use_module(library(system), [file_exists/1]).

:- export(generate_version_auto/2).
% Create a file (File) with a version/1 fact indicating the current
% version of Bundle, build time, and compiler version.

generate_version_auto(Bundle, File) :-
	Version = ~bundle_version(Bundle),
	CVersion = ~bundle_version(core),
	%
	VersionAtm = ~atom_concat([
	  Version, ' (compiled with Ciao ', CVersion, ')'
        ]),
	update_file_from_clauses([version(VersionAtm)], File, _).

% ---------------------------------------------------------------------------
% TODO: move somewhere else?

:- use_module(library(write), [portray_clause/2]).
:- use_module(library(hiordlib), [maplist/2]).

:- export(print_clauses_to_file/2).
% Portray clauses to a file
print_clauses_to_file(Clauses, Path) :-
        open(Path, write, S),
	display(S, '% Do not edit -- generated automatically\n\n'), % TODO: optional?
	maplist(print_clause(S), Clauses),
	close(S).

print_clause(Clause, S) :-
 	portray_clause(S, Clause).

% ---------------------------------------------------------------------------
% TODO: move somewhere else?

:- use_module(library(system_extra), [move_if_diff/3]).
:- use_module(library(system), [mktemp_in_tmp/2]).
:- use_module(library(file_utils), [string_to_file/2]).

:- export(update_file_from_clauses/3).
% Like @pred{print_clauses_to_file/2} but preserves timestamp if file
% contents have not changed.
update_file_from_clauses(Clauses, Path, NewOrOld) :-
	mktemp_in_tmp('clauses-XXXXXX', File),
	print_clauses_to_file(Clauses, File),
	move_if_diff(File, Path, NewOrOld).

:- export(update_file_from_string/3).
% Like @pred{string_to_file/2} but preserves timestamp if file
% contents have not changed.
update_file_from_string(String, Path, NewOrOld) :-
	mktemp_in_tmp('clauses-XXXXXX', File),
	string_to_file(String, File),
	move_if_diff(File, Path, NewOrOld).

% ===========================================================================
% TODO: move to eng_maker.pl?

:- use_module(ciaobld(third_party_install), [third_party_path/2]).
:- use_module(library(pathnames), [path_relocate/4]).
:- use_module(library(lists), [append/3]).

:- export(add_rpath/3).
% Add rpaths (runtime search path for shared libraries)
add_rpath(local_third_party, LinkerOpts0, LinkerOpts) :- !,
	% TODO: better way to compute RelativeLibDir?
	% (for 'ciaoc_car.pl')
	ciao_root(CiaoRoot), % TODO: get workspace from bundle!
	third_party_path(libdir, LibDir),
	path_relocate(CiaoRoot, '.', LibDir, RelativeLibDir),
	add_rpath_(RelativeLibDir, LinkerOpts0, LinkerOpts).
add_rpath(executable_path, LinkerOpts0, LinkerOpts) :- !,
	% (for 'ciaoc_sdyn')
	% (note: not quoted here since we pass args with process_call/3)
	add_rpath_('$ORIGIN', LinkerOpts0, LinkerOpts).

add_rpath_(Path, LinkerOpts0, LinkerOpts) :-
	Opt = ~atom_concat('-Wl,-rpath,', Path),
	LinkerOpts = [Opt|LinkerOpts0].

% ===========================================================================
% Alternative hooks for installation of third-party code
% TODO: merge with the Prolog version

:- use_module(library(process), [process_call/3]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).
:- use_module(ciaobld(third_party_install), [third_party_path/2]).

% (will not work in Windows)
third_party_aux_sh := ~bundle_path(builder, 'src/third_party_aux.bash').

third_party_defs_sh(Bundle, ForeignName) := Path :-
	Dir = ~bundle_path(Bundle, 'Manifest'),
	DefsSh = ~atom_concat(ForeignName, '.defs.sh'),
	Path = ~path_concat(Dir, DefsSh).

:- export(third_party_aux/3).
third_party_aux(Bundle, ForeignName, Args) :- 
	DefsSh = ~third_party_defs_sh(Bundle, ForeignName),
	OS = ~get_bundle_flag(core:os),
	Arch = ~get_bundle_flag(core:arch),
	third_party_path(prefix, ThirdParty), % TODO: add bundle to third_party_path/2
	Env = ['CIAO_OS'=OS, 'CIAO_ARCH'=Arch, 'THIRDPARTY'=ThirdParty],
	process_call(~third_party_aux_sh, [DefsSh|Args], [env(Env)]).

% ===========================================================================

:- use_module(ciaobld(eng_defs), [eng_path/3]).
:- use_module(library(llists), [flatten/2]).

:- export(update_stat_config_sh/2).
update_stat_config_sh(Eng, LinkerOpts) :-
	% TODO: missing quote
	LinkerOptsStr = ~atom_codes(~atom_concat_with_blanks(LinkerOpts)),
	Str = ~flatten(["ADD_STAT_LIBS=\'"||LinkerOptsStr, "\'\n"]),
	%
	CfgDir = ~eng_path(cfgdir, Eng), 
	mkpath(CfgDir),
	update_file_from_string(Str, ~path_concat(CfgDir, 'config_sh'), _).

:- use_module(library(terms), [atom_concat/2]).

atom_concat_with_blanks(L) := ~atom_concat(~separate_with_blanks(L)).

separate_with_blanks([]) := [] :- !.
separate_with_blanks([A]) := [A] :- !.
separate_with_blanks([A, B|Cs]) := [A, ' '|~separate_with_blanks([B|Cs])] :- !.

% ===========================================================================
% TODO: Move both create_rel_link/2 and relpath/3 to the libraries

:- use_module(library(system), [copy_file/3]).
:- use_module(library(system), [using_windows/0]).
:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(library(system_extra), [ignore_nosuccess/1]).

:- export(remove_dir_nofail/1).
remove_dir_nofail(Dir2) :-
	( file_exists(Dir2) -> remove_dir(Dir2) ; true ).

:- export(create_rel_link/2).
% Create a "relocatable" link (computing relative paths)
% (e.g., "/a/b/c (symlink) -> /a/d/e" becomes "/a/b/c (symlink) -> ../d/e"
create_rel_link(From, To) :-
	path_dirname(To, ToDir),
	relpath(ToDir, From, RelFrom),
	create_link(RelFrom, To).

%:- export(create_link/2).
create_link(From, To) :-
	del_file_nofail(To),
	% TODO: better solution? windows lacks proper symlinks
        ( using_windows ->
            ignore_nosuccess(copy_file(From, To, [overwrite]))
        ; ignore_nosuccess(copy_file(From, To, [overwrite, symlink]))
        ).
        % TODO: do not set perms on a symbolic link (the source may
        %       not exist, as it happens in RPM generation)
%	warn_on_nosuccess(set_file_perms(To, ~perms)).

:- use_module(library(pathnames), [path_split_list/2, path_concat_list/2]).

%:- export(relpath/3).
% C is a path to B relative to A (using '..' if needed)
% (e.g., "/a/b/c -> /a/d/e" becomes "/a/b/c -> ../../d/e".
% Assume both are absolute, otherwise just return B.
relpath(A, B, C) :-
	path_split_list(A, As),
	path_split_list(B, Bs),
	As = ['/'|As0],
	Bs = ['/'|Bs0],
	!,
	relpath_(As0, Bs0, Cs),
	path_concat_list(Cs, C).
relpath(_, B, B).

% Consume common part
relpath_([A|As], [A|Bs], Cs) :- !, relpath_(As, Bs, Cs).
relpath_(As, Bs, Cs) :- relpath__(As, Bs, Cs).

% Add '..' for each A component (go back)
relpath__([_|As], Bs, ['..'|Cs]) :- !, relpath__(As, Bs, Cs).
relpath__([], Bs, Bs). % Finish with rest
