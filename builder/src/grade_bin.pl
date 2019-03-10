:- module(_, [], [fsyntax, hiord, assertions, regtypes, isomodes]).

:- doc(title, "Binary grade").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module contains command definitions for the
   @tt{bin} grade, which builds/clean/install/uninstall binaries from
   sources.").

:- use_module(ciaobld(builder_cmds), [builder_cmd/2, target_is_workspace/1]).
:- use_module(ciaobld(manifest_compiler), [target_is_bundle/1, main_file_path/3]).

% ---------------------------------------------------------------------------

:- include(ciaobld(cmd_hooks)).

% ---------------------------------------------------------------------------

% Any target requires 'core.ciaobase' (this ensures that it is
% compiled first)
'grade.requires'(bin, bin, 'core.ciaobase').

% ---------------------------------------------------------------------------
% build/clean (bin)

'grade.cmd'(bin, build, build_bin).
'grade.cmd'(bin, clean, clean_bin).

'cmd.comment'(build_bin, ["building [bin]", "built [bin]"]).
'cmd.grade'(build_bin, bin).
'cmd.needs_update_builder'(build_bin).
'cmd.needs_rescan'(build_bin).
'cmd.needs_config'(build_bin).
'cmd.recursive'(build_bin, forward).
'cmd.do_before.decl'(build_bin).
'cmd.do_before'(build_bin, Target) :- !,
	builder_cmd(prepare_build_bin, Target).

'cmd.comment'(prepare_build_bin, ["preparing build [bin]", "prepared build [bin]"]).
'cmd.grade'(prepare_build_bin, custom_bin).

:- use_module(ciaobld(builder_aux), [builddir_clean/2]).
:- use_module(ciaobld(ciaoc_aux), [clean_tree/1]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(library(pathnames), [path_concat/3]).

% Like 'clean', but keeps documentation targets.
% (This reverses the 'build_bin' and part of 'build_docs' actions)
'cmd.comment'(clean_bin, ["cleaning [bin]", "cleaned [bin]"]).
'cmd.grade'(clean_bin, bin).
'cmd.needs_config'(clean_bin).
'cmd.recursive'(clean_bin, backward).
'cmd.do_after.decl'(clean_bin).
'cmd.do_after'(clean_bin, Target) :- !,
	( target_is_workspace(Target) -> true
	; target_is_bundle(Target) ->
	    do_clean_bundle(Target) 
	; true 
	).

% TODO: make it fine grained, implement clean_bin on primtgts
do_clean_bundle(Bundle) :-
	% TODO: clean only on lib, etc. areas (not externals/ etc.)
	% clean_tree(~bundle_path(Bundle, 'Manifest')) % TODO: only if it is a directory!
	clean_tree(~bundle_path(Bundle, '.')).

% ---------------------------------------------------------------------------
% install/uninstall (bin)

:- use_module(ciaobld(install_aux), [
  install_bin_dirs/1,
  uninstall_bin_dirs/1,
  install_bundlereg/1,
  uninstall_bundlereg/1
]).

'grade.cmd'(bin, install, install_bin).
'grade.cmd'(bin, uninstall, uninstall_bin).

'cmd.comment'(install_bin, ["installing [bin]", "installed [bin]"]).
'cmd.grade'(install_bin, bin).
'cmd.only_global_instype'(install_bin).
%'cmd.needs_update_builder'(install_bin).
'cmd.needs_rescan'(install_bin).
'cmd.recursive'(install_bin, forward).
'cmd.do_before.decl'(install_bin).
'cmd.do_before'(install_bin, Target) :- !,
	( target_is_workspace(Target) -> true
	; target_is_bundle(Target) -> install_bin_dirs(Target)
	; true
	).
'cmd.do_after.decl'(install_bin).
'cmd.do_after'(install_bin, Target) :- !,
	( target_is_workspace(Target) -> true
	; target_is_bundle(Target) -> install_bundlereg(Target) % Activate
	; true
	).

'cmd.comment'(uninstall_bin, ["uninstalling [bin]", "uninstalled [bin]"]).
'cmd.grade'(uninstall_bin, bin).
'cmd.only_global_instype'(uninstall_bin).
'cmd.recursive'(uninstall_bin, backward).
'cmd.do_before.decl'(uninstall_bin).
'cmd.do_before'(uninstall_bin, Target) :- !,
	( target_is_workspace(Target) -> true
	; target_is_bundle(Target) -> uninstall_bundlereg(Target) % Deactivate
	; true
	).
'cmd.do_after.decl'(uninstall_bin).
'cmd.do_after'(uninstall_bin, Target) :- !,
	( target_is_workspace(Target) -> true
	; target_is_bundle(Target) -> uninstall_bin_dirs(Target) % TODO: uninstall 'initial' bundle?
	; true
	).

% ---------------------------------------------------------------------------
% Primitive targets for bin grade

:- use_module(library(lists), [member/2]).
:- use_module(library(pathnames), [path_concat/3, path_split/3]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(ciaobld(messages_aux), [normal_message/2]).
% (build)
:- use_module(ciaobld(ciaoc_aux), [
    build_eng_exec_header/1,
    clean_eng_exec_header/1,
    %
    build_libs/2,
    cmd_build/1
]).
:- use_module(ciaobld(eng_maker), [
    eng_build/1,
    eng_clean/1
]).
% (installation)
:- use_module(ciaobld(install_aux), [
    eng_active_bld/1,
    instdir_install/1,
    instdir_uninstall/1,
    inst_bundle_path/3,
    final_ciao_root/1
]).

'grade.prim_kind'(bin, bin) :- !.
'grade.prim_do'(bin, Prim, Bundle, Cmd) :- !,
	prim(Prim, Bundle, Cmd).

% NOTE: install_bin and uninstall_bin require ~instype=global!

% Commands with no default action in bin grade
prim(_, _, prebuild_bin) :- !. % (default is nop)
prim(_, _, register) :- !. % (default is nop)
prim(_, _, unregister) :- !. % (default is nop)
% lib/1
prim(lib(Path), Bundle, build_bin) :- !,
	build_libs(Bundle, ~bundle_path(Bundle, Path)).
prim(lib(_), _Bundle, clean_bin) :- !.
prim(lib(Path), Bundle, install_bin) :- !,
	% Install the module collection under Path (along compiled files)
	normal_message("installing ~w/", [Path]),
	From = ~bundle_path(Bundle, Path),
	To = ~inst_bundle_path(Bundle, Path),
	instdir_install(dir_rec(From, To)). % TODO: make it work with CIAOCACHE
prim(lib(Path), Bundle, uninstall_bin) :- !,
	% Uninstall the previously installed module collection Path
	To = ~inst_bundle_path(Bundle, Path),
	instdir_uninstall(dir_rec(To)). % TODO: make it work with CIAOCACHE
% lib_force_build/1
prim(lib_force_build(Path), Bundle, build_bin) :- !, % TODO: hack for library/clpq, library/clpr (see core bundle)
	build_libs(Bundle, ~bundle_path(Bundle, Path)).
prim(lib_force_build(_), _Bundle, clean_bin) :- !.
prim(lib_force_build(_), _Bundle, install_bin) :- !. % TODO: assume installed with lib()
prim(lib_force_build(_), _Bundle, uninstall_bin) :- !. % TODO: assume installed with lib()
% src/1
prim(src(_Path), _Bundle, build_bin) :- !. % (only for install_bin)
prim(src(_Path), _Bundle, clean_bin) :- !.
prim(src(Path), Bundle, install_bin) :- !,
	% Install the module collection under Path (just source, e.g., for examples)
	normal_message("installing ~w (source)", [Path]),
	instdir_install(src_dir_rec(~bundle_path(Bundle, Path), ~inst_bundle_path(Bundle, Path))).
prim(src(Path), Bundle, uninstall_bin) :- !,
	% Uninstall the previously installed source-only module collection Path
	instdir_uninstall(src_dir_rec(~inst_bundle_path(Bundle, Path))).
% assets/1
prim(assets(_Path), _Bundle, build_bin) :- !.
prim(assets(_Path), _Bundle, clean_bin) :- !.
prim(assets(Path), Bundle, install_bin) :- !,
	% Copy all files from Path
	From = ~bundle_path(Bundle, Path),
	To = ~inst_bundle_path(Bundle, Path),
	instdir_install(dir(To)),
	instdir_install(dir_rec(From, To)).
prim(assets(Path), Bundle, uninstall_bin) :- !,
	% on uninstall, remove contents recursively
	% TODO: remove also the directory?
	To = ~inst_bundle_path(Bundle, Path),
	instdir_uninstall(dir_rec(To)).
% assets/2
prim(assets(_Path, _List), _Bundle, build_bin) :- !.
prim(assets(_Path, _List), _Bundle, clean_bin) :- !.
prim(assets(Path, List), Bundle, install_bin) :- !,
	instdir_install(dir(~inst_bundle_path(Bundle, Path))),
	assets_files_do(List, Bundle, Path, install_bin).
prim(assets(Path, List), Bundle, uninstall_bin) :- !,
	assets_files_do(List, Bundle, Path, uninstall_bin),
	instdir_uninstall(dir(~inst_bundle_path(Bundle, Path))).
% cmd_raw/3
prim(cmd_raw(_K, _File, _Props), _Bundle, build_bin) :- !.
prim(cmd_raw(_K, _File, _Props), _Bundle, clean_bin) :- !.
prim(cmd_raw(K, File, Props), Bundle, install_bin) :- !,
	instdir_install(cmd_copy_and_link(K, Bundle, File)),
	( member(link_as(Link), Props) ->
	    instdir_install(cmd_link_as(K, Bundle, File, Link))
	; true
	).
prim(cmd_raw(K, File, Props), Bundle, uninstall_bin) :- !,
	( member(link_as(Link), Props) ->
	    instdir_uninstall(cmd_link(K, Link))
	; true
	),
	instdir_uninstall(cmd_copy_and_link(K, Bundle, File)).
% cmd/1: executable commands
prim(cmd(Path), Bundle, Cmd) :- atom(Path), !,
	path_split(Path, _, Name0),
	( atom_concat(Name, '.pl', Name0) -> true
	; Name = Name0
	),
	prim(cmd(Name, [main=Path]), Bundle, Cmd).
prim(cmd(Name, Opts), Bundle, build_bin) :- !,
	cmd_build(~get_cmd_def(Bundle, Name, Opts)).
prim(cmd(_, _), _Bundle, clean_bin) :- !. % TODO: missing!
prim(cmd(Name, Opts), Bundle, install_bin) :- !,
	% TODO: show the same kind of messages that are used when compiling libraries
	cmd_def_kind(Opts, K),
	( member(libexec, Opts) ->
	    normal_message("installing ~w (libexec)", [Name]),
	    instdir_install(libcmd_copy(K, Bundle, Name))
	; normal_message("installing ~w (command)", [Name]),
	  instdir_install(cmd_copy_and_link(K, Bundle, Name))
	).
prim(cmd(Name, Opts), Bundle, uninstall_bin) :- !,
	% TODO: show the same kind of messages that are used when compiling libraries
	cmd_def_kind(Opts, K),
	( member(libexec, Opts) ->
	    normal_message("uninstalling ~w (libexec)", [Name]),
	    instdir_uninstall(libcmd_copy(K, Bundle, Name))
	; normal_message("uninstalling ~w (command)", [Name]),
	  instdir_uninstall(cmd_copy_and_link(K, Bundle, Name))
	).
% eng/2: engines
% TODO: mimic 'cmd'! (this is a very similar case)
prim(eng(EngMainSpec, EngOpts), Bundle, build_bin) :- !,
	FinalCiaoRoot = ~final_ciao_root,
	EngOpts2 = [default_ciaoroot(FinalCiaoRoot)|EngOpts],
	Eng = eng_def(Bundle, EngMainSpec, EngOpts2),
	eng_build(Eng),
	% Activate
 	eng_active_bld(Eng).
prim(eng(EngMainSpec, EngOpts), Bundle, clean_bin) :- !,
	eng_clean(eng_def(Bundle, EngMainSpec, EngOpts)).
prim(eng(EngMainSpec, EngOpts), Bundle, install_bin) :- !,
	Eng = eng_def(Bundle, EngMainSpec, EngOpts),
	instdir_install(eng_contents(Eng)),
	instdir_install(eng_active(Eng)).
prim(eng(EngMainSpec, EngOpts), Bundle, uninstall_bin) :- !,
	Eng = eng_def(Bundle, EngMainSpec, EngOpts),
	instdir_uninstall(eng_active(Eng)),
	instdir_uninstall(eng_contents(Eng)).
% eng_exec_header/1: engine header stubs for executables
prim(eng_exec_header(eng(EngMainSpec, EngOpts)), Bundle, build_bin) :- !,
	build_eng_exec_header(eng_def(Bundle, EngMainSpec, EngOpts)).
prim(eng_exec_header(eng(EngMainSpec, EngOpts)), Bundle, clean_bin) :- !,
	clean_eng_exec_header(eng_def(Bundle, EngMainSpec, EngOpts)).
prim(eng_exec_header(eng(_EngMainSpec, _EngOpts)), _Bundle, install_bin) :- !,
	% TODO: do nothing -- is it right?
	true.
prim(eng_exec_header(eng(_EngMainSpec, _EngOpts)), _Bundle, uninstall_bin) :- !,
	% TODO: do nothing -- is it right?
	true.
% (Error)
prim(X, _Bundle, Cmd) :-
	throw(error(unknown_primitive_target(X, Cmd), prim/3)).

% TODO: simplify
assets_files_do([], _Bundle, _Path, _Cmd).
assets_files_do([X|Xs], Bundle, Path, Cmd) :-
	assets_file_do(X, Bundle, Path, Cmd),
	assets_files_do(Xs, Bundle, Path, Cmd).

assets_file_do(File, Bundle, Path, install_bin) :- !,
	instdir_install(lib_file(Bundle, ~path_concat(Path, File))).
assets_file_do(File, Bundle, Path, uninstall_bin) :- !,
	instdir_uninstall(lib_file(Bundle, ~path_concat(Path, File))).
	
get_cmd_def(Bundle, Name, Opts) := Def :-
	( AbsPath = ~main_file_path(Bundle, Opts) -> true
	; throw(cmd_requires_main(Name, Opts))
	),
	Def = cmd_def(Bundle, AbsPath, Name, Opts).

% Properties of commands
% TODO: move to ciaoc_aux?
cmd_def_kind(Props, Kind) :-
	( member(kind=Kind, Props) -> true ; Kind=plexe ).

