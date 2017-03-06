:- module(_, [], [fsyntax, hiord, assertions, regtypes, isomodes]).

:- doc(title,  "Primitive builder targets").
:- doc(author, "Jose F. Morales").

:- doc(module, "Commands for primitive targets that define a bundle
   (libraries, command-line executables, engines, file assets,
   documentation, etc.).").

% ===========================================================================
:- doc(section, "Primitive targets for bin grade").

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

:- export(bintgt/1).
:- regtype bintgt(X) # "@var{X} is a primitive target for @tt{bin} grade".
% (Path is relative to bundle)
bintgt(lib(_Path)). % Source and compiled
bintgt(lib_force_build(_Path)). % Force build of _Path (use in combination with lib/1)
bintgt(src(_Path)). % Only source code
bintgt(assets(_Path)). % Other files (under _Path)
bintgt(assets(_Path, _Files)). % Explicit files under Path
bintgt(cmd(_)). % A command (module with main/{0,1})
bintgt(cmd(_, _)). % A command (module with main/{0,1})
bintgt(cmd_raw(_, _, _)).
bintgt(eng(_, _)). % An engine
bintgt(eng_exec_header(_)). % Engine stub loader

% NOTE: install_bin and uninstall_bin require ~instype=global!
:- export(bintgt_do/3).
% Primitive actions for the bin grade
%
% Commands with no default action in bin grade
bintgt_do(_, _, prebuild_bin) :- !. % (default is nop)
bintgt_do(_, _, register) :- !. % (default is nop)
bintgt_do(_, _, unregister) :- !. % (default is nop)
% lib/1
bintgt_do(lib(Path), Bundle, build_bin) :- !,
	build_libs(Bundle, ~bundle_path(Bundle, Path)).
bintgt_do(lib(_), _Bundle, clean_bin) :- !.
bintgt_do(lib(Path), Bundle, install_bin) :- !,
	% Install the module collection under Path (along compiled files)
	% TODO: needs update for CIAOCACHEDIR!
	normal_message("installing ~w/", [Path]),
	From = ~bundle_path(Bundle, Path),
	To = ~inst_bundle_path(Bundle, Path),
	instdir_install(dir_rec(From, To)).
bintgt_do(lib(Path), Bundle, uninstall_bin) :- !,
	% Uninstall the previously installed module collection Path
	% TODO: needs update for CIAOCACHEDIR!
	To = ~inst_bundle_path(Bundle, Path),
	instdir_uninstall(dir_rec(To)).
% lib_force_build/1
bintgt_do(lib_force_build(Path), Bundle, build_bin) :- !, % TODO: hack for library/clpq, library/clpr (see core bundle)
	build_libs(Bundle, ~bundle_path(Bundle, Path)).
bintgt_do(lib_force_build(_), _Bundle, clean_bin) :- !.
bintgt_do(lib_force_build(_), _Bundle, install_bin) :- !. % TODO: assume installed with lib()
bintgt_do(lib_force_build(_), _Bundle, uninstall_bin) :- !. % TODO: assume installed with lib()
% src/1
bintgt_do(src(_Path), _Bundle, build_bin) :- !. % (only for install_bin)
bintgt_do(src(_Path), _Bundle, clean_bin) :- !.
bintgt_do(src(Path), Bundle, install_bin) :- !,
	% Install the module collection under Path (just source, e.g., for examples)
	normal_message("installing ~w (source)", [Path]),
	instdir_install(src_dir_rec(~bundle_path(Bundle, Path), ~inst_bundle_path(Bundle, Path))).
bintgt_do(src(Path), Bundle, uninstall_bin) :- !,
	% Uninstall the previously installed source-only module collection Path
	instdir_uninstall(src_dir_rec(~inst_bundle_path(Bundle, Path))).
% assets/1
bintgt_do(assets(_Path), _Bundle, build_bin) :- !.
bintgt_do(assets(_Path), _Bundle, clean_bin) :- !.
bintgt_do(assets(Path), Bundle, install_bin) :- !,
	% Copy all files from Path
	From = ~bundle_path(Bundle, Path),
	To = ~inst_bundle_path(Bundle, Path),
	instdir_install(dir(To)),
	instdir_install(dir_rec(From, To)).
bintgt_do(assets(Path), Bundle, uninstall_bin) :- !,
	% on uninstall, remove contents recursively
	% TODO: remove also the directory?
	To = ~inst_bundle_path(Bundle, Path),
	instdir_uninstall(dir_rec(To)).
% assets/2
bintgt_do(assets(_Path, _List), _Bundle, build_bin) :- !.
bintgt_do(assets(_Path, _List), _Bundle, clean_bin) :- !.
bintgt_do(assets(Path, List), Bundle, install_bin) :- !,
	instdir_install(dir(~inst_bundle_path(Bundle, Path))),
	assets_files_do(List, Bundle, Path, install_bin).
bintgt_do(assets(Path, List), Bundle, uninstall_bin) :- !,
	assets_files_do(List, Bundle, Path, uninstall_bin),
	instdir_uninstall(dir(~inst_bundle_path(Bundle, Path))).
% cmd_raw/3
bintgt_do(cmd_raw(_K, _File, _Props), _Bundle, build_bin) :- !.
bintgt_do(cmd_raw(_K, _File, _Props), _Bundle, clean_bin) :- !.
bintgt_do(cmd_raw(K, File, Props), Bundle, install_bin) :- !,
	instdir_install(cmd_copy_and_link(K, Bundle, File)),
	( member(link_as(Link), Props) ->
	    instdir_install(cmd_link_as(K, Bundle, File, Link))
	; true
	).
bintgt_do(cmd_raw(K, File, Props), Bundle, uninstall_bin) :- !,
	( member(link_as(Link), Props) ->
	    instdir_uninstall(cmd_link(K, Link))
	; true
	),
	instdir_uninstall(cmd_copy_and_link(K, Bundle, File)).
% cmd/1: executable commands
bintgt_do(cmd(Path), Bundle, Cmd) :- atom(Path), !,
	path_split(Path, _, Name0),
	( atom_concat(Name, '.pl', Name0) -> true
	; Name = Name0
	),
	bintgt_do(cmd(Name, [main=Path]), Bundle, Cmd).
bintgt_do(cmd(Name, Opts), Bundle, build_bin) :- !,
	cmd_build(~get_cmd_def(Bundle, Name, Opts)).
bintgt_do(cmd(_, _), _Bundle, clean_bin) :- !. % TODO: missing!
bintgt_do(cmd(Name, Opts), Bundle, install_bin) :- !,
	% TODO: show the same kind of messages that are used when compiling libraries
	cmd_def_kind(Opts, K),
	normal_message("installing ~w (command)", [Name]),
	instdir_install(cmd_copy_and_link(K, Bundle, Name)).
bintgt_do(cmd(Name, Opts), Bundle, uninstall_bin) :- !,
	% TODO: show the same kind of messages that are used when compiling libraries
	cmd_def_kind(Opts, K),
	normal_message("uninstalling ~w (command)", [Name]),
	instdir_uninstall(cmd_copy_and_link(K, Bundle, Name)).
% eng/2: engines
% TODO: mimic 'cmd'! (this is a very similar case)
bintgt_do(eng(EngMainSpec, EngOpts), Bundle, build_bin) :- !,
	FinalCiaoRoot = ~final_ciao_root,
	EngOpts2 = [default_ciaoroot(FinalCiaoRoot)|EngOpts],
	Eng = eng_def(Bundle, EngMainSpec, EngOpts2),
	eng_build(Eng),
	% Activate
 	eng_active_bld(Eng).
bintgt_do(eng(EngMainSpec, EngOpts), Bundle, clean_bin) :- !,
	eng_clean(eng_def(Bundle, EngMainSpec, EngOpts)).
bintgt_do(eng(EngMainSpec, EngOpts), Bundle, install_bin) :- !,
	Eng = eng_def(Bundle, EngMainSpec, EngOpts),
	instdir_install(eng_contents(Eng)),
	instdir_install(eng_active(Eng)).
bintgt_do(eng(EngMainSpec, EngOpts), Bundle, uninstall_bin) :- !,
	Eng = eng_def(Bundle, EngMainSpec, EngOpts),
	instdir_uninstall(eng_active(Eng)),
	instdir_uninstall(eng_contents(Eng)).
% eng_exec_header/1: engine header stubs for executables
bintgt_do(eng_exec_header(eng(EngMainSpec, EngOpts)), Bundle, build_bin) :- !,
	build_eng_exec_header(eng_def(Bundle, EngMainSpec, EngOpts)).
bintgt_do(eng_exec_header(eng(EngMainSpec, EngOpts)), Bundle, clean_bin) :- !,
	clean_eng_exec_header(eng_def(Bundle, EngMainSpec, EngOpts)).
bintgt_do(eng_exec_header(eng(_EngMainSpec, _EngOpts)), _Bundle, install_bin) :- !,
	% TODO: do nothing -- is it right?
	true.
bintgt_do(eng_exec_header(eng(_EngMainSpec, _EngOpts)), _Bundle, uninstall_bin) :- !,
	% TODO: do nothing -- is it right?
	true.
% (Error)
bintgt_do(X, _Bundle, Cmd) :-
	throw(error(unknown_primitive_target(X, Cmd), bintgt_do/3)).

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
	( member(main=Path, Opts) -> true
	; throw(cmd_requires_main(Name, Opts))
	),
	Def = cmd_def(Bundle, ~bundle_path(Bundle, Path), Name, Opts).

% Properties of commands
% TODO: move to ciaoc_aux?
cmd_def_kind(Props, Kind) :-
	( member(kind=Kind, Props) -> true ; Kind=plexe ).

% ===========================================================================
:- doc(section, "Primitive targets for docs grade").

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(bundle/bundle_info), [bundle_version/2]).
:- use_module(ciaobld(config_common), [docformat/1]).
:- use_module(ciaobld(lpdoc_aux), [build_docs_readme/3, build_doc/2]).
:- use_module(ciaobld(install_aux), [install_doc/3, uninstall_doc/3]).

:- export(docstgt/1).
:- regtype bintgt(X) # "@var{X} is a primitive target for @tt{docs} grade".
docstgt(readme(_,_)).
docstgt(manual(_,_)).

:- export(docstgt_do/3).
docstgt_do(_, _, prebuild_docs) :- !. % (default is nop)
docstgt_do(readme(OutName, Props), Bundle, build_docs) :- !,
	normal_message("generating ~w (file)", [OutName]),
	( member(main=SrcPath, Props) -> true
	; fail % ill-formed
	),
	build_docs_readme(Bundle, SrcPath, OutName).
docstgt_do(readme(_OutName, _Props), _Bundle, clean_docs) :- !,
	% (Not cleaned, assuming they are part of the sources)
	% R = ~bundle_path(_Bundle, _OutName).
	% del_file_nofail(R).
	true.
docstgt_do(readme(_OutName, _Props), _Bundle, install_docs) :- !,
	% Not installed (part of the sources)
	true.
docstgt_do(readme(_OutName, _Props), _Bundle, uninstall_docs) :- !,
	% Not installed (part of the sources)
	true.
%
docstgt_do(manual(Base, Props), Bundle, build_docs) :- !,
	normal_message("generating ~w (manual)", [Base]),
	( member(main=Path, Props) -> true
	; fail % ill-formed
	),
	build_doc(Bundle, Path).
docstgt_do(manual(_Base, _Props), _Bundle, clean_docs) :- !,
	% TODO: use Manifest, use lpdoc to clean?
	true.
docstgt_do(manual(Base, _Props), Bundle, install_docs) :- !,
	normal_message("installing ~w (manual)", [Base]),
	install_doc_all_formats(Bundle, Base).
docstgt_do(manual(Base, _Props), Bundle, uninstall_docs) :- !,
	normal_message("uninstalling ~w (manual)", [Base]),
	uninstall_doc_all_formats(Bundle, Base).
docstgt_do(X, Bundle, Cmd) :- !,
	throw(unknown_docstgt(X, Bundle, Cmd)).

install_doc_all_formats(Bundle, Base) :-
	( % (failure-driven loop)
	  docformat(DocFormat), % (nondet)
	    install_doc(Bundle, Base, DocFormat),
	    fail
	; true
	).

uninstall_doc_all_formats(Bundle, Base) :-
	( % (failure-driven loop)
	  docformat(DocFormat), % (nondet)
	    uninstall_doc(Bundle, Base, DocFormat),
	    fail
	; true
	).

