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
:- use_module(ciaobld(builder_aux), [
    eng_active_bld/1,
    storedir_install/1,
    storedir_uninstall/1
]).
:- use_module(ciaobld(config_common), [
    instciao_bundledir/2
]).
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

:- export(bintgt/1).
:- regtype bintgt(X) # "@var{X} is a primitive target for @tt{bin} grade".
bintgt(lib(_)).
bintgt(lib_force_build(_)).
bintgt(src(_)).
bintgt(files_from(_, _, _)).
bintgt(lib_file_list(_, _)).
bintgt(bin_copy_and_link(_, _, _)).
bintgt(cmd(_)).
bintgt(cmd(_, _)).
bintgt(eng(_, _)).
bintgt(eng_exec_header(_)).

% NOTE: install_bin and uninstall_bin require ~instype=global!
:- export(bintgt_do/3).
% Primitive actions for the bin grade
%
% Commands with no default action in bin grade
bintgt_do(_, _, prebuild_bin) :- !. % (default is nop)
bintgt_do(_, _, register) :- !. % (default is nop)
bintgt_do(_, _, unregister) :- !. % (default is nop)
% lib/1
bintgt_do(lib(DirName), Bundle, build_bin) :- !,
	build_libs(Bundle, ~bundle_path(Bundle, DirName)).
bintgt_do(lib(_), _Bundle, clean_bin) :- !.
bintgt_do(lib(DirName), Bundle, install_bin) :- !,
	% Install the module collection under DirName (along compiled files)
	normal_message("installing ~w/", [DirName]),
	From = ~bundle_path(Bundle, DirName),
	To = ~inst_bundle_path(Bundle, DirName),
	storedir_install(dir_rec(From, To)).
bintgt_do(lib(DirName), Bundle, uninstall_bin) :- !,
	% Uninstall the previously installed module collection DirName
	To = ~inst_bundle_path(Bundle, DirName),
	storedir_uninstall(dir_rec(To)).
% lib_force_build/1
bintgt_do(lib_force_build(DirName), Bundle, build_bin) :- !, % TODO: hack for library/clpq, library/clpr (see core bundle)
	build_libs(Bundle, ~bundle_path(Bundle, DirName)).
bintgt_do(lib_force_build(_), _Bundle, clean_bin) :- !.
bintgt_do(lib_force_build(_), _Bundle, install_bin) :- !. % TODO: assume installed with lib()
bintgt_do(lib_force_build(_), _Bundle, uninstall_bin) :- !. % TODO: assume installed with lib()
% src/1
bintgt_do(src(_DirName), _Bundle, build_bin) :- !. % (only for install_bin)
bintgt_do(src(_DirName), _Bundle, clean_bin) :- !.
bintgt_do(src(DirName), Bundle, install_bin) :- !,
	% Install the module collection under DirName (just source, e.g., for examples)
	normal_message("installing ~w (source)", [DirName]),
	storedir_install(src_dir_rec(~bundle_path(Bundle, DirName), ~inst_bundle_path(Bundle, DirName))).
bintgt_do(src(DirName), Bundle, uninstall_bin) :- !,
	% Uninstall the previously installed source-only module collection DirName
	storedir_uninstall(src_dir_rec(~inst_bundle_path(Bundle, DirName))).
% files_from/3
bintgt_do(files_from(_SrcDir, _Path, _Props), _Bundle, build_bin) :- !.
bintgt_do(files_from(_SrcDir, _Path, _Props), _Bundle, clean_bin) :- !.
bintgt_do(files_from(SrcDir, Path, _Props), Bundle, install_bin) :- !,
	% Copy contents from SrcDir into Path
	storedir_install(dir(Path)),
	storedir_install(dir_rec(~bundle_path(Bundle, SrcDir), Path)).
bintgt_do(files_from(_SrcDir, Path, Props), _Bundle, uninstall_bin) :- !,
	( member(del_rec, Props) ->
	    % on uninstall, remove contents recursively
	    % TODO: remove also the directory?
	    storedir_uninstall(dir_rec(Path))
	; member(do_not_del, Props) ->
	    % do not delete on uninstall
	    true
	; storedir_uninstall(dir(Path))
	).
% lib_file_list/2
bintgt_do(lib_file_list(_Path, _List), _Bundle, build_bin) :- !.
bintgt_do(lib_file_list(_Path, _List), _Bundle, clean_bin) :- !.
bintgt_do(lib_file_list(Path, List), Bundle, install_bin) :- !,
	lib_file_list_do(List, Bundle, ~bundle_path(Bundle, Path), install_bin).
bintgt_do(lib_file_list(Path, List), Bundle, uninstall_bin) :- !,
	lib_file_list_do(List, Bundle, ~bundle_path(Bundle, Path), uninstall_bin).
% bin_copy_and_link/3
bintgt_do(bin_copy_and_link(_K, _File, _Props), _Bundle, build_bin) :- !.
bintgt_do(bin_copy_and_link(_K, _File, _Props), _Bundle, clean_bin) :- !.
bintgt_do(bin_copy_and_link(K, File, Props), Bundle, install_bin) :- !,
	storedir_install(copy_and_link(K, Bundle, File)),
	( member(link_as(Link), Props) ->
	    storedir_install(link_as(K, Bundle, File, Link))
	; true
	).
bintgt_do(bin_copy_and_link(K, File, Props), Bundle, uninstall_bin) :- !,
	( member(link_as(Link), Props) ->
	    storedir_uninstall(link(K, Link))
	; true
	),
	storedir_uninstall(copy_and_link(K, Bundle, File)).
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
	storedir_install(~get_cmd_def(Bundle, Name, Opts)).
bintgt_do(cmd(Name, Opts), Bundle, uninstall_bin) :- !,
 	storedir_uninstall(~get_cmd_def(Bundle, Name, Opts)).
% eng/2: engines
% TODO: mimic 'cmd'! (this is a very similar case)
bintgt_do(eng(EngMainSpec, EngOpts), Bundle, build_bin) :- !,
	Eng = eng_def(Bundle, EngMainSpec, EngOpts),
	eng_build(Eng),
	% Activate
 	eng_active_bld(Eng).
bintgt_do(eng(EngMainSpec, EngOpts), Bundle, clean_bin) :- !,
	eng_clean(eng_def(Bundle, EngMainSpec, EngOpts)).
bintgt_do(eng(EngMainSpec, EngOpts), Bundle, install_bin) :- !,
	Eng = eng_def(Bundle, EngMainSpec, EngOpts),
	storedir_install(eng_contents(Eng)),
	storedir_install(eng_active(Eng)).
bintgt_do(eng(EngMainSpec, EngOpts), Bundle, uninstall_bin) :- !,
	Eng = eng_def(Bundle, EngMainSpec, EngOpts),
	storedir_uninstall(eng_active(Eng)),
	storedir_uninstall(eng_contents(Eng)).
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
lib_file_list_do([], _Bundle, _Path, _Cmd).
lib_file_list_do([X|Xs], Bundle, Path, Cmd) :-
	lib_file_item_do(X, Bundle, Path, Cmd),
	lib_file_list_do(Xs, Bundle, Path, Cmd).

lib_file_item_do(File-Props, Bundle, Path, install_bin) :- !,
	lib_file_props(File, Props, Path, _Props2),
	storedir_install(lib_file_copy_and_link(Bundle, Path, File)).
lib_file_item_do(File-Props, Bundle, Path, uninstall_bin) :- !,
	lib_file_props(File, Props, Path, _Props2),
	storedir_uninstall(lib_file_copy_and_link(Bundle, Path, File)).

lib_file_props(File, Props, Path, Props2) :-
	( member(copy_and_link, Props) ->
	    Props2 = []
	; throw(unknown_props_lib_file(File, Props, Path))
	).
	
% TODO: use in more code?
inst_bundle_path(Bundle, Rel) := R :-
	R0 = ~instciao_bundledir(Bundle),
	( Rel = '.' ->
	    R = R0
	; R = ~path_concat(R0, Rel)
	).

get_cmd_def(Bundle, Name, Opts) := Def :-
	( member(main=Path, Opts) -> true
	; throw(cmd_requires_main(Name, Opts))
	),
	Def = cmd_def(Bundle, ~bundle_path(Bundle, Path), Name, Opts).

% ---------------------------------------------------------------------------
% Directories for install bin (bindir, storedir, bundledir)

:- use_module(ciaobld(config_common), [
    instciao_bindir/1,
    instciao_storedir/1,
    instciao_bundledir/2
]).

:- export(install_bin_dirs/1).
install_bin_dirs(Bundle) :-
	storedir_install(dir(~instciao_bindir)),
	storedir_install(dir(~instciao_storedir)),
	storedir_install(dir(~instciao_bundledir(Bundle))).

:- export(uninstall_bin_dirs/1).
uninstall_bin_dirs(Bundle) :-
	storedir_uninstall(dir_rec(~instciao_bundledir(Bundle))),
	% delete if empty
	storedir_uninstall(dir_if_empty(~instciao_storedir)),
	% Keep ~instciao_bindir (e.g., it may be /usr/bin)
	%% storedir_uninstall(dir_if_empty(~instciao_bindir)).
	true.

% ---------------------------------------------------------------------------
% Bundle registry installation (after bundle scan!)
% TODO: rename those predicates?
% (this rewrites bundlereg in the global installation area, with modified paths)

:- use_module(ciaobld(bundle_scan),
	[create_bundlereg/2, remove_bundlereg/2,
	 ensure_global_bundle_reg_dir/0,
	 rootprefix_bundle_reg_dir/2]).

:- export(install_bundlereg/1).
install_bundlereg(Bundle) :-
	BundleDir = ~bundle_path(Bundle, '.'),
	ensure_global_bundle_reg_dir,
	create_bundlereg(BundleDir, global),
	install_bundle_flags(Bundle).

:- export(uninstall_bundlereg/1).
uninstall_bundlereg(Bundle) :-
	ensure_global_bundle_reg_dir,
	uninstall_bundle_flags(Bundle),
	remove_bundlereg(Bundle, global).

% ---------------------------------------------------------------------------
% Bundle configuration flags installation
% (attached to bundle registry)

:- use_module(library(bundle/bundle_flags),
	[bundle_flags_file/2, bundlecfg_filename/3]).
:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(library(source_tree), [copy_file_or_dir/2]).

% NOTE: needs ~instype = global
%:- export(install_bundle_flags/1).
install_bundle_flags(Bundle) :-
	CfgFile = ~bundle_flags_file(Bundle),
	rootprefix_bundlecfg_file(global, Bundle, InsCfgFile),
	copy_file_or_dir(CfgFile, InsCfgFile). % TODO: never a dir!

% NOTE: needs ~instype = global
%:- export(uninstall_bundle_flags/1).
uninstall_bundle_flags(Bundle) :-
	rootprefix_bundlecfg_file(global, Bundle, InsCfgFile),
	del_file_nofail(InsCfgFile).

% File is the registry file for the BundleName bundle
rootprefix_bundlecfg_file(InsType, BundleName, RegFile) :-
	rootprefix_bundle_reg_dir(InsType, BundleRegDir),
	bundlecfg_filename(BundleName, BundleRegDir, RegFile).

% ===========================================================================
:- doc(section, "Primitive targets for docs grade").

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(bundle/bundle_info), [bundle_version/2]).
:- use_module(library(bundle/doc_flags), [docformat/1]).
:- use_module(ciaobld(lpdoc_aux), [build_docs_readme/3, build_doc/2]).
:- use_module(ciaobld(lpdoc_aux), [install_doc/3, uninstall_doc/3]).
:- use_module(ciaobld(builder_aux), [
    versioned_manual_base/3
]).

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
	versioned_manual_base(Bundle, Base, R),
	( % (failure-driven loop)
	  docformat(DocFormat), % (nondet)
	    install_doc(Bundle, R, DocFormat),
	    fail
	; true
	).

uninstall_doc_all_formats(Bundle, Base) :-
	versioned_manual_base(Bundle, Base, R),
	( % (failure-driven loop)
	  docformat(DocFormat), % (nondet)
	    uninstall_doc(Bundle, R, DocFormat),
	    fail
	; true
	).

