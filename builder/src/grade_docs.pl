:- module(_, [], [fsyntax, hiord, assertions, regtypes, isomodes]).

:- doc(title, "Docs grade").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module contains command definitions for the
   @tt{docs} grade, which builds/clean/install/uninstall documentation
   from sources.").

:- use_module(ciaobld(builder_cmds), [builder_cmd/2, root_target/1]).
:- use_module(ciaobld(manifest_compiler), [main_file_relpath/2]).

% ---------------------------------------------------------------------------

:- include(ciaobld(cmd_hooks)).

% ---------------------------------------------------------------------------

% Any target requires a bin build of lpdoc
'grade.requires'(docs, bin, 'lpdoc').

% ---------------------------------------------------------------------------
% build/clean (docs)

:- use_module(ciaobld(builder_aux), [builddir_clean/2]).

'grade.cmd'(docs, build, build_docs).
'grade.cmd'(docs, clean, clean_docs).

'cmd.comment'(build_docs, ["building [docs]", "built [docs]"]).
'cmd.grade'(build_docs, docs).
'cmd.needs_update_builder'(build_docs).
'cmd.needs_rescan'(build_docs).
'cmd.needs_config'(build_docs).
'cmd.recursive'(build_docs, forward).
'cmd.do_before.decl'(build_docs).
'cmd.do_before'(build_docs, Target) :- !,
	builder_cmd(prepare_build_docs, Target).

'cmd.comment'(prepare_build_docs, ["preparing build [docs]", "prepared build [docs]"]).
'cmd.grade'(prepare_build_docs, custom_docs).
%'cmd.needs_config'(prepare_build_docs).
%'cmd.recursive'(prepare_build_docs, forward).

% Clean documentation
'cmd.comment'(clean_docs, ["cleaning [docs]", "cleaned [docs]"]).
'cmd.grade'(clean_docs, docs).
'cmd.needs_config'(clean_docs).
'cmd.recursive'(clean_docs, backward).
'cmd.do_after.decl'(clean_docs).
'cmd.do_after'(clean_docs, Target) :- !,
	( root_target(Target) -> % TODO: generalize for all workspaces
	    builddir_clean(core, doc) % TODO: 'core' hardwired
	; true
	).

% ---------------------------------------------------------------------------
% install/uninstall (docs)

'grade.cmd'(docs, install, install_docs).
'grade.cmd'(docs, uninstall, uninstall_docs).

'cmd.comment'(install_docs, ["installing [docs]", "installed [docs]"]).
'cmd.grade'(install_docs, docs).
%'cmd.needs_update_builder'(install_docs).
'cmd.needs_rescan'(install_docs).
'cmd.recursive'(install_docs, forward).

'cmd.comment'(uninstall_docs, ["uninstalling [docs]", "uninstalled [docs]"]).
'cmd.grade'(uninstall_docs, docs).
'cmd.recursive'(uninstall_docs, backward).

% ---------------------------------------------------------------------------
% Primitive targets for docs grade

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(bundle/bundle_info), [bundle_version/2]).
:- use_module(ciaobld(config_common), [docformat/1]).
:- use_module(ciaobld(lpdoc_aux), [build_docs_readme/3, build_doc/2]).
:- use_module(ciaobld(install_aux), [install_doc/3, uninstall_doc/3]).
:- use_module(ciaobld(messages_aux), [normal_message/2]).

'grade.prim_kind'(docs, docs) :- !.
'grade.prim_do'(docs, Prim, Bundle, Cmd) :- !,
	prim(Prim, Bundle, Cmd).

prim(_, _, prebuild_docs) :- !. % (default is nop)
prim(readme(OutName, Props), Bundle, build_docs) :- !,
	normal_message("generating ~w (file)", [OutName]),
	build_docs_readme(Bundle, ~main_file_relpath(Props), OutName).
prim(readme(_OutName, _Props), _Bundle, clean_docs) :- !,
	% (Not cleaned, assuming they are part of the sources)
	% R = ~bundle_path(_Bundle, _OutName).
	% del_file_nofail(R).
	true.
prim(readme(_OutName, _Props), _Bundle, install_docs) :- !,
	% Not installed (part of the sources)
	true.
prim(readme(_OutName, _Props), _Bundle, uninstall_docs) :- !,
	% Not installed (part of the sources)
	true.
%
prim(manual(Base, Props), Bundle, build_docs) :- !,
	normal_message("generating ~w (manual)", [Base]),
	build_doc(Bundle, ~main_file_relpath(Props)).
prim(manual(_Base, _Props), _Bundle, clean_docs) :- !,
	% TODO: use Manifest, use lpdoc to clean?
	true.
prim(manual(Base, _Props), Bundle, install_docs) :- !,
	normal_message("installing ~w (manual)", [Base]),
	install_doc_all_formats(Bundle, Base).
prim(manual(Base, _Props), Bundle, uninstall_docs) :- !,
	normal_message("uninstalling ~w (manual)", [Base]),
	uninstall_doc_all_formats(Bundle, Base).
prim(X, Bundle, Cmd) :- !,
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

