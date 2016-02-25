:- module(docmaker, [make_doc/4], [assertions, regtypes, dcg, basicmodes, fsyntax]).

:- doc(title, "Driver for Documentation Targets").
:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Jose F. Morales").

:- doc(module, "This library drives the documentation generation from
   a @index{SETTINGS.pl} file").

%% ISO Prolog-like modules
:- use_module(library(format)).
:- use_module(library(aggregates)).
:- use_module(library(compiler), [use_module/1]).

%% Ciao packages and libraries
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(messages)).
:- use_module(library(lists), [list_concat/2, append/3]).
:- use_module(library(file_utils)).
:- use_module(library(pathnames), [path_concat/3, path_dirname/2]).

%% LPdoc libraries
:- use_module(lpdoc(autodoc)).
:- use_module(lpdoc(autodoc_state)).
:- use_module(lpdoc(autodoc_images), [clean_image_cache/0]).
:- use_module(lpdoc(autodoc_structure)).
:- use_module(lpdoc(autodoc_settings)).
:- use_module(lpdoc(autodoc_filesystem)).
:- use_module(lpdoc(autodoc_texinfo), [infodir_base/2]).

:- use_module(library(make/make_rt)).

% ===========================================================================

:- use_module(library(port_reify), [once_port_reify/2, port_call/1]).
:- use_module(library(system), [working_directory/2, cd/1]).

% TODO: document Opts better, change format (e.g., N(V) instead of name_value(N,V))
:- pred make_doc(ConfigFile, Opts, Targets, OutputDir) # "Process
   @var{Targets} given @var{ConfigFile}, producing all output in
   @var{OutputDir}. If @var{OutputDir} is free, directory name of
   @var{ConfigFile} is used (similar to @pred{make_exec/2}).

   @var{Opts} is the list of options for @lib{make_rt} (see
   @pred{set_make_opts/1}).".

make_doc(ConfigFile, Opts, Targets, OutputDir) :-
	% Load settings and set make_rt opts
	load_settings(ConfigFile, Opts),
	% Call and cleanup
	working_directory(WD, WD),
	( var(OutputDir) ->
	    OutputDir = ~path_dirname(~settings_file)
	; true
	),
	cd(OutputDir),
	once_port_reify(make_doc_(Targets), Port),
	cd(WD), % move to original directory
	% TODO: cleanup all databases here too
	port_call(Port).

make_doc_(Targets) :-
	verify_settings,	
	clean_fs_db,
	clean_image_cache,
	reset_output_dir_db,
	load_vpaths,
	parse_structure,
	%
	process_targets(Targets).

% ---------------------------------------------------------------------------
% Process the targets

% standalone target bases (does not depend on a settings file)
:- data standalone_target_base/1.

process_targets(Targets) :-
	retractall_fact(standalone_target_base(_)),
	process_targets_(Targets).

process_targets_([]) :- !.
process_targets_(['-c', Target|Targets]) :- !,
	process_standalone(Target),
	process_targets_(Targets).
process_targets_([Target|Targets]) :- !,
	process_target(Target),
	process_targets_(Targets).

process_target(Target) :-
	report_make_target('Starting', Target),
	make(Target),
	report_make_target('Finished', Target).

%% Treat Target as a separated component
process_standalone(Target) :-
	base_from_target(Target, Base),
	( standalone_target_base(Base) ->
	    % (already processed)
	    true
	; assertz_fact(standalone_target_base(Base)),
	  report_make_target('Starting', Base),
	  standalone_docstr(Base),
	  ( make(Target) -> Ok = yes ; Ok = no ),
	  clean_docstr,
	  Ok = yes,
	  report_make_target('Finished', Base)
	).

% Obtain the name of a target (by removing the suffix, which must be a
% supported one)
base_from_target(Target) := Base :-
	( supported_file_format(Suffix),
	  atom_concat([Base, '.', Suffix], Target) ->
	    true
	; Base = Target
	).

report_make_target(BegEnd, Ext) :-
	file_format_name(Ext, FormatName),
	!,
	simple_message("~w manual generation in ~w (~w) format.",
	    [BegEnd, Ext, FormatName]).
report_make_target(BegEnd, Base) :-
	simple_message("~w processing of '~w'.", [BegEnd, Base]).

% ===========================================================================

:- doc(section, "Command Entries (lpmake)").

:- multifile m_do_target_atm/3.
:- multifile m_do_target_var/3.
:- multifile m_target_exists/2.
:- multifile m_target_deps/4.
:- multifile m_target_comment/4.
:- multifile m_do_dependency/4.
:- multifile m_dependency_exists/3.
:- multifile m_dependency_precond/4.

:- discontiguous do_target_atm/2.
:- discontiguous do_target_var/2.
:- discontiguous target_exists/1.
:- discontiguous target_deps/3.
:- discontiguous target_comment/3.
:- discontiguous do_dependency/3.
:- discontiguous dependency_exists/2.
:- discontiguous dependency_precond/3.

m_do_target_atm(docmaker,_1,_2) :-
        docmaker:do_target_atm(_1,_2).
m_do_target_var(docmaker,_1,_2) :-
        docmaker:do_target_var(_1,_2).
m_target_exists(docmaker,_1) :-
        docmaker:target_exists(_1).
m_target_deps(docmaker,_1,_2,_3) :-
        docmaker:target_deps(_1,_2,_3).
m_target_comment(docmaker,_1,_2,_3) :-
        docmaker:target_comment(_1,_2,_3).

%% ---------------------------------------------------------------------------

% Note: do not confuse a file format (e.g. PDF) with a backend (texinfo)

% Generate all the requested file formats (in SETTINGS.pl)
target_comment(all,"Generates all documentation files specified by docformat/1",[]).
target_deps(all,Formats,[]) :-
        !,
        requested_file_formats(Formats).
target_exists(all).
do_target_atm(all,[]).

% Generate one file format (not necessarily requested in SETTINGS.pl)
target_deps(Suffix,DepFiles,[Suffix]) :-
	supported_file_format(Suffix),
        !,
	top_suffix(Suffix,PrincipalExt),
        main_absfile_in_format(PrincipalExt,DepF),
	DepFiles = [DepF].
target_exists(Suffix) :-
        supported_file_format(Suffix).
do_target_var(Suffix,[Suffix]) :-
        supported_file_format(Suffix),
        !,
        true.

% ---------------------------------------------------------------------------

:- use_module(library(pathnames), [path_basename/2]).

:- doc(subsection, "Rules for Documentation Generation").
% Schematically, there are the rules that defines how documentation is
% generated for a specific backend. Let A be a file, Main the mainmod
% and Ci the components:
%
%   1) dr(A) <- source(A) + <SETTINGS>
%   2) gr(Main) <- [dr(Main), dr(C1),...,dr(Cn)]
%   3) cr(A) <- [gr(Main),dr(A)]
%   4) fr(Main) <- [cr(Main),cr(C1),...,cr(Cn)]
%
% NOTE: Dependency to SETTINGS could be refined

% 1) Doctree and references from each source
target_deps(F,DepFiles,[Spec,SourceSuffix]) :-
	dr_absfiles(Spec, _Backend, F),
        !,
	query_source(Spec,SourceSuffix,AbsFile),
	add_settings_dep(AbsFile,Spec,DepFiles).
target_exists(F) :-
	dr_absfiles(_Spec, _Backend, F).
do_target_var(F,[Spec,SourceSuffix]) :-
	dr_absfiles(Spec, Backend, F),
        !,
        gen_doctree(Backend, Spec, SourceSuffix).

% dr_absfiles(-Spec, -Backend, -F)
dr_absfiles(Spec, Backend, F) :-
	Spec = ~all_specs,
	path_basename(Spec,Name),
	Backend = ~backend_id,
	absfile_for_subtarget(Name,Backend,dr,F).

% 2) Globally resolved references
target_deps(F,DepFiles,[Backend]) :-
	Backend = ~backend_id,
        main_absfile_for_subtarget(Backend,gr,F),
        !,
	main_absfile_for_subtarget(Backend,dr,Fdr),
	components_target(Backend,dr,FdrComps),
        DepFiles=[Fdr|FdrComps].
target_exists(F) :-
        Backend = ~backend_id,
        main_absfile_for_subtarget(Backend,gr,F).
do_target_var(F,[Backend]) :-
        Backend = ~backend_id,
	main_absfile_for_subtarget(Backend,gr,F),
        !,
        compute_grefs(Backend).

% 3) Backend-specific temporary result
target_deps(F,DepFiles,[Spec,Backend]) :-
	Spec = ~all_specs,
	path_basename(Spec,Name),
	Backend = ~backend_id,
	absfile_for_subtarget(Name,Backend,cr,F),
        !,
%	path_basename(Spec,Name),
	absfile_for_subtarget(Name,Backend,dr,Fdr),
	main_absfile_for_subtarget(Backend,gr,Fgr),
        DepFiles=[Fdr,Fgr].
target_exists(F) :-
        Spec = ~all_specs,
        path_basename(Spec,Name),
        Backend = ~backend_id,
        absfile_for_subtarget(Name,Backend,cr,F).
do_target_var(F,[Spec,Backend]) :-
	Spec = ~all_specs,
	path_basename(Spec,Name),
	Backend = ~backend_id,
	absfile_for_subtarget(Name,Backend,cr,F),
        !,
	path_basename(Spec,Name),
        translate_doctree(Backend,Name).

% 4) Backend-specific final result
target_deps(F,DepFiles,[Backend]) :-
	Backend = ~backend_id,
	main_absfile_for_subtarget(Backend,fr,F),
        !,
	main_absfile_for_subtarget(Backend,cr,Fcr),
	components_target(Backend,cr,FcrComps),
        DepFiles=[Fcr|FcrComps].
target_exists(F) :-
        Backend = ~backend_id,
        main_absfile_for_subtarget(Backend,fr,F).
do_target_var(F,[Backend]) :-
	Backend = ~backend_id,
	main_absfile_for_subtarget(Backend,fr,F),
        !,
        simple_message("Post-processing the document.",[]),
        autodoc_finish(Backend).

% (extra) Alternative final results (e.g. PDF, PS, etc.)
% [Rules for generating DVI, PS, and PDF from texi]
target_deps(F,DepFiles,[Backend]) :-
	Backend = ~backend_id,
	Alt = ~backend_alt_format(Backend),
	main_absfile_in_format(Alt,F),
        !,
	main_absfile_for_subtarget(Backend,fr,Ffr),
        DepFiles=[Ffr].
target_exists(F) :-
	Backend = ~backend_id,
	Alt = ~backend_alt_format(Backend),
        main_absfile_in_format(Alt,F).
do_target_var(F,[Backend]) :-
	Backend = ~backend_id,
	Alt = ~backend_alt_format(Backend),
	main_absfile_in_format(Alt,F),
        !,
        simple_message("Generating document in ~w format.",[Alt]),
        autodoc_gen_alternative(Backend,Alt).

% ---------------------------------------------------------------------------

:- doc(subsection, "Some Auxiliary Definitions").
% (for the rules above)

components_target(Backend, _Subtarget, []) :- backend_ignores_components(Backend), !.
components_target(Backend, Subtarget, CompTargets) :-
	CompTargets = ~target_files(~all_component_specs, Backend, Subtarget).

target_files([],           _, _) := [].
target_files([FileBase|FileBases], Backend, Subtarget) := [NewFile|NewFiles] :-
	path_basename(FileBase, Name),
	NewFile = ~absfile_for_subtarget(Name, Backend, Subtarget),
	NewFiles = ~target_files(FileBases, Backend, Subtarget).

query_source(Spec, S) := Main :-
	find_source(Spec, S, Main, _),
	!.
query_source(Spec, _S) := _Main :-
	throw(make_error("Source file not found: ~w", [Spec])).

all_specs := B :-
	( B = ~get_mainmod_spec
	; Bs = ~all_component_specs,
	  member(B, Bs)
	).

settings_absfile(F) :-
	F0 = ~fixed_absolute_file_name(~settings_file),
	( atom_concat(_, '.pl', F0) ->
	    F = F0
	; atom_concat(F0, '.pl', F)
	).

add_settings_dep(SpecF, Spec) := [SpecF|Fs] :-
	( standalone_target_base(Spec) ->
	    Fs = []
	; Fs = [~settings_absfile]
	).

% ---------------------------------------------------------------------------

:- doc(subsection, "Steps of Documentation Generation").

gen_doctree(Backend, FileBase, SourceSuffix) :-
	ensure_cache_dir(Backend),
	path_basename(FileBase, Name),
%	display(user_error, generating_doctree(Backend, Name)), nl(user_error),
	get_autodoc_opts(Backend, Name, Opts),
	autodoc_gen_doctree(Backend, FileBase, SourceSuffix, Opts, Name).

compute_grefs(Backend) :-
	Mod = ~get_mainmod,
	get_autodoc_opts(Backend, Mod, Opts),
%	display(user_error, computing_grefs(Backend, Mod)), nl(user_error),
	autodoc_compute_grefs(Backend, Mod, Opts).

translate_doctree(Backend, FileBase) :-
	path_basename(FileBase, Base),
%	display(user_error, translating_doctree(Backend, Base)), nl(user_error),
	get_autodoc_opts(Backend, Base, Opts),
	ensure_output_dir_prepared(Backend, Opts),
	autodoc_translate_doctree(Backend, Opts, Base).

%% ===========================================================================

:- doc(section, "Commands for Output Visualization").

target_comment(view, "Visualize default format (.html)", []).
target_comment(pdfview, "Visualize .pdf (with a default viewer)", []).
target_comment(psview, "Visualize .ps (with a default viewer)", []).
target_comment(htmlview, "Visualize .html (with a default viewer)", []).
target_comment(infoview, "Visualize .info (with a default viewer)", []).
target_comment(manlview, "Visualize .manl (with a default viewer)", []).

viewcmd(view, html).
viewcmd(pdfview, pdf).
viewcmd(psview, ps).
viewcmd(htmlview, html).
viewcmd(infoview, info).
viewcmd(manlview, manl).

target_deps(Cmd,Deps,[]) :- viewcmd(Cmd, _), !, Deps = [].
target_exists(Cmd) :- viewcmd(Cmd, _), !.
do_target_atm(Cmd,[]) :- viewcmd(Cmd, Suffix), !, view(Suffix).

view(Suffix) :-
	main_absfile_in_format(Suffix, File),
	view_document(Suffix, File).

:- use_module(library(process), [process_call/3]).

view_document(Suffix, File) :-
	( view_in_emacs(Suffix, EmacsFun) ->
	    % TODO: Use absolute file name instead?
	    % Escape file name (for elisp expression)
	    File0 = ~atom_concat('./', File),
	    atom_codes(File0, File1),
	    esc_codes(File1, File2, []),
	    atom_codes(File3, File2),
	    Code = ~atom_concat(['(', EmacsFun, ' \"', File3, '\")']),
	    %
	    process_call(path(emacsclient), ['-n', '--eval', Code], [])
	; generic_viewer(Viewer),
	  process_call(path(Viewer), [File], [])
	).

% view_in_emacs(Format, Fun): 
%   view Format document with elisp function Fun
view_in_emacs(info, info).
view_in_emacs(manl, man).

% TODO: merge esc_codes/3 with elisp_interface.pl

% string-escaped codes
esc_codes([]) --> [].
esc_codes([X|Xs]) --> esc_code(X), esc_codes(Xs).

esc_code(0'") --> !, "\\\"".
esc_code(0'\\) --> !, "\\\\".
esc_code(X) --> [X].

%% ===========================================================================

:- doc(section, "Cleaning up Commands").
% (after generating manuals)

target_comment(clean, "Delete intermediate files. Leaves .texi & docs", []).
target_comment(docsclean, "Delete all generated document files (temporary and final). Leaves only .texi", []).
target_comment(distclean, "Delete all temporary files, including .texi", []).
target_comment(realclean, "Deletes everything", []).

target_deps(Cmd,Deps,[]) :- cleancmd(Cmd, _), !, Deps = [].
target_exists(Cmd) :- cleancmd(Cmd, _), !.
do_target_atm(Cmd,[]) :- cleancmd(Cmd, Mode), !, clean(Mode).

cleancmd(clean, intermediate).
cleancmd(docsclean, docs_no_texi).
cleancmd(distclean, all_temporary).
cleancmd(realclean, all).

clean(intermediate) :- clean_intermediate.
clean(docs_no_texi) :- clean_docs_no_texi.
clean(all_temporary) :- clean_all_temporary.
clean(all) :- clean_all.

% ===========================================================================

