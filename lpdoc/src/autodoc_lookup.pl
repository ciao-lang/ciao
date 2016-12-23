:- module(autodoc_lookup, [], [assertions, dcg, fsyntax]).

:- doc(title, "Lookup manuals").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module provides a predicate to locate the
   generated manuals for modules and bundles.").

:- doc(bug, "Do not include by default. Prune dependencies to builder
   (see TODO notes below)").

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(lists), [append/3]).
:- use_module(library(pathnames)).
:- use_module(library(system)).
:- use_module(library(file_utils), [file_to_string/2]).

% ---------------------------------------------------------------------------

:- doc(section, "Bundle doc specification").
% TODO: merge with builder modules

% TODO: save bundle docs in a 'docreg' to avoid loading manifests?
:- use_module(ciaobld(manifest_compiler), [
    ensure_load_manifest/1,
    manifest_call/2,
    get_bundle_readme/2, 
    bundle_manual_base/2
]).

% TODO: doc_flags has implicit initialization (avoid it)
:- use_module(library(bundle/doc_flags), [docformat/1, docformatdir/2]).
:- use_module(library(bundle/bundle_paths),
	[bundle_path/3, bundle_path/4,
	 reverse_bundle_path/3, ext_find_pl_filename/3]).
:- use_module(engine(internals), ['$bundle_id'/1]).

:- export(get_bundle_readme/2).
get_bundle_readme(Bundle, R) :-
	ensure_load_manifest(Bundle),
	manifest_compiler:get_bundle_readme(Bundle, R).

:- export(get_bundle_readme_source/2).
% Source for bundle README files
get_bundle_readme_source(Bundle, R) :-
	ensure_load_manifest(Bundle),
	manifest_call(Bundle, readme(_OutName, Props)), % (nondet)
	( member(main=SrcPath, Props) -> true
	; fail % ill-formed
	),
	R0 = ~bundle_path(Bundle, SrcPath),
	atom_concat(R0, '.lpdoc', R),
	file_exists(R).

:- export(get_bundle_manual_source/2).
% Source for bundle manual files
get_bundle_manual_source(Bundle, R) :-
	ensure_load_manifest(Bundle),
	manifest_call(Bundle, manual(_, Props)), % (nondet)
	( member(main=Path, Props) -> true
	; fail % ill-formed
	),
	R = ~bundle_path(Bundle, Path),
	file_exists(R).

% TODO: when using a Manifest the 'SETTINGS':output_name/1 is not needed
%   (the same than it is not needed to specify an output name in a .pl file
%    with a main/{0,1})

% ---------------------------------------------------------------------------
:- doc(section, "Lookup generated documentation files").

:- export(html_doc_file/2).
% Documentation (in HTML) for a bundle, module, or package. Note that
% there may be more than one manual entry for the same @var{Spec}.
% (nondet)
html_doc_file(Spec, HtmlFile) :-
	% TODO: do not ignore the whole path
	( Spec = bundle(Bundle) ->
	    Basename = ''
	; ext_find_pl_filename(Spec, '', AbsFile),
	  reverse_bundle_path(AbsFile, Bundle, Rel),
	  path_basename(Rel, Basename)
	),
	% Enumerate candidates for manuals (nondet)
	man_bundle(Bundle, ManBundle),
	% Compose path for module documentation
	bundle_manual_htmldir(ManBundle, HtmlDir),
	( Basename = '' ->
	    html_doc_entry(HtmlDir, Base)
	; Base = Basename
	),
	%
	path_concat(HtmlDir, ~atom_concat(Base, '.html'), HtmlFile),
	% Make sure that it exists (backtrack if not)
	file_exists(HtmlFile).

% `ManBundle` is a bundle that may contain a manual that documents
% modules from `Bundle` (nondet)
% TODO: ad-hoc, use bundle meta info?
man_bundle(Bundle, ManBundle) :-
	% (nondet)
	( Bundle = core ->
	    ( ManBundle = core ; ManBundle = doc )
	; ManBundle = Bundle
	).

% HtmlDir for specified Bundle manual
% TODO: Only one manual per bundle is allowed
% TODO: TargetDir is ignored, does not work for installed docs
bundle_manual_htmldir(Bundle, HtmlDir) :-
	'$bundle_id'(Bundle),
	docformat(DocFormat), DocFormat = 'html',
	docformatdir(DocFormat, _TargetDir), % TODO: TargetDir ignored!
	NoExt = ~bundle_manual_base(Bundle),
	FileName = ~atom_concat([NoExt, '.', DocFormat]),
	DocDir = ~bundle_path(Bundle, builddir, 'doc'),
	HtmlDir = ~path_concat(DocDir, FileName),
	file_exists(HtmlDir).

% Entry point (initial module) for lpdoc html documentation at HtmlDir
html_doc_entry(HtmlDir, Entry) :-
	path_split(HtmlDir, _, Base),
	path_splitext(Base, BaseNoExt, _),
	path_concat(HtmlDir, ~atom_concat(BaseNoExt, '.htmlmeta'), EntryFile),
	file_to_string(EntryFile, EntryContent),
	atom_codes(Entry, EntryContent).

