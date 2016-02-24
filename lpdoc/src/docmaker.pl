:- module(docmaker, [make_doc/4], [assertions, regtypes, dcg, basicmodes, make, fsyntax]).

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

%% ---------------------------------------------------------------------------
% target_comment(all,       "Generate manual in all default formats", []).
%% ---------------------------------------------------------------------------

% Note: do not confuse a file format (e.g. PDF) with a backend (texinfo)

% Generate all the requested file formats (in SETTINGS.pl)
all <- ~requested_file_formats
# "Generates all documentation files specified by @pred{docformat/1}." :- true.

% Generate one file format (not necessarily requested in SETTINGS.pl)
~supported_file_format <- ~main_absfile_in_format(~top_suffix(Suffix)) :: Suffix :-
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
~absfile_for_subtarget(~path_basename(~dupspec(Spec)), ~dupft(F), dr) <-
	    ~add_settings_dep(~query_source(Spec, S), Spec) :-
	gen_doctree(F, Spec, S).

% 2) Globally resolved references
~main_absfile_for_subtarget(~dupft(F), gr) <-
	    [~main_absfile_for_subtarget(F, dr)|~components_target(F, dr)] :-
	compute_grefs(F).

% 3) Backend-specific temporary result
~absfile_for_subtarget(~path_basename(~dupspec(Spec)), ~dupft(F), cr) <-
	    [~absfile_for_subtarget(~path_basename(Spec), F, dr),
	     ~main_absfile_for_subtarget(F, gr)] :-
	% note: Base here is a modname (not a modspec)
	translate_doctree(F, ~path_basename(Spec)).

% 4) Backend-specific final result
~main_absfile_for_subtarget(~dupft(F), fr) <-
	    [~main_absfile_for_subtarget(F, cr)|~components_target(F, cr)] :-
	simple_message("Post-processing the document.", []),
	autodoc_finish(F).

% (extra) Alternative final results (e.g. PDF, PS, etc.)
% [Rules for generating DVI, PS, and PDF from texi]
~main_absfile_in_format(~dup_alt_format(Alt, F)) <-
	    [~main_absfile_for_subtarget(F, fr)] :-
	simple_message("Generating document in ~w format.", [Alt]),
	autodoc_gen_alternative(F, Alt).

% @var{Alt} is an alternative format for backend @var{F}
dup_alt_format(Alt, F) := ~dup(~backend_alt_format(~dupft(F)), Alt).

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

% Unify arguments and return them. This is useful to define generic
% rules that work for several formats and source suffixes.
% TODO: This trick is necessary because of how the fsyntax package
%       currently interacts with the lpmake package. This should
%       be reviewed carefully. --JF
dup(X,X) := X.

dupft(F) := ~dup(~backend_id,F).

query_source(Spec, S) := Main :-
	find_source(Spec, S, Main, _),
	!.
query_source(Spec, _S) := _Main :-
	throw(make_error("Source file not found: ~w", [Spec])).

dupspec(Spec) := ~dup(~all_specs, Spec).

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

% Default 
view <- [htmlview] # "Visualize default format (.html)" :- true.

% Main viewers
pdfview <- [] # "Visualize .pdf (with a default viewer)" :-
	view('pdf').
psview <- [] # "Visualize .ps (with a default viewer)" :-
	view('ps').
htmlview <- [] # "Visualize .html (with a default viewer)" :-
	view('html').
infoview <- [] # "Visualize .info (with a default viewer)" :-
	view('info').
manlview <- [] # "Visualize .manl (with a default viewer)" :-
	view('manl').

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

clean <- [] # "Delete intermediate files. Leaves .texi & docs" :-
	clean_intermediate.

docsclean <- [] # "Delete all generated document files (temporary and final). Leaves only .texi" :-
	clean_docs_no_texi.

%% distclean <- [clean] # "Leaves only generated docs." :- 
distclean <- [] # "Delete all temporary files, including .texi" :-
	clean_all_temporary.

% :- use_module(library(source_tree), [delete_glob/2]).
%% realclean <- [docsclean] # "Deletes everything." :-
%% 	-delete_glob('.', '*.texic').
realclean <- [] # "Deletes everything." :-
	clean_all.

% ===========================================================================

