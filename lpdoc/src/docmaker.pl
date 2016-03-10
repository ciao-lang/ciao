:- module(docmaker, [], [assertions, regtypes, dcg, basicmodes, fsyntax]).

:- doc(title, "Driver for Documentation Generation").
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

:- use_package(fsmemo). % (for 'fsmemo.*') 

% ===========================================================================
:- doc(section, "Commands on Documentation").

:- use_module(library(port_reify), [once_port_reify/2, port_call/1]).
:- use_module(library(system), [working_directory/2, cd/1]).

% TODO: document Opts better, change format (e.g., N(V) instead of name_value(N,V))
:- export(doc_cmd/4).
:- pred doc_cmd(ConfigFile, Opts, Cmd, OutputDir) # "Process
   @var{Targets} given @var{ConfigFile}, producing all output in
   @var{OutputDir}. If @var{OutputDir} is free, directory name of
   @var{ConfigFile} is used (similar to @pred{make_exec/2}).

   @var{Opts} is the list of options (see @pred{autodoc_option/1}).".

doc_cmd(ConfigFile, Opts, Cmd, OutputDir) :-
	% Setup configuration
	load_settings(ConfigFile, Opts),
	% Call and cleanup
	working_directory(WD, WD),
	( var(OutputDir) ->
	    OutputDir = ~path_dirname(~settings_file)
	; true
	),
	cd(OutputDir),
	once_port_reify(doc_cmd_(Cmd), Port),
	cd(WD), % move to original directory
	% TODO: cleanup all databases here too
	port_call(Port).

doc_cmd_(start(Targets)) :-
	setup_doc_settings,
	process_targets(Targets).
doc_cmd_(view(Suffix)) :-
	setup_doc_settings,
	view(Suffix).
doc_cmd_(clean(Mode)) :-
	setup_doc_settings,
	clean(Mode).

setup_doc_settings :-
	verify_settings,
	clean_fs_db,
	clean_image_cache,
	reset_output_dir_db,
	load_vpaths,
	parse_structure.

% standalone target bases (does not depend on a settings file)
:- data no_settings_file/0.

% TODO: do all targets
% TODO: do something like auto-settings (or standalone)? recognize SETTINGS.pl file automatically?

process_targets(Targets) :-
	process_targets_(Targets).

process_targets_([]) :- !.
process_targets_(['-c', Target|Targets]) :- !,
	process_standalone(Target),
	process_targets_(Targets).
process_targets_([Cmd|Targets]) :- !,
	process_cmd(Cmd),
	process_targets_(Targets).

process_cmd(Cmd) :-
	report_cmd('Starting', Cmd),
	cmd_actions(Cmd, Actions),
	fsmemo_call(Actions),
	report_cmd('Finished', Cmd).

% TODO: Replace Target by a Spec!
% TODO: Make it much simpler! Integrate with single module manual! (M-x ciao-gen-buffer-doc)
%% Treat Target as a separated component
process_standalone(Target) :-
	base_from_target(Target, Base),
	retractall_fact(no_settings_file),
	assertz_fact(no_settings_file),
	report_cmd('Starting', Base),
	standalone_docstr(Base),
	( target_action(Target, Action),
	  fsmemo_call([Action]) ->
	    Ok = yes 
	; Ok = no
	),
	clean_docstr,
	retractall_fact(no_settings_file),
	Ok = yes,
	report_cmd('Finished', Base).

% Obtain the name of a target (by removing the suffix, which must be a
% supported one)
base_from_target(Target) := Base :-
	( supported_file_format(Suffix),
	  atom_concat([Base, '.', Suffix], Target) ->
	    true
	; Base = Target
	).

report_cmd(BegEnd, Ext) :-
	file_format_name(Ext, FormatName),
	!,
	simple_message("~w manual generation in ~w (~w) format.",
	    [BegEnd, Ext, FormatName]).
report_cmd(BegEnd, Base) :-
	simple_message("~w processing of '~w'.", [BegEnd, Base]).

% Obtain the action Action that generates some target (file) F
target_action(F,Action) :- % TODO: Disable
	Spec = ~all_specs,
	path_basename(Spec,Name),
	Backend = ~backend_id,
	absfile_for_subtarget(Name,Backend,dr,F),
        !,
	Action = gen_doctree(Backend, Spec).
target_action(F,Action) :- % TODO: Disable
	Backend = ~backend_id,
	get_mainmod(Mod),
        absfile_for_subtarget(Mod,Backend,gr,F),
        !,
	Action = compute_grefs(Backend).
target_action(F,Action) :- % TODO: Disable
	Spec = ~all_specs,
	path_basename(Spec,Name),
	Backend = ~backend_id,
	absfile_for_subtarget(Name,Backend,cr,F),
        !,
	Action = translate_doctree(Backend,Spec).
target_action(F,Action) :- % TODO: Disable
	Backend = ~backend_id,
	get_mainmod(Mod),
	absfile_for_subtarget(Mod,Backend,fr,F),
        !,
        Action = autodoc_finish(Backend).
target_action(F,Action) :- % TODO: Disable
	Backend = ~backend_id,
	Alt = ~backend_alt_format(Backend),
	file_format_provided_by_backend(Alt, Backend, Subtarget),
	get_mainmod(Mod),
	absfile_for_subtarget(Mod, Backend, Subtarget, F),
        !,
	Action = autodoc_gen_alternative(Backend,Alt).

all_specs := B :-
	( B = ~get_mainmod_spec
	; Bs = ~all_component_specs,
	  member(B, Bs)
	).

% Note: do not confuse a file format (e.g. PDF) with a backend (texinfo)

% Actions to generate documentation in some specific format
cmd_actions(all, Actions) :- !,
        findall(Action,
	        (requested_file_formats(Formats),
		 member(Format, Formats),
		 action_for_suffix(Format, Action)),
		Actions).
cmd_actions(Suffix, Actions) :-
	supported_file_format(Suffix),
        !,
	action_for_suffix(Suffix, Action),
	Actions = [Action].

% Action that generate one file format (not necessarily requested in SETTINGS.pl)
action_for_suffix(Suffix, Action) :-
	top_suffix(Suffix,PrincipalExt),
	file_format_provided_by_backend(PrincipalExt, Backend, Subtarget),
	( Subtarget = fr -> Action = autodoc_finish(Backend)
	; Subtarget = fr_alt(Suffix2) -> Action = autodoc_gen_alternative(Backend,Suffix2)
	; Subtarget = cr -> get_mainmod_spec(Spec), Action = translate_doctree(Backend,Spec)
	; throw(unknown_subtarget(Subtarget))
	).

% ===========================================================================
:- doc(section, "Documentation Generation").

:- use_module(library(pathnames), [path_basename/2]).

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

% ---------------------------------------------------------------------------
% 1) Doctree and references from each source

'fsmemo.key'(gen_doctree(Backend,Spec), Target) :- !,
	path_basename(Spec,Name),
	absfile_for_subtarget(Name, Backend, dr, Target).
'fsmemo.deps'(gen_doctree(_Backend, Spec),Deps) :- !,
	query_source(Spec,_SourceSuffix,AbsFile),
	add_settings_dep(AbsFile,Deps).
'fsmemo.run'(gen_doctree(Backend, Spec)) :- !,
        gen_doctree(Backend, Spec).

gen_doctree(Backend, FileBase) :-
	query_source(FileBase, SourceSuffix, _AbsFile), % TODO: only for SourceSuffix; simplify?
	ensure_cache_dir(Backend),
	path_basename(FileBase, Name),
%	display(user_error, generating_doctree(Backend, Name)), nl(user_error),
	get_autodoc_opts(Backend, Name, Opts),
	autodoc_gen_doctree(Backend, FileBase, SourceSuffix, Opts, Name).

query_source(Spec, S) := Path :-
	find_source(Spec, S, _, Path0),
	!,
	Path = Path0.
query_source(Spec, _S) := _Path :-
	throw(autodoc_error("Source file not found: ~w", [Spec])).

% TODO: Missing dependencies to included files, etc. We need c_itf for this.
add_settings_dep(SpecF) := ['SOURCE'(SpecF)|Fs] :-
	( no_settings_file ->
	    Fs = []
	; Fs = ['SOURCE'(~settings_absfile)]
	).

settings_absfile(F) :-
	F0 = ~fixed_absolute_file_name(~settings_file),
	( atom_concat(_, '.pl', F0) ->
	    F = F0
	; atom_concat(F0, '.pl', F)
	).

% ---------------------------------------------------------------------------
% 2) Globally resolved references

'fsmemo.key'(compute_grefs(Backend), Target) :- !,
	get_mainmod(Mod),
        absfile_for_subtarget(Mod, Backend, gr, Target).
'fsmemo.deps'(compute_grefs(Backend),Deps) :- !,
	get_mainmod_spec(Spec),
	components_target(Backend,dr,FdrComps),
        Deps = [gen_doctree(Backend,Spec)|FdrComps].
'fsmemo.run'(compute_grefs(Backend)) :- !,
        compute_grefs(Backend).

compute_grefs(Backend) :-
	Mod = ~get_mainmod,
	get_autodoc_opts(Backend, Mod, Opts),
%	display(user_error, computing_grefs(Backend, Mod)), nl(user_error),
	autodoc_compute_grefs(Backend, Mod, Opts).

% ---------------------------------------------------------------------------
% 3) Backend-specific temporary result

'fsmemo.key'(translate_doctree(Backend,Spec), Target) :- !,
	path_basename(Spec,Name),
	absfile_for_subtarget(Name, Backend, cr, Target).
'fsmemo.deps'(translate_doctree(Backend,Spec),Deps) :- !,
        Deps = [gen_doctree(Backend,Spec),compute_grefs(Backend)].
'fsmemo.run'(translate_doctree(Backend,Spec)) :- !,
        translate_doctree(Backend,Spec).

translate_doctree(Backend, FileBase) :-
	path_basename(FileBase, Base),
%	display(user_error, translating_doctree(Backend, Base)), nl(user_error),
	get_autodoc_opts(Backend, Base, Opts),
	ensure_output_dir_prepared(Backend, Opts),
	autodoc_translate_doctree(Backend, Opts, Base).

% ---------------------------------------------------------------------------
% 4) Backend-specific final result

'fsmemo.key'(autodoc_finish(Backend), Target) :- !,
	get_mainmod(Mod),
	absfile_for_subtarget(Mod, Backend, fr, Target).
'fsmemo.deps'(autodoc_finish(Backend),Deps) :- !,
	get_mainmod_spec(Spec),
	components_target(Backend,cr,FcrComps),
        Deps = [translate_doctree(Backend,Spec)|FcrComps].
'fsmemo.run'(autodoc_finish(Backend)) :- !,
        simple_message("Post-processing the document.",[]),
        autodoc_finish(Backend).

% ---------------------------------------------------------------------------
% (extra) Alternative final results (e.g. PDF, PS, etc.)
% [Rules for generating DVI, PS, and PDF from texi]
'fsmemo.key'(autodoc_gen_alternative(Backend,Suffix2), Target) :- !,
	get_mainmod(Mod),
	absfile_for_subtarget(Mod, Backend, fr_alt(Suffix2), Target).
'fsmemo.deps'(autodoc_gen_alternative(Backend,_Alt),Deps) :- !,
        Deps = [autodoc_finish(Backend)].
'fsmemo.run'(autodoc_gen_alternative(Backend,Alt)) :- !,
        simple_message("Generating document in ~w format.",[Alt]),
        autodoc_gen_alternative(Backend,Alt).

% ---------------------------------------------------------------------------

components_target(Backend, _Subtarget, []) :- backend_ignores_components(Backend), !.
components_target(Backend, Subtarget, CompTargets) :-
	CompTargets = ~targets(~all_component_specs, Backend, Subtarget).

targets([],           _, _) := [].
targets([FileBase|FileBases], Backend, Subtarget) := [X|Xs] :-
	( Subtarget = dr -> X = gen_doctree(Backend,FileBase)
	; Subtarget = cr -> X = translate_doctree(Backend,FileBase)
	; fail
	),
	Xs = ~targets(FileBases, Backend, Subtarget).

% ===========================================================================
:- doc(section, "Visualization").

view(Suffix) :-
	file_format_provided_by_backend(Suffix, Backend, Subtarget),
	get_mainmod(Mod),
	absfile_for_subtarget(Mod, Backend, Subtarget, File),
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

% ===========================================================================
:- doc(section, "Cleaning up").
% (after generating manuals)

clean(intermediate) :- clean_intermediate. % leaves targets and .texi
clean(docs_no_texi) :- clean_docs_no_texi. % leaves only .texi
clean(all_temporary) :- clean_all_temporary. % leaves only targets
clean(all) :- clean_all. % leaves nothing

