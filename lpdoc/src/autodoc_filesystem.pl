:- module(autodoc_filesystem, [], [dcg, assertions, regtypes, basicmodes, fsyntax, hiord]).

:- doc(title, "Filesystem Abstraction").

:- doc(author, "Jose F. Morales").

:- doc(module, "This module provides definitions to assign unique
   file-system paths and names for each of the intermediate and final
   results of documentation generation.").

:- use_module(lpdoc(autodoc_settings)).
:- use_module(lpdoc(autodoc_structure)).
:- use_module(lpdoc(autodoc_state), [backend_id/1]).

:- use_module(library(aggregates)).
:- use_module(library(pathnames),
	[path_split/3, path_concat/3, path_get_relative/3,
	 path_is_absolute/1]).
:- use_module(library(system), [file_exists/1]).
:- use_module(library(system_extra), [(-) /1]).
:- use_module(library(terms), [atom_concat/2]).

%% ---------------------------------------------------------------------------

% TODO: Should really use sourcename, but not really supported until we eliminate 
%       the makefile completely. (Check again this to-do -- JFMC)

:- export(filename/1).
:- regtype filename(X) # "@var{X} is the name of a file.".

filename(X) :- atm(X).

% TODO: Check again; we sometimes use filename_noext as module names
:- export(filename_noext/1).
:- regtype filename_noext(X) # "@var{X} is the base name of a file (without extension).".

filename_noext(X) :- atm(X).

% ---------------------------------------------------------------------------

% Search path for files
:- data vpath/1.

:- export(cleanup_vpath/0).
cleanup_vpath :-
	retractall_fact(vpath(_)).

:- export(add_vpath/1).
add_vpath(Path) :-
	( data_facts:current_fact(vpath(Path)) ->
	    true
	; data_facts:assertz_fact(vpath(Path))
	).

:- export(find_file/2).
% Find file in any of vpath/1
% (Fail if not found)
find_file(File, PathFile) :-
	path_is_absolute(File), !,
	PathFile = File,
	file_exists(PathFile).
find_file(File, PathFile) :-
	vpath(Path),
	path_concat(Path, File, PathFile),
	file_exists(PathFile),
	!.

:- export(find_doc_source/2).
% Find the first source that exists (e.g., .pl or .lpdoc). See @pred{find_file/2}
find_doc_source(Name, Path) :-
	Ext = ~srcext,
	NameExt = ~atom_concat(Name, Ext),
	find_file(NameExt, Path).

% TODO: I am not sure if here is the place to define this.
srcext := '.pl' | '.lpdoc'.

% ---------------------------------------------------------------------------

:- export(subtarget/1).
:- regtype subtarget/1 # "The kind of intermediate/final results for a
   single documentation processing unit (module).".

subtarget(fr). % final result
subtarget(fr_alt(_Alt)). % alternative format for the final results (e.g. PDF, PS, infoindex)
subtarget(cr). % intermediate results in the specified format
subtarget(dr). % doctree representation (almost format agnostic)
subtarget(rr). % local references (joint later as global refs)
subtarget(gr). % globally resolved references (including biblio)

% Use the output name for this subtarget?
% TODO: Base on some backend option?
subtarget_uses_output_name(fr, _) :- !.
subtarget_uses_output_name(fr_alt(_Alt), _) :- !.
subtarget_uses_output_name(cr, man) :- !.

% Subtargets that are placed in the output dir (and not in the cache dir)
subtarget_is_final(cr, html) :- !.
subtarget_is_final(cr, man) :- !.
subtarget_is_final(fr_alt(Alt), html) :- !, \+ Alt = 'htmlmeta'.
subtarget_is_final(fr_alt(_Alt), _) :- !.
subtarget_is_final(fr, _) :- !.

% Aux placed in the output dir (and not in the cache dir)
aux_is_final(html) :- !.

:- pred target_ext/3 : atm * subtarget * atm 
   # "File extension for a format and subtarget.".
% TODO: put everything in a temporary directory so that suffixes can be mixed?

target_ext(Backend, Subtarget, Ext) :-
	( Subtarget = fr -> % final result
	    backend_final_ext(Backend, Ext)
	; Subtarget = fr_alt(Alt) ->
	    alt_ext(Alt, Ext)
	; Subtarget = cr -> % intermediate
	    backend_comp_ext(Backend, Ext)
	; Subtarget = dr -> % doctree
	    backend_comp_ext(Backend, Ext0),
	    atom_concat(Ext0, '_dr', Ext)
	; Subtarget = rr -> % local references
	    backend_comp_ext(Backend, Ext0),
	    atom_concat(Ext0, '_rr', Ext)
	; Subtarget = gr -> % global references
	    backend_comp_ext(Backend, Ext0),
	    atom_concat(Ext0, '_gr', Ext)
	; fail
	).

% (extension of component output)
backend_comp_ext(texinfo,'.texic').
backend_comp_ext(html,'.html').
backend_comp_ext(man,'.manl').

% (extension of main output)
backend_final_ext(texinfo, '.texi').
backend_final_ext(html, '.htmlmeta').
%backend_final_ext(man, '.manmeta').

alt_ext(dvi, '.dvi').
alt_ext(ps, '.ps').
alt_ext(pdf, '.pdf').
alt_ext(info, '.info').
alt_ext(infoindex, '.infoindex').
alt_ext(ascii, '.ascii').

% ---------------------------------------------------------------------------

% TODO: For information purposes, also for cleaning. This could be defined
%   in the backends.
:- export(file_format_name/2).
file_format_name(dvi,   'TeX device-indep').
file_format_name(ps,    'postscript').
file_format_name(pdf,   'Adobe PDF (acrobat)').
file_format_name(texi,  'GNU texinfo source').
%file_format_name('HLP', 'Windows help').
file_format_name(txt,   'plain text'). % TODO: used?
file_format_name(ascii, 'ASCII plain text').
file_format_name(html,  'HTML hypertext').
file_format_name(htmlmeta,  'HTML hypertext (metafile)').
file_format_name(info,  'GNU info hypertext').
file_format_name(infoindex, 'GNU info hypertext (directory)').
file_format_name(manl,  'Unix man').

:- export(supported_file_format/1).
supported_file_format(texi).
supported_file_format(dvi).
supported_file_format(ps).
supported_file_format(pdf).
supported_file_format(info).
supported_file_format(infoindex).
supported_file_format(ascii).
supported_file_format(manl).
supported_file_format(html).
supported_file_format(htmlmeta).

% TODO: Store in the backends?
:- export(format_get_subtarget/3).
:- pred format_get_subtarget(Format, Backend, Subtarget) :: atm * backend_id * atm #
   "@var{Backend} and @var{Subtarget} are the backend and subtarget that generates files with format @var{Format}".
format_get_subtarget(texi, texinfo, fr).
format_get_subtarget(dvi, texinfo, fr_alt(dvi)).
format_get_subtarget(ps, texinfo, fr_alt(ps)).
format_get_subtarget(pdf, texinfo, fr_alt(pdf)).
format_get_subtarget(info, texinfo, fr_alt(info)).
format_get_subtarget(infoindex, texinfo, fr_alt(infoindex)).
format_get_subtarget(ascii, texinfo, fr_alt(ascii)).
format_get_subtarget(manl, man, cr).
format_get_subtarget(html, html, cr).
format_get_subtarget(htmlmeta, html, fr).

:- export(format_get_file/3).
:- pred format_get_file(Format, Mod, File) :: atm * atm * atm #
   "@var{File} is the principal file for @var{Mod} in format @var{Format}".
format_get_file(Format, Mod, File) :-
	format_get_subtarget(Format, Backend, Subtarget),
	absfile_for_subtarget(Mod, Backend, Subtarget, File).

% ---------------------------------------------------------------------------

:- data computed_output_name/2. % name of main output
:- data computed_output_dir/2. % directory for output
:- data computed_cache_dir/2. % directory cached temporary results

:- export(clean_fs_db/0).
:- pred clean_fs_db # "Clean the cached information for the
   filesystem mapping of the documentaton generation.".

clean_fs_db :-
	retractall_fact(computed_output_name(_, _)),
	retractall_fact(computed_output_dir(_, _)),
	retractall_fact(computed_cache_dir(_, _)).

:- export(get_output_dir/2).
:- pred get_output_dir(Backend, Dir) # "@var{Dir} is the directory
   where the documentation files are generated. Note that this is not
   the installation directory.".

get_output_dir(Backend, Dir) :-
	computed_output_dir(Backend, Dir0), !, Dir = Dir0.
get_output_dir(Backend, Dir) :-
	Dir1 = '',
	( output_packed_in_dir(Backend, DirSuffix) ->
	    % Use a directory inside 'htmldir'
	    main_output_name(Backend, OutBase),
	    atom_concat(OutBase, DirSuffix, OutDir),
	    path_concat(Dir1, OutDir, Dir)
	; % Store in 'htmldir' directly
	  Dir = Dir1
	),
	assertz_fact(computed_output_dir(Backend, Dir)).

:- export(get_cache_dir/2).
:- pred get_cache_dir(Backend, Dir) # "@var{Dir} is the directory
   where temporary documentation files will be stored (for the active
   backend)".
% Note: this is <main>.cachedoc/<backend>/

get_cache_dir(Backend, Dir) :-
	computed_cache_dir(Backend, Dir0), !, Dir = Dir0.
get_cache_dir(Backend, Dir) :-
	get_cache_dir0(Backend, CacheDir),
	path_concat(CacheDir, Backend, Dir),
	assertz_fact(computed_cache_dir(Backend, Dir)).

:- export(get_cache_dir0/2).
:- pred get_cache_dir0(Backend, Dir) # "@var{Dir} is the directory
   where temporary documentation files will be stored (for all
   backends)".

get_cache_dir0(Backend, CacheDir) :-
	% TODO: missing some root dir?
	main_output_name(Backend, OutBase),
	atom_concat(OutBase, '.cachedoc', CacheDir).

% ---------------------------------------------------------------------------

% Make sure that the output directory exists
:- export(ensure_output_dir/1).
ensure_output_dir(Backend) :-
	get_output_dir(Backend, Dir),
	ensure_dir(Dir).

% Make sure that the cache directory exists
:- export(ensure_cache_dir/1).
ensure_cache_dir(Backend) :-
	get_cache_dir(Backend, Dir),
	ensure_dir(Dir).

ensure_dir(Dir) :-
	( Dir = '' -> true 
	; path_concat(Dir, '', Dir2),
	  mkpath(Dir2) 
	).

:- use_module(library(system_extra), [mkpath/1]).

% ---------------------------------------------------------------------------
% Some special cases for @pred{absfile_for_subtarget/4}

:- export(absfile_for_aux/3).
:- pred absfile_for_aux(AuxName, Backend, AbsFile) # "Absolute file
   for an auxiliary output file (e.g. CSS, images, etc.)".

absfile_for_aux(Base, Backend, AbsFile) :-
	( aux_is_final(Backend) ->
	    get_output_dir(Backend, Dir)
	; get_cache_dir(Backend, Dir)
	),
	path_concat(Dir, Base, AbsFile). % (note path_concat('',X,X))

% ---------------------------------------------------------------------------

:- export(absfile_for_subtarget/4).
absfile_for_subtarget(Mod, Backend, Subtarget, File) :-
	subtarget_name(Backend, Subtarget, Mod, File0),
	absfile_for_subtarget_(File0, Backend, Subtarget, File).

% Obtain the name (without extension) for the given subtarget
subtarget_name(Backend, Subtarget, Mod, NameExt) :-
	( get_mainmod(Mod),
	  subtarget_uses_output_name(Subtarget, Backend) ->
	    main_output_name(Backend, Base)
	; Base = Mod
	),
	target_ext(Backend, Subtarget, Ext),
	atom_concat(Base, Ext, NameExt).

% The final (absolute) base
% TODO: Output and temporary file handling needs a major rework
% absfile_for_subtarget_(FinalBase, Backend, Subtarget, FinalAbsBase)
absfile_for_subtarget_(Base, Backend, Subtarget, AbsFile) :-
	( subtarget_is_final(Subtarget, Backend) ->
	    get_output_dir(Backend, Dir)
	; get_cache_dir(Backend, Dir)
	),
	path_concat(Dir, Base, AbsFile). % (note path_concat('',X,X))

% ---------------------------------------------------------------------------

:- use_module(library(bundle/paths_extra), [fsR/2]).
:- use_module(library(bundle/bundle_info), [bundle_version_patch/2]).

% Note: I cannot obtain the version from version_maintenance at this
%       point, since main_output_name needs to be calculated before
%       the mainfile is read.
% TODO: Generate documentation symlinks automatically?
% TODO: Reuse this for binaries in bundle installation (this code
%       and the links)

:- export(main_output_name/2).
% The output name of the generated manual. The version number will be
% concatenated if available.
% TODO: Make sure that this behaviour and the lpdoc documentation are
%       consistent.
main_output_name(Backend, NV) :-
	computed_output_name(Backend, NV0), !, NV = NV0.
main_output_name(Backend, NV) :-
	main_output_name_novers(OutputBase1),
	% Include the version (if required)
	( setting_value(doc_mainopts, versioned_output),
	  get_parent_bundle(Bundle),
	  V = ~bundle_version_patch(Bundle),
	  atom_concat([OutputBase1, '-', V], NV0) ->
	    % Use the bundle version for the output name
	    NV = NV0
	; % Do not use the version for the output name
	  NV = OutputBase1
	),
	assertz_fact(computed_output_name(Backend, NV)).

:- export(main_output_name_novers/1).
main_output_name_novers(OutputBase) :-
	( ( setting_value(output_name, OutputBase0) ->
	      OutputBase = OutputBase0
	  ; get_mainmod(InBase),
	    modname_nodoc(InBase, OutputBase)
	  )
	).

:- use_module(library(bundle/paths_extra),
	[reverse_fsRx/2, fsRx_get_bundle_and_basename/3]).

% Extract parent bundle from mainmod (fails if not in a bundle)
:- export(get_parent_bundle/1).
get_parent_bundle(Bundle) :-
	get_mainmod(Mod),
	find_doc_source(Mod, ModPath),
	reverse_fsRx(ModPath, ModSpec),
	fsRx_get_bundle_and_basename(ModSpec, Bundle, _).

% Extract a modspec from an absolute file name
% (or give back the same ModPath if not in a bundle)
:- export(get_modspec/2).
get_modspec(ModPath, ModSpec) :-
	( reverse_fsRx(ModPath, ModSpec0),
	  \+ ModSpec0 = (_/_) ->
	    ModSpec = ModSpec0
	; % TODO: emit warning?
	  ModSpec = ModPath % absolute path (no better way...)
	).

% A module name without the '_doc' suffix (if present)
:- export(modname_nodoc/2).
modname_nodoc(Base0, Base) :-
	( atom_concat(Base1, '_doc', Base0) -> Base = Base1
	; Base = Base0
	).

% A modspec without '_doc' suffix in its name (.../a/a_doc path is
% collapsed as .../a if needed).
:- export(modspec_nodoc/2).
modspec_nodoc(ModSpec0, ModSpec) :-
	ModSpec0 =.. [Alias, Path0],
	!,
	modspec_nodoc_(Path0, Path),
	ModSpec =.. [Alias, Path].
modspec_nodoc(Path0, Path) :-
	modspec_nodoc_(Path0, Path).

modspec_nodoc_(Rel/Name0, Path) :- !,
	modname_nodoc(Name0, Name),
	( ( Rel = Name ; Rel = _/Name ) -> 
	    % Collapse '(.../)a/a' as '(.../)a'
	    % (was '(.../)a/a_doc')
	    Path = Rel
	; Path = Rel/Name
	).
modspec_nodoc_(Name0, Name) :-
	modname_nodoc(Name0, Name).

% ---------------------------------------------------------------------------

% TODO: use library(pathnames), it looks like path_concat
:- export(get_subbase/3).
:- pred get_subbase(Base, Sub, SubBase) =>
	filename_noext * atm * filename_noext
   # "@var{SubBase} is the name for the sub-file (@var{Sub})
     associated with @var{Base}".
% e.g., 'ciaointro' for the introduction section of 'ciao', when we
% want it to be a separate file.

get_subbase(Base, Sub, SubBase) :-
	atom_concat(Base, Sub, SubBase).

% ---------------------------------------------------------------------------

:- export(absfile_to_relfile/3).
:- pred absfile_to_relfile(A, Backend, B) # "Obtain the relative path,
   w.r.t. the output directory, of an absolute file. This is useful,
   e.g., for URLs.".

absfile_to_relfile(A, Backend, B) :-
	get_output_dir(Backend, Dir),
	( path_get_relative(Dir, A, B0) ->
	    B = B0
	; B = A
	).

% ---------------------------------------------------------------------------
% Cleaning final and temporary files

% TODO: merge with library(source_tree) code

:- use_module(library(source_tree),
	[remove_glob/2, delete_glob/2]).
:- use_module(library(system_extra),
	[ (-)/1,
	  del_file_nofail/1
	]).

:- export(clean_all/0).
clean_all :-
	clean_intermediate,
	clean_temp_no_texi,
	clean_texi.

:- export(clean_docs_no_texi/0).
clean_docs_no_texi :-
	clean_intermediate,
	clean_temp_no_texi.

:- export(clean_all_temporary/0).
clean_all_temporary :-
	clean_intermediate,
	clean_texi.

clean_temp_no_texi :-
	doc_output_pattern(Pattern),
	-remove_glob('.', Pattern).

doc_output_pattern(Pattern) :-
	pred_to_glob_pattern(doc_output_pattern_, Pattern).

doc_output_pattern_(Pattern) :-
	( backend_comp_ext(_, Ext)
	; backend_final_ext(_, Ext)
	; alt_ext(_, Ext)
	),
	\+ Ext = '.texi',
	atom_concat('*', Ext, Pattern).

clean_texi :-
	-delete_glob('.', '*.texi').

:- export(clean_intermediate/0). % (clean cachedoc and other compilation caches)
clean_intermediate :-
	% TODO: It should not delete *_autofig.png files, right? (indeed they are not generated here)
	% TODO: Use a directory for temporary files?
	pred_to_glob_pattern(other_pattern, Pattern),
	-delete_glob('.', Pattern),
	PatternD = '*.cachedoc', % (see get_cache_dir0/2)
	-remove_glob('.', PatternD).

% TODO: clean using ciao?
other_pattern('*.itf').
other_pattern('*.po').
other_pattern('*.asr').
other_pattern('*.testout').

:- export(pred_to_glob_pattern/2).
:- meta_predicate pred_to_glob_pattern(pred(1), ?).
pred_to_glob_pattern(Pred, Pattern) :-
	findall(P, Pred(P), Ps),
	list_to_glob_pattern(Ps, Pattern).

:- use_module(library(terms), [atom_concat/2]).

:- doc(bug, "list_to_glob_pattern/2 does not optimize anything due
   regexp implementation").

:- pred list_to_glob_pattern(+List, -Pattern)
   # "Obtain a glob pattern from a list of glob patterns
     (concatenating atoms with @tt{'|'} as separator).".

list_to_glob_pattern([], Pattern) :- !, Pattern = ''.
list_to_glob_pattern(List, Pattern) :-
	findall(P, (member(P0, List), atom_concat('|', P0, P)), Ps),
	atom_concat(Ps, Pattern0),
	atom_concat('|', Pattern, Pattern0).

% ---------------------------------------------------------------------------

% The output (for the given format) is packed in a directory
% TODO: Move as options?
output_packed_in_dir(Backend, DirSuffix) :-
	% Only HTML, when not generating a website
	Backend = html,
	\+ custom_html_layout,
	DirSuffix = '.html'. % TODO: it may be '_files', etc.

