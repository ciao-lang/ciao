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
	[path_split/3, path_concat/3, path_get_relative/3]).
:- use_module(library(system_extra), [(-) /1]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(make/make_rt), [find_file/2]).

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

:- export(find_file/2).
find_file(RelPath, Path) :- make_rt:find_file(RelPath, Path).

:- export(find_source/4).
% Find the first source that exists (e.g., .pl or .lpdoc)
find_source(Name, Suffix, NameSuffix, Path) :-
	Suffix = ~srcsuff,
	NameSuffix = ~atom_concat([Name, '.', Suffix]),
%	catch(absolute_file_name(library(Main), _), _, fail),
	% TODO: Cannot use absolute_file_name, library_directory is
	%       not updated, only vpath.
	% TODO: Silent fail if source does not exist
	make_rt:find_file(NameSuffix, Path).

% TODO: I am not sure if here is the place to define this.
srcsuff := pl | lpdoc.

% ---------------------------------------------------------------------------

:- export(subtarget/1).
:- regtype subtarget/1 # "The kind of intermediate/final results for a
   single documentation processing unit (module).".

subtarget(fr). % final result
subtarget(fr_aux(_)). % auxiliary files included in the final result
subtarget(fr_alt(_)). % alternative views/versions of the final results (e.g. PDF, PS, infoindex)
subtarget(cr). % intermediate results in the specified format
subtarget(dr). % doctree representation (almost format agnostic)
subtarget(rr). % local references (joint later as global refs)
subtarget(gr). % globally resolved references (including biblio)

% Use the output name for this subtarget?
% TODO: Base on some backend option?
subtarget_uses_output_name(fr, _) :- !.
subtarget_uses_output_name(fr_alt(_), _) :- !.
subtarget_uses_output_name(cr, man) :- !.

% Subtargets that are placed in the output dir (and not in the cache dir)
subtarget_is_final(cr, html) :- !.
subtarget_is_final(cr, man) :- !.
subtarget_is_final(fr_alt(Ext), html) :- !, \+ Ext = 'htmlmeta'.
subtarget_is_final(fr_aux(_), html) :- !.
subtarget_is_final(fr, _) :- !.
subtarget_is_final(fr_alt(_), _) :- !.

:- pred target_suffix/3 : atm * subtarget * atm # "A
   final suffix given a format and subtarget.".
% TODO: put everything in a temporary directory so that suffixes can be mixed?

target_suffix(Backend, Subtarget, Suffix) :-
	( Subtarget = fr -> % final result
	    backend_final_suffix(Backend, Suffix)
	; Subtarget = cr -> % intermediate
	    backend_temp_suffix(Backend, Suffix)
	; Subtarget = dr -> % doctree
	    backend_temp_suffix(Backend, Suffix0),
	    atom_concat(Suffix0, '_dr', Suffix)
	; Subtarget = rr -> % local references
	    backend_temp_suffix(Backend, Suffix0),
	    atom_concat(Suffix0, '_rr', Suffix)
	; Subtarget = gr -> % global references
	    backend_temp_suffix(Backend, Suffix0),
	    atom_concat(Suffix0, '_gr', Suffix)
	).

% TODO: This is the supported format suffix for components
backend_temp_suffix(texinfo,'texic').
backend_temp_suffix(html,'html').
backend_temp_suffix(man,'manl').

backend_final_suffix(texinfo, 'texi').
backend_final_suffix(html, 'htmlmeta').
%backend_final_suffix(man, 'manmeta').

% ---------------------------------------------------------------------------

% TODO: For information purposes, also for cleaning. This could be defined
%   in the backends.
:- export(file_format_name/2).
file_format_name(dvi,   'TeX device-indep').
file_format_name(ps,    'postscript').
file_format_name(pdf,   'Adobe PDF (acrobat)').
file_format_name(texi,  'GNU texinfo source').
%file_format_name('HLP', 'Windows help').
file_format_name(txt,   'plain text').
file_format_name(ascii, 'ASCII plain text').
file_format_name(html,  'HTML hypertext').
file_format_name(htmlmeta,  'HTML hypertext (metafile)').
file_format_name(info,  'GNU info hypertext').
file_format_name(infoindex, 'GNU info hypertext (directory)').
file_format_name(manl,  'Unix man').

:- export(supported_file_format/1).
supported_file_format(Ext) :-
	file_format_provided_by_backend(Ext, _, _).

% TODO: Store in the backends?
:- export(file_format_provided_by_backend/3).
:- pred file_format_provided_by_backend(Ext, Backend, Subtarget) :: atm * backend_id * atm #
   "@var{Backend} is the backend that generates files with format @var{Ext}".
file_format_provided_by_backend(texi, texinfo, fr).
file_format_provided_by_backend(dvi, texinfo, fr_alt(dvi)).
file_format_provided_by_backend(ps, texinfo, fr_alt(ps)).
file_format_provided_by_backend(pdf, texinfo, fr_alt(pdf)).
file_format_provided_by_backend(info, texinfo, fr_alt(info)).
file_format_provided_by_backend(infoindex, texinfo, fr_alt(infoindex)).
file_format_provided_by_backend(ascii, texinfo, fr_alt(ascii)).
file_format_provided_by_backend(manl, man, cr).
file_format_provided_by_backend(html, html, cr).
file_format_provided_by_backend(htmlmeta, html, fr).

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

:- pred get_output_dir(Backend, Dir) # "Obtain the @var{Dir} directory
   where the documentation files are generated. Note that this is not
   the installation directory.".

get_output_dir(Backend, Dir) :-
	computed_output_dir(Backend, Dir0), !, Dir = Dir0.
get_output_dir(Backend, Dir) :-
	Dir1 = '',
	( output_packed_in_dir(Backend) ->
	    % Use a directory inside 'htmldir'
	    main_output_name(Backend, OutBase),
	    atom_concat([Dir1, OutBase, '.', Backend], Dir)
	; % Store in 'htmldir' directly
	  Dir = Dir1
	),
	assertz_fact(computed_output_dir(Backend, Dir)).

:- export(get_cache_dir/2).
:- pred get_cache_dir(Backend, Dir) # "Obtain the @var{Dir} directory
where final documentation files will be stored".

get_cache_dir(Backend, Dir) :-
	computed_cache_dir(Backend, Dir0), !, Dir = Dir0.
get_cache_dir(Backend, Dir) :-
	% TODO: missing some root dir
	main_output_name(Backend, OutBase),
	atom_concat([OutBase, '.tmp-', Backend], Dir),
	assertz_fact(computed_cache_dir(Backend, Dir)).

% ---------------------------------------------------------------------------

% Make sure that the output directory exists
:- export(ensure_output_dir/1).
ensure_output_dir(Backend) :-
	get_output_dir(Backend, Dir),
	( Dir = '' -> true
	; path_concat(Dir, '', Dir2),
	  mkpath(Dir2)
	).

% Make sure that the cache directory exists
:- export(ensure_cache_dir/1).
ensure_cache_dir(Backend) :-
	get_cache_dir(Backend, Dir),
	( Dir = '' -> true 
	; path_concat(Dir, '', Dir2),
	  mkpath(Dir2) 
	).

:- use_module(library(system_extra), [mkpath/1]).

% ---------------------------------------------------------------------------
% Some special cases for @pred{absfile_for_subtarget/4}

:- export(main_absfile_in_format/2).
:- pred main_absfile_in_format(Ext, File) # 
   "@var{File} is the absolute file name for the documentation in
   @var{Ext} format of the @em{main} module".

% TODO: note that Ext is not a backend, but a file format. Made that 
% explicit in the documentation.
main_absfile_in_format(Ext, File) :-
	file_format_provided_by_backend(Ext, Backend, Subtarget),
	main_absfile_for_subtarget(Backend, Subtarget, File).

:- export(main_absfile_for_subtarget/3).
main_absfile_for_subtarget(Backend, Subtarget, File) :-
	get_mainmod(Mod),
	absfile_for_subtarget(Mod, Backend, Subtarget, File).

:- export(absfile_for_aux/3).
:- pred absfile_for_aux(AuxName, Backend, AbsFile) # "Absolute file
   for an auxiliary output file (e.g. CSS, images, etc.)".

absfile_for_aux(AuxName, Backend, AbsFile) :-
	absfile_for_subtarget(AuxName, Backend, fr_aux(''), AbsFile).

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
	( Subtarget = fr_aux('') ->
	    NameExt = Base
	; ( Subtarget = fr_aux(Ext) -> true
	  ; Subtarget = fr_alt(Ext) -> true
	  ; target_suffix(Backend, Subtarget, Ext)
	  ),
	  atom_concat([Base, '.', Ext], NameExt)
	).

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
	( \+ setting_value(doc_mainopts, no_versioned_output),
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
	find_source(Mod, _, _, ModPath),
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

:- export(clean_intermediate/0).
clean_intermediate :-
	clean_other_intermediate.

doc_output_pattern(Pattern) :-
	pred_to_glob_pattern(doc_output_pattern_, Pattern).

doc_output_pattern_(Pattern) :-
	file_format_name(Ext, _),
	\+ Ext = 'texi',
	atom_concat('*.', Ext, Pattern).

clean_temp_no_texi :-
	doc_output_pattern(Pattern),
	-remove_glob('.', Pattern).

clean_texi :-
	-delete_glob('.', '*.texi').

clean_other_intermediate :-
	% TODO: It should not delete *_autofig.png files, right? (indeed they are not generated here)
	% TODO: Use a directory for temporary files?
	pred_to_glob_pattern(other_pattern, Pattern),
	-delete_glob('.', Pattern),
	PatternD = '*.tmp-*', % (see get_cache_dir/2)
	-remove_glob('.', PatternD).

other_pattern('*~').
other_pattern('*.itf').
other_pattern('*.po').
other_pattern('*.asr').
other_pattern('*.testout').
other_pattern('*.infoindex').
other_pattern('*.err').
other_pattern('*.tmp').
other_pattern('*.log').
other_pattern('*.aux').
other_pattern('*.blg').
other_pattern('*.bbl').
other_pattern('*_autofig.jpg').
other_pattern('*_autofig.png').
other_pattern('*_autofig.eps').
other_pattern('*.htmlmeta').

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
output_packed_in_dir(Backend) :-
	% Only HTML, when not generating a website
	Backend = html,
	\+ setting_value(html_layout, 'website_layout').

