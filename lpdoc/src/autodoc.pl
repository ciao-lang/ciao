:- module(autodoc, [], [assertions, regtypes, dcg, basicmodes, fsyntax]).

:- doc(title, "Documentation Generation Library").
:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Jose F. Morales").

:- doc(module, "
   @cindex{automatic documentation library}

   This library provides some predicates which generate documentation
   automatically for a given module or application, using the
   declarations and assertions used in the module itself as input (see
   the @lib{assertions} library). By default, only the
   @concept{exported predicates} of the module appear in the
   documentation.  The predicates will be documented in the order in
   which they appear in the @pred{module/1} or @pred{module/2}
   declaration. @cindex{module declaration} 

   @cindex{automatic documentation}

   The idea of this package is on one hand to reuse the information
   present in the assertions and on the other to help ensure that code
   and documentation are kept as coherent as possible.  Hopefully,
   keeping them close together should help in this always difficult
   task.  The resulting documentation is somewhat rigidly structured,
   but generally sufficient for a @em{reference} manual, provided a
   little effort is put into the assertions and comments. The end
   product understandably depends heavily on how much work is put into
   adding additional comments to the source. Some documentation will
   be generated in any case, but it is recommended that, at the
   minimum, a module title and a comment for each of the exported
   predicates be provided.

   The output format @cindex{documentation format} in which the
   documentation is generated is defined by the backend modules
   (@lib{autodoc_texinfo}, @lib{autodoc_html}, @lib{autodoc_man},
   etc.).

   One of the main output format supported is @tt{texinfo} (see The
   GNU Texinfo Documentation System manual for more info), from which
   printed manuals and several other printing and on-line formats can
   be easily generated automatically (including info). There is also
   some limited support for direct output in unix @tt{man} format. For
   texinfo, the documentation for a module is a texinfo chapter,
   suitable for inclusion in a wrapper ``main'' document file.

   A simple example of the use of this library a reference manual is
   included with the library source code. Other examples can be found
   in the Ciao documentation directory (i.e., the Ciao manuals
   themselves).  ").

% ---------------------------------------------------------------------------

%% Order below is still important (at least in current Ciao version):

% TODO: Refine
:- use_module(library(format), [format_to_string/3]).
:- use_module(library(ttyout)).
:- use_module(library(aggregates)).
:- use_module(library(read),          [read/2]).
:- use_module(library(dict)).

% Ciao libraries
:- use_module(library(compiler), [use_module/1]).
:- use_module(library(assertions/assrt_lib),
	    [
		cleanup_code_and_related_assertions/0,
		clause_read/7,
		assertion_read/9,
		assertion_body/7,
		get_code_and_related_assertions_opts/6,
		use_pkg/2
	    ]).
:- use_module(library(compiler/c_itf)).
:- use_module(library(messages)).
:- use_module(library(pathnames),
	[path_basename/2, path_splitext/3, path_concat/3]).
:- use_module(library(lists),
	[append/3, reverse/2, length/2, list_concat/2, select/3]).
:- use_module(library(terms), [atom_concat/2]).

:- use_module(library(system_extra), [(-) /1, try_finally/3]).

% ---------------------------------------------------------------------------

% TODO: rename predicates in assertions_props
:- use_module(library(assertions/assertions_props),
	    [predfunctor/1, propfunctor/1]).

% @var{X} is a definition for predicates
defkind_pred(X) :- predfunctor(X).
% @var{X} is a definition for properties
defkind_prop(X) :- propfunctor(X).

% ---------------------------------------------------------------------------

% Local libraries
:- use_module(lpdoc(autodoc_state)).
:- use_module(lpdoc(autodoc_settings)).
:- use_module(lpdoc(autodoc_filesystem)).
:- use_module(lpdoc(autodoc_structure)).
:- use_module(lpdoc(autodoc_doctree)).
:- use_module(lpdoc(autodoc_refsdb)).
:- use_module(lpdoc(autodoc_parse)).
:- use_module(lpdoc(autodoc_index)).
:- use_module(lpdoc(autodoc_aux), [verbose_message/1, verbose_message/2]).
:- use_module(lpdoc(comments), [version_descriptor/1, docstring/1,
	stringcommand/1, doc_id_type/3]).

% ===========================================================================

:- export(index_comment/2).
:- pred index_comment(Index,Text) 

	=> atom * string

        # "@var{Type} is a type of index which is
          supported. @var{Text} describes the index contents.".

index_comment(Type,Text) :-
	typeindex(Type,_,_,Text,_).

:- use_module(lpdoc(autodoc_doctree)).

% ---------------------------------------------------------------------------

:- doc(section, "Output Directory Preparation").
% Make sure that the output directory for this target has been prepared
% TODO: move customization or options to each backend?

:- data output_dir_prepared/1.

:- export(reset_output_dir_db/0).
reset_output_dir_db :-
	retractall_fact(output_dir_prepared(_)).

:- export(ensure_output_dir_prepared/2).
:- pred ensure_output_dir_prepared(Backend, Opts) # "Ensure that the
   output directories for backend @var{Backend} are prepared.".
% Prepare the output directory (computes the output directory, makes
% sure it exists, and calls @pred{prepare_auxfiles/2}).
ensure_output_dir_prepared(Backend, _) :-
	current_fact(output_dir_prepared(Backend)),
	!.
ensure_output_dir_prepared(Backend, Opts) :-
	assertz_fact(output_dir_prepared(Backend)),
	% Make sure that the output directory exists
	ensure_output_dir(Backend),
	%
	prepare_auxfiles(Backend, Opts).

% Copy the auxiliary files required for this output. This is specific
% to each backend (e.g. CSS, web-site skeleton, images, etc.)
%
prepare_auxfiles(Backend, Opts) :- Backend = html, !,
	( setting_value(website_skeleton, SkelDir) ->
	    prepare_web_skel(SkelDir)
	; true
	),
	( member(no_math, Opts) ->
	    true
	; prepare_mathjax
	),
	% Add CSS files
	CSS = 'lpdoc.css',
	setting_value(lpdoclib, LpdocLib),
	path_concat(LpdocLib, CSS, HtmlStyle),
	absfile_for_aux(CSS, Backend, OutCSS),
	copy_file(HtmlStyle, OutCSS, [overwrite]).
prepare_auxfiles(_, _) :- !.

:- use_module(library(system), [copy_file/3]).

:- use_module(lpdoc(autodoc_html_assets),
	[prepare_web_skel/1, prepare_mathjax/0]).

% ===========================================================================

% TODO: Add type to Opts (indices, lib-opts and paper-opts are missing!)
:- export(get_autodoc_opts/3).
:- pred get_autodoc_opts(Backend, Mod, Opts) : atm * atm * list(supported_option) #
   "Get the list of documentation options @var{Opts} for the
   @var{FileBase} file.".

get_autodoc_opts(_Backend, Mod, Opts) :-
	StartPage = ~setting_value_or_default(startpage),
	PaperType = ~setting_value_or_default(papertype),
	get_lib_opts(Libs, _SysLibs),
	Indices = ~all_setting_values(index),
	%
	( Mod = ~get_mainmod ->
	    Opts0 = ~all_setting_values(doc_mainopts),
	    % TODO: Should this be here?
	    % Complain about not defined values (only when documenting mainmod)
	    warn_if_empty(Libs, "no filepath variable was found"),
	    warn_if_empty(Indices, "no index variable was found")
	; Opts0 = ~all_setting_values(doc_compopts)
	),
	Opts = [indices(Indices),
	        paper_opts(StartPage, PaperType)|Opts0].

warn_if_empty(X, Msg) :-
	( X == [] ->
	    verbose_message("~s", [Msg])
	; true
	).

% ===========================================================================

:- doc(section, "Generate and Save the doctree for a Module").

:- export(autodoc_gen_doctree/5).
:- pred autodoc_gen_doctree(Backend, FileBase, SourceSuffix, Opts, Mod)
	:: backend_id * filename_noext * atm * list(supported_option) * atm

# "@var{FileBase} is the module specifier of the source file being
   documented (without extension, @var{SourceSuffix} is the suffix of
   the source). The output is a file whose contents document the main
   file, based on any assertions present in that file.  The
   documentation is produced in the format given by @var{Backend} (the
   name of the output file also depends on @var{Backend}). The formats
   supported are given by @pred{backend_id/1}. @cindex{supported
   documentation formats}".

% indices(Indices)
% paper_opts(StartPage, PaperType)
%
%      @var{Indices} is a list of index names (the @concept{indices
%      generated automatically}). @var{StartPage} is the page number
%      of the first page of the manual. This can be useful if the
%      manual is to be included in a larger document or set of
%      manuals.

% Note on performance:
%   For the average module, doctree scan and write is fast. The
%   slowest part is reading the source file.
%
% TODO: Profile the source reader; time may be wasted looking at
%       dependencies, whose cached information is possibly discarded
%       from one execution to the other. --JF

autodoc_gen_doctree(Backend, FileBase, SourceSuffix, Opts, Mod) :-
	verbose_message("{Generating ~w documentation for ~w", [Backend, FileBase]),
	docst_new_with_src(Backend, FileBase, SourceSuffix, Opts, DocSt),
	%
	docst_modtype(DocSt, ModuleType),
	docst_message("File being documented as '~w'", [ModuleType], DocSt),
	get_last_version(Version, GlobalVers, DocSt),
	fmt_module(DocSt, Version, GlobalVers, ModuleR),
	% Register document info
	register_main_logo(DocSt),
	register_main_title(Version, DocSt),
	register_main_version(GlobalVers, DocSt),
	%
	doctree_scan_and_save(ModuleR, Mod, DocSt),
	% TODO: This generates the infoindex if necessary; generalize for other formats
	( %SourceSuffix = pl,
	  docst_currmod_is_main(DocSt),
	  Backend = texinfo ->
	    fmt_infodir_entry(DocSt, GlobalVers, Mod)
	; true
	),
	%
	verbose_message("}"),
	ttyflush,
	!.
autodoc_gen_doctree(_, FileBase, _, _, _) :-
	error_message("formatting ~w could not be completed", [FileBase]).

doctree_scan_and_save(R, Mod, DocSt) :-
	% Scan the references, save them, and save the doctree.
	doctree_scan_and_save_refs(R, DocSt),
	docst_backend(DocSt, Backend),
	absfile_for_subtarget(Mod, Backend, dr, RFile),
	doctree_save(RFile, R).

register_main_logo(DocSt) :-
	( docst_currmod_is_main(DocSt),
	  get_doc(logo, dofail, DocSt, Logo) ->
	    docst_mdata_assertz(main_logo(Logo), DocSt)
	; true
	).

register_main_version(GlobalVers, DocSt) :-
	( docst_currmod_is_main(DocSt),
	  is_version(GlobalVers) ->
	    docst_mdata_assertz(main_globalvers(GlobalVers), DocSt)
	; true
	).

register_main_title(Version, DocSt) :-
	( docst_currmod_is_main(DocSt) ->
	    compose_main_title(Version, DocSt, MainTitleR),
	    docst_mdata_assertz(main_title(MainTitleR), DocSt)
	; true
	).

% This is the title of the manual, including version number (e.g., for
% the web page window title)
compose_main_title(Version, DocSt, MainTitleR) :-
        get_doc(title, ignore, DocSt, TitleR),
	MainTitleR = [TitleR|MainTitleR0],
	( version_numstr(Version, VerStr) ->
	    MainTitleR0 = [string_esc(" v"), string_esc(VerStr)]
	; MainTitleR0 = []
	).

% ---------------------------------------------------------------------------

:- doc(subsection, "Format Authors").

get_authors(DocSt, AuthorRs) :-
	( docst_opt(no_authors, DocSt) ->
	    AuthorRs=[]
	; get_doc(author, warning, DocSt, AuthorRs0),
	  ( docst_currmod_is_main(DocSt) ->
	      % Do not add defauthor in this case
	      % (this is the cover!)
	      AuthorRs1 = AuthorRs0
	  ; add_author_defs(AuthorRs0, DocSt, AuthorRs1)
	  ),
	  % TODO: good idea?
	  get_doc(credits, ignore, DocSt, CreditsRs),
	  append(AuthorRs1, CreditsRs, AuthorRs)
	).

add_author_defs([], _DocSt, []).
add_author_defs([A|As], DocSt, [B|Bs]) :-
	( doctree_to_rawtext(A, DocSt, A1),
	  remove_author_details(A1, Name0) ->
	    % TODO: special commands (like accents) are lost here!
	    Name = raw(Name0)
	; % do nothing, no extra info
	  Name = A
	),
	B = defauthor(local_label(_), Name, A),
	add_author_defs(As, DocSt, Bs).

% ---------------------------------------------------------------------------

:- use_module(lpdoc(autodoc_texinfo), [infodir_base/2]).

:- doc(subsection, "Format infoindex").

:- export(fmt_infodir_entry/3).
% TODO: Generalize for other backends, not just info
:- pred fmt_infodir_entry(DocSt, Version, Mod)
	: docstate * term * filename_noext
# "Generates a one line description (ASCII) of the application or library
   in a file for the directory of @tt{emacs info} manuals.".

fmt_infodir_entry(DocSt, Version, Mod) :-
	% TODO: Do not generate if not necessary (e.g. readme files...)
	verbose_message("{Generating info index", []),
	docst_backend(DocSt, Backend),
	main_output_name(Backend, InfoBase),
	% ( docst_opt(no_version, DocSt) ->
	%     docst_modname(DocSt, NDName),
	%     InfodirName = NDName
	% ; InfodirName = InfoBase % TODO: Not very nice...
	% ),
	% TODO: instead, get a short pretty name for the manual? (that should be used somewhere else too)
	main_output_name_novers(InfodirName),
	%
	atom_codes(InfodirName, InfodirNameS),
	atom_codes(InfoBase, InfoBaseS),
	%
	get_default_title(DocSt, TitleR),
	infodir_version(Version, VersionR),
	%
	% TODO: reuse menu_link?
	R = [raw("* "), raw(InfodirNameS), raw(": ("), raw(InfoBaseS), raw("). "),
	     TitleR, VersionR, raw("."), raw_nl],
	% Write the doctree contents of what will be in the '.infoindex' file
	% (note: see @pred{fmt_infodir_entry2} for more details)
	infodir_base(Mod, ModInfodir),
	%
	docst_set_currmod(DocSt, ModInfodir, DocSt1),
	doctree_scan_and_save(R, ModInfodir, DocSt1),
	%
	verbose_message("}"),
	ttyflush.

get_default_title(DocSt, TitleR) :-
	get_doc(title, ignore, DocSt, TitleR0),
	( is_nonempty_doctree(TitleR0) ->
	    TitleR = TitleR0
	; docst_modname(DocSt, NDName),
	  atom_codes(NDName, NDNameS),
	  TitleR = [raw(NDNameS), raw(" Reference Manual")]
	).

% (only version)
infodir_version(Version, VersionR) :-
	( version_numstr(Version, VerStr) -> 
	    VersionR = [raw(" ("), raw(VerStr), raw(")")]
	; VersionR = []
	).

% (version and date)
%% infodir_version(Version, VersionR) :-
%% 	( version_date(Version, Date),
%% 	  version_numstr(Version, VerStr)
%% 	-> 
%% 	    format_to_string("~w", [Date], DateStr),
%% 	    VersionR = [raw(" (version "),
%% 	                raw(VerStr), raw(" of "),
%% 			raw(DateStr), raw(")")]
%% 	; VersionR = []
%% 	).

% ---------------------------------------------------------------------------

:- doc(subsection, "Version Extraction").
% TODO: Merge with bundle/bundle_info.pl
% TODO: This could be extended to extract version info other revision
%       control system (such as SVN, GIT, Hg)

get_last_version(Version, GlobalVers, DocSt) :-
	( docst_opt(no_version, DocSt) ->
	    Version = [], GlobalVers = []
	; docst_modtype(DocSt, ModuleType),
	  ( ModuleType = part ; ModuleType = plain ) ->
	    Version = [], GlobalVers = []
	; docst_mvar_get(DocSt, dir, dir(Dir)),
	  get_last_version_(Version, GlobalVers, Dir, DocSt)
	).

get_last_version_(Version, GlobalVers, Dir, DocSt) :-
	% TODO: Indeed, directory in 'version_maintenance' could be
 	%       avoided if automatic Manifest.pl detection is
 	%       implemented. The version_maintenance directory is
 	%       indeed the bundle directory.
	get_doc(version_maintenance, dofail, DocSt, dir(VDir)),
	!,
	%% version maintained in dir (computed relative to .pl file Dir!)
	path_concat(Dir, VDir, DirVDir),
	path_concat(DirVDir, 'GlobalChangeLog', ChangeLogFile),
	docst_message("Getting global version from ~w...", [ChangeLogFile], DocSt),
	( file_exists(ChangeLogFile) ->
	    try_finally(
		open(ChangeLogFile, read, CLFS),
		read(CLFS, (:- doc(GlobalVers, _))),
		close(CLFS))
	; error_message(
	        "Version file ~w not found, using version comments in file",
		[ChangeLogFile]),
	  GlobalVers = Version
	),
	get_last_local_version(Version, DocSt).
get_last_version_(Version, Version, _Dir, DocSt) :-
	%% else, component or version maintained in doc/2 decls in file
	get_last_local_version(Version, DocSt).

get_last_local_version(Version, DocSt) :-
	%% get last version in doc/2 decls in file
	docst_message("Getting local version from file...", DocSt),
	( get_doc_changes(DocSt, _, Changes),
	  Changes = [change(LVersion, _)|_] ->
	    Version = LVersion
	; ( setting_value(comment_version, yes) ->
	      docst_inputfile(DocSt, I),
	      note_message(loc(I, 1, 1),
	        "no "":- doc(version(...),...)"" declaration found", [])
	  ; true
	  ),
	  Version = []
        ).

:- use_module(library(system), [file_exists/1]).

%% ---------------------------------------------------------------------------

:- doc(subsection, "Document Module").

:- doc(fmt_module(DocSt,Version,GlobalVers,ModR),
   "This predicate defines the first part of the format of the main
   file of a manual, the introduction, and some auxiliary information.
%
   @var{TitleR} is the intended title of the application (taken from
   the approriate @pred{doc/2} declaration). 

   @var{Version} is the version of the first @pred{doc/2} entry which
   specifies a version number (which should be the current
   version). This is the version of the last local
   change. @var{GlobalVers} is the global version.
   ").

% @var{StartPage} is the page number of the first page of the manual.
%
% @var{Name} is the name of the application (taken from the name of
% the input file). @var{NDName} is the same, but without @tt{_doc},
% if applicable. 

:- pred fmt_module(DocSt,Version,GlobalVers,ModR)
        : docstate(DocSt),
	  version_descriptor(Version), version_descriptor(GlobalVers),
	  doctree(ModR).

fmt_module(DocSt, _Version, _GlobalVers, ModuleR) :-
	docst_modtype(DocSt, ModuleType),
	ModuleType = plain, !,
	docst_mvar_get(DocSt, plain_content, ModuleR).
fmt_module(DocSt, _Version, GlobalVers, ModuleR) :-
	docst_backend(DocSt, Backend),
	Backend = man,
	!,
	% Generates a brief description of the application or library in
	% @concept{unix man format}.
	docst_modtype(DocSt, ModuleType),
	( ModuleType = application ->
	    % TODO: Should 'S' be 'I' here?
	    ( clause_read(_, usage_message(_), true, _, S, LB, LE) ->
	        UsageString = "@begin{verbatim}@includefact{usage_message/1}@end{verbatim}",
		parse_docstring_loc(DocSt, loc(S, LB, LE), UsageString, UsageR)
	    ; note('No usage_message/1 fact found for application'),
	      UsageR = []
	    )
	; UsageR = []
	),
	get_doc(title, warning, DocSt, TitleR),
	get_authors(DocSt, AuthorRs),
	get_doc(address, ignore, DocSt, AddressRs),
	% TODO: 'stability' is ignored for man pages. Good idea?
	get_mod_doc(copyright, DocSt, CopyrightR),
	get_mod_doc(summary,   DocSt, SummaryR),
	ModuleR = man_page(TitleR, GlobalVers, AuthorRs, AddressRs, SummaryR, UsageR, CopyrightR).
fmt_module(DocSt, Version, GlobalVers, ModR) :-
        docst_currmod_is_main(DocSt),
	!,
	docst_message("Generating documentation for main file", DocSt),
	%
	get_mod_doc(copyright, DocSt, CopyrightR),
	% TODO: The right order for non-body sections may depend on
	%       the size of each part. For example, in a book the
	%       acknowledgment part may appear before the
	%       intruduction, while in a paper it can appear at the
	%       end (JFMC)
	fmt_appendix(DocSt, AppendixR),
	fmt_acknowledges(DocSt, AckR),
	( docst_backend(DocSt, html) ->
	    % TODO: Move the appendix and/or acknowledges to a separate page?
	    fmt_bugs(yes(bugs), DocSt, BugsR),
	    fmt_changes(yes(changelog), DocSt, ChangesR),
	    IntroExtraR = [AppendixR, AckR],
	    ExtraR = [BugsR, ChangesR]
	; fmt_bugs(no, DocSt, BugsR),
	  fmt_changes(no, DocSt, ChangesR),
	  IntroExtraR = [AppendixR, AckR, BugsR, ChangesR],
	  ExtraR = []
	),
	fmt_introduction(IntroExtraR, DocSt, IntroR, AfterIntroR),
	fmt_components(DocSt, ComponentsR),
	fmt_biblio(DocSt, BiblioR),
	fmt_indices(DocSt, IndicesR),
	%
	cover_prop(CopyrightR, GlobalVers, DocSt, CoverProp),
	SecProps00 = [CoverProp, level(0)],
	%
	fmt_summary(Version, DocSt, SummaryR), % TODO: summary should not be in the TOC
	fmt_copyright(CopyrightR, DocSt, CopyrightR2),
	%
	fmt_full_toc(DocSt, TocR),
	%
	doctree_simplify([SummaryR, CopyrightR2,
                          TocR,
                          IntroR, AfterIntroR, ExtraR, ComponentsR,
			  BiblioR, IndicesR], DocR),
	%
	fmt_file_top_section(SecProps00, DocR, DocSt, ModR).
fmt_module(DocSt, Version, GlobalVers, ModR) :-
	docst_message("Generating documentation for component", DocSt),
	% Contents inside the cartouche
	module_idx(DocSt, IdxR),
	get_authors(DocSt, AuthorRs),
	fmt_authors(AuthorRs, AuthorR2),
	fmt_version(Version, GlobalVers, VerR),
	%
	get_doc(module, note, DocSt, CommentR0),
	add_lines(CommentR0, CommentR1),
	%
	CommentR2 = [AuthorR2, VerR, CommentR1],
	%
	docst_modtype(DocSt, ModuleType),
	( docst_backend(DocSt, texinfo), ModuleType = part ->
	    % TODO: Good idea?
	    CommentR = optional_cartouche(CommentR2)
	; CommentR = CommentR2
	),
	% Show TOC (when not displayed on the sidebar)
	TocR = show_toc(toc_view(no)),
	%
	doc_interface(DocSt, InterfaceR),
	fmt_appendix(DocSt, AppendixR),
	fmt_acknowledges(DocSt, AckR),
	fmt_bugs(no, DocSt, BugsR),
	fmt_changes(no, DocSt, ChangesR),
	%
	doctree_simplify([IdxR, CommentR, TocR, InterfaceR, AppendixR, AckR, BugsR, ChangesR], DocR),
	%
	fmt_file_top_section([], DocR, DocSt, ModR).

fmt_file_top_section(SecProps0, DocR, DocSt, ModR) :-
	get_doc(title, warning, DocSt, TitleR),
	title_for_module_type(TitleR, DocSt, TitleR2),
	%
	SectLabel = global_label(_),
	%
        ( docst_currmod_is_main(DocSt) -> % main file
	    docst_opt(paper_opts(StartPage, PaperType), DocSt),
	    SecProps2 = [paper_opts(StartPage, PaperType)|SecProps0]
	; % not main file
	  SecProps2 = [level(1)|SecProps0]
	  % (add '***' to parts, deprecated)
%	  SecProps1 = [level(1)|SecProps0],
%	  docst_modtype(DocSt, ModuleType),
%	  ( ModuleType = part ->
%	      SecProps2 = [unnumbered|SecProps1],
%	      doctree_to_rawtext(TitleR2, DocSt, RwTitle),
%	      SectLabel = global_label("*** "||RwTitle)
%	  ; SecProps2 = SecProps1,
%	    SectLabel = global_label(_)
%	  )
	),
	get_doc(pragma, ignore, DocSt, Pragmas),
	SecProps3 = [pragmas(Pragmas)|SecProps2],
	%
	docst_modtype(DocSt, ModuleType2),
	SecProps4 = [module_type(ModuleType2)|SecProps3],
	%
	ModR0 = section_env([file_top_section|SecProps4], SectLabel, TitleR2, DocR),
	insert_show_toc(ModR0, DocSt, ModR).

cover_prop(CopyrightR, GlobalVers, DocSt, CoverProp) :-
	get_authors(DocSt, AuthorRs),
	%
	( is_version(GlobalVers) ->
	    gen_version_note(GlobalVers, GlobalVersR),
	    gen_short_version_note(GlobalVers, GlobalVersShortR)
	; GlobalVersR = nop,
	  GlobalVersShortR = nop
	),
	get_doc(subtitle, ignore, DocSt, SubtitleRs),
	get_doc(subtitle_extra, ignore, DocSt, SubtitleExtraRs),
	get_doc(address, ignore, DocSt, AddressRs),
	CoverProp = coversec(SubtitleRs,
	                     SubtitleExtraRs,
			     AuthorRs,
			     AddressRs,
			     GlobalVersShortR,
			     GlobalVersR,
			     CopyrightR).

fmt_introduction(IntroExtra, DocSt, IntroR, AfterIntroR) :-
	doc_interface(DocSt, InterfaceR),
	get_doc(module, note, DocSt, CommentR),
	add_lines(CommentR, CommentR2),
	%
	IntroProps0 = [level(1),subfile('intro')],
	IntroR0 = [CommentR2, InterfaceR|IntroRest],
	( setting_value(html_layout, 'website_layout') ->
	    % TODO: generalize
	    % Do not emit a section for the introduction
	    IntroRest = [],
	    AfterIntroR = [IntroExtra],
	    IntroR = IntroR0
	; % Emit a section for the introduction
          ( docst_no_components(DocSt),
	    \+ docst_backend(DocSt, html) -> % TODO: strange output in html
	      IntroProps = [unnumbered|IntroProps0],
	      docst_modname(DocSt, NDName),
	      atom_codes(NDName, IntroTitle),
	      OtherInIntro = no
	  ; IntroProps = IntroProps0,
	    IntroTitle = "Introduction",
	    OtherInIntro = yes
	  ),
	  ( OtherInIntro = yes ->
	      IntroRest = [IntroExtra], AfterIntroR = []
	  ; IntroRest = [], AfterIntroR = [IntroExtra]
	  ),
	  IntroR = section_env(
                     IntroProps,
		     global_label(_),
		     string_esc(IntroTitle),
		     IntroR0)
	).

% Use a copyright section?
use_copyright_section(DocSt) :-
	docst_backend(DocSt, html).
% Place the summary text in the cover
summary_in_cover(DocSt) :-
	docst_backend(DocSt, Backend),
        ( Backend = html -> true
	; Backend = texinfo -> true
	; fail
	).
% Add the full table of contents as a section
show_full_toc(DocSt) :-
        % TODO: allow a full TOC in a website? (this is useful as a website directory)
        \+ setting_value(html_layout, 'website_layout'),
	docst_backend(DocSt, html).

module_idx(DocSt, IdxR) :-
	docst_modtype(DocSt, ModuleType),
	( ModuleType \== part,
	  ( ModuleType = application ->
	      TI = apl
	  ; TI = lib
	  ),
	  docst_has_index(TI, DocSt) ->
	    docst_modname(DocSt, NDName),
	    atom_codes(NDName, NDNameC),
	    % note: use a global_label(_) for anchor id since we point to the module itself
	    IdxR = idx_env(def, TI, global_label(_), string_esc(NDNameC), [])
	; IdxR = nop
	).

fmt_authors(AuthorRs, R) :-
	( AuthorRs = [] -> R = []
	; fmt_commas_period(AuthorRs, AuthorListR),
          R = [raw_nl,
	       bf(string_esc("Author(s):")),
	       string_esc(" "),
	       AuthorListR, p("")]
	).

title_for_module_type(TitleR, DocSt, TitleR2) :-
	docst_modtype(DocSt, ModuleType),
	( doctree_is_empty(TitleR) ->
	    docst_modname(DocSt, NDName),
	    atom_codes(NDName, NTitle),
	    ( docst_currmod_is_main(DocSt) ->
	        TitleR2 = [string_esc(NTitle), string_esc(" Reference Manual")]
	    ; ModuleType = application ->
	        TitleR2 = [string_esc(NTitle), string_esc(" (application)")]
	    ; ModuleType = documentation ->
	        TitleR2 = [string_esc(NTitle), string_esc(" (documentation)")]
	    ; ModuleType = part ->
		TitleR2 = string_esc(NTitle)
	    ; TitleR2 = [string_esc(NTitle), string_esc(" (library)")]
	    )
	; TitleR2 = TitleR
	).

fmt_components(_DocSt, ComponentsR) :-
	all_component_specs(Components),
	gen_components_include(Components, ComponentsR).

:- use_module(library(pathnames), [path_basename/2]).

gen_components_include([],                      []).
gen_components_include([Component0|Components], [CR|CsR]) :-
	path_basename(Component0, Base),
	CR = component_link(Base),
	gen_components_include(Components, CsR).

gen_short_version_note(Version, R) :-
	version_string(Version, VersionStr),
	R = [string_esc("Version "), string_esc(VersionStr)].

gen_version_note(Version, R) :-
	version_string(Version, VersionStr),
	R = [sp("1"),
	     string_esc("This documentation corresponds to version "),
	     string_esc(VersionStr),
	     string_esc("."),
	     raw_nl].

fmt_version(Version, GlobalVers, [Rov1, Rov2]) :-
	gen_opt_version_field("Version:", GlobalVers, Rov1),
	( Version == GlobalVers -> Rov2 = []
	; gen_opt_version_field("Version of last change:", Version, Rov2)
	).

gen_opt_version_field(Text, Version, R) :-
	( is_version(Version) ->
	    version_string(Version, VersionStr),
	    R = [raw_nl,
	         bf(string_esc(Text)),
		 string_esc(" "),
		 string_esc(VersionStr),
		 raw_nl]
	; R = []
	).

% ======================================================================

:- doc(section, "The Front/Back Matter").

% ---------------------------------------------------------------------------

:- doc(subsection, "References/Bibliography Section").

fmt_biblio(DocSt, BiblioR) :-
	( docst_opt(no_biblio, DocSt) ->
	    BiblioR = []
	; gen_biblio_section(DocSt, BiblioR)  
	).

% The section where bibliography goes.
% TODO: indices are not generated here, but the section where they go
gen_biblio_section(_DocSt, BiblioR) :-
	Title = "References", 
	BiblioR = section_env([unnumbered,level(1),subfile('refs'),is_special(references)],
                              global_label(_), 
                              string_esc(Title),
                              [
			        show_biblio
                              ]).

% ---------------------------------------------------------------------------

:- doc(subsection, "Index Sections").

fmt_indices(DocSt, IndicesR) :-
	all_indices(DocSt, Indices),
	gen_index_sections(Indices, DocSt, IndicesR).

gen_index_sections([], _DocSt, []).
gen_index_sections([IdxName|Is], DocSt, [IndexR|IsR]) :-
	gen_index_section(IdxName, DocSt, IndexR),
	gen_index_sections(Is, DocSt, IsR).

% The sections where each index goes.
% TODO: This was said previously, but it is not longer true, fix?
%       "Each index goes in a separate file unless no components"
%       (then they go inline)
gen_index_section(IdxName, _DocSt, IndexR) :-
	get_idxsub(IdxName, SubName),
	typeindex(IdxName, IndexId, _, ITitle, IComment),
	add_lines(IComment, IComment2),
	IndexR = section_env(
          [unnumbered,level(1),subfile(SubName),is_special(index)],
           global_label(_), 
           string_esc(ITitle), 
           [
	     IComment2,
	     show_index(IndexId)
	   ]).

% ---------------------------------------------------------------------------

:- doc(subsection, "Formatting Abstract/Summary").

fmt_summary(Version, DocSt, SummaryR) :-
	get_mod_doc(summary, DocSt, SummaryR0),
	fmt_summary_(Version, DocSt, SummaryR0, SummaryR).

fmt_summary_(_Version, _DocSt, SummaryR0, SummaryR) :-
	doctree_is_empty(SummaryR0),
	!,
	SummaryR = nop.
fmt_summary_(Version, DocSt, SummaryR0, SummaryR) :-
	% Version string for summary
	( is_version(Version) ->
	    gen_version_note(Version, VersionR)
	; VersionR = nop
	),
	% Summary contents and version
	add_lines(SummaryR0, SummaryR1),
	SummaryR2 = [SummaryR1, VersionR],
	% Wrap as a section if needed
	summary_sect(DocSt, SummaryR2, SummaryR).

% Summary as a section where TextR is the summary text
summary_sect(DocSt, TextR, SummaryR) :-
	% Options and label for summary (as a section)
	( summary_in_cover(DocSt) ->
	    % NOTE: 'summary_section' is treated in texinfo backend specially
	    Opts = [level(1),summary_section],
	    Label = local_label(_)
	; Opts = [unnumbered,level(1),subfile('summary')],
	  Label = global_label(_)
	),
	SummaryR = section_env(Opts,
	                       Label,
			       string_esc("Summary"),
			       TextR).

% TODO: in pdf/ps, summary may need to appear with a different page style?

% ---------------------------------------------------------------------------

:- doc(subsection, "Full Table of Contents").

fmt_full_toc(DocSt, R) :-
        show_full_toc(DocSt),
	!,
	Opts = [unnumbered,level(1),subfile('fulltoc'),is_special(toc)],
	Label = global_label(_),
	R = section_env(Opts,
	                Label,
			string_esc("Table of Contents"),
			[show_toc(full)]).
fmt_full_toc(_, []).

% ---------------------------------------------------------------------------

:- doc(subsection, "Copyright").

fmt_copyright(CopyrightR, DocSt, CopyrightR2) :-
	% TODO: Note: I cannot creat subfiles during the backend treatment (references will not work)
	%       This is why I write the copyright here and not in the HTML cover.
	( use_copyright_section(DocSt),
	  \+ doctree_is_empty(CopyrightR) ->
	    CopyrightR2 = section_env([level(1), subfile('copyright'), is_special(copyright)],
	                              global_label(_),
				      string_esc("Copyright"),
				      CopyrightR)
	; CopyrightR2 = nop
	).

% ---------------------------------------------------------------------------

:- doc(subsection, "Appendix Section").
% TODO: Document sections could make this obsolete.

fmt_appendix(DocSt, AppendixR) :-
	get_doc(appendix, ignore, DocSt, AppendixR0),
	add_lines(AppendixR0, AppendixR1),
	nonbody_section(no, 'appdx', "Other information", AppendixR1, DocSt, AppendixR).

% ---------------------------------------------------------------------------

:- doc(subsection, "Acknowledges").
% TODO: Support big/small acknowledgement sections?

fmt_acknowledges(DocSt, AckR) :-
	get_doc(ack, ignore, DocSt, AckR0),
	add_lines(AckR0, AckR1),
	nonbody_section(no, 'ack', "Acknowledgments", AckR1, DocSt, AckR).

% ---------------------------------------------------------------------------

:- doc(subsection, "Bugs, Issues, Planned Improvements"). % wish list?
% TODO: This needs a major rework...
% TODO: Add identifiers to bugs

fmt_bugs(Special, DocSt, BugsR) :-
	( docst_opt(no_bugs, DocSt) ->
	    BugRs=[]
	; get_doc(bug, ignore, DocSt, BugRs)
	),
	gen_bugs(BugRs, Bugs2),
	nonbody_section(Special, 'bugs', "Known bugs and planned improvements", Bugs2, DocSt, BugsR).

gen_bugs([], nop) :- !.
gen_bugs(Xs, R) :-
	gen_bugs_(Xs, Items),
	R = itemize_bullet(Items).

gen_bugs_([], []).
gen_bugs_([BugR|BugRs], [C|Cs]) :-
	C = [item(""), BugR],
	gen_bugs_(BugRs, Cs).

% ---------------------------------------------------------------------------

:- doc(subsection, "Release Notes / Changelog"). % also corrigendum?
% TODO: In a real book, changes w.r.t. real editions are explained in
%       the foreword (which appears just after the contents)

% The changelog.
fmt_changes(Special, DocSt, ChangesR) :-
	( docst_opt(no_changelog, DocSt) ->
	    Changes = []
	; ( docst_opt(no_patches, DocSt) ->
	      VPatch = 0
	  ; true
	  ),
	  get_doc_changes(DocSt, VPatch, Changes)
	),
	gen_changes(Changes, Changes2),
	nonbody_section(Special, 'changes', "Version/Change Log", Changes2, DocSt, ChangesR).

gen_changes([], nop) :- !.
gen_changes(Xs, R) :-
	gen_changes_(Xs, Items),
	R = description_env(Items).

gen_changes_([], []).
gen_changes_([change(Version, ChangeTextR)|Changes], [C|Cs]) :- !,
	version_string(Version, VersionStr),
	C = [item(bf(string_esc("Version "||VersionStr))), ChangeTextR],
	gen_changes_(Changes, Cs).

% ---------------------------------------------------------------------------

:- doc(subsection, "Helper Code for Non-body (Front/Back) Matter Sections").
% TODO: Rename

nonbody_section(Special, Sub, Title, BodyR, DocSt, SectR) :-
	( doctree_is_empty(BodyR) ->
	    SectR = nop
	; nonbody_props(Special, Sub, DocSt, SecProps, SectLabel),
	  % display(user_error, op(Sub, SecProps, SectLabel)), nl(user_error),
	  SectR = section_env(
            SecProps,
	    SectLabel,
	    string_esc(Title),
	    BodyR
          )
	).

nonbody_props(Special, Sub, DocSt, SecProps, SectLabel) :-
	( docst_currmod_is_main(DocSt) ->
	    ( Special = yes(Special2) ->
	        %% Detach in a subfile (and add is_special property)
	        SecProps = [level(1),subfile(Sub),is_special(Special2)],
		SectLabel = global_label(_)
% TODO: This code generated a buggy doctree for 'texinfo' backend
%       when documenting single modules. Make sure that it is really
%       not useful somewhere else. Then, remove.
%%	    ; docst_no_components(DocSt),
%%	      \+ docst_backend(DocSt, html) -> % TODO: strange output in html
%%	        %% Detach in a subfile
%%	        display(nobodyhere), nl,
%%	        SecProps = [level(0),subfile(Sub)],
%%		SectLabel = global_label(_)
	    ; SecProps = [level(2)],
	      SectLabel = local_label(_)
	    )
	; SecProps = [with_parent,level(2)],
	  SectLabel = local_label(_)
	).

% Add optional line break
% TODO: Wrong if we add lines and there is no following text; it could
%       be easier if we work with paragraphs (or simplify linebreaks
%       later)
add_lines(R, R) :- doctree_is_empty(R), !.
add_lines(R0, R) :- R = [raw_nl, R0, linebreak].

% Remove the text detailing the author contribution
% TODO: Not documented!
remove_author_details(Text, Name) :-
	append(Name0, "("||_, Text),
	reverse(Name0, Name1),
	remove_leading_blanks(Name1, Name2),
	reverse(Name2, Name).

% TODO: useful?
remove_leading_blanks([X|Xs], Ys) :- is_blank(X), !,
 	remove_leading_blanks(Xs, Ys).
remove_leading_blanks(Xs, Xs).
 
is_blank(0' ).
is_blank(0'\t).
is_blank(0'\n).

:- regtype change/1.
change(change(_Version, _Change)).
change(_).

% ======================================================================

:- doc(subsection, "Module Inferface Formatting").

:- pred doc_interface/2 # "Document the module interface.".

doc_interface(DocSt, R) :-
	docst_modtype(DocSt, ModuleType),
	( ModuleType = application
	; ModuleType = documentation
	; ModuleType = part
	),
	!,
	R = [].
doc_interface(DocSt, R) :-
	docst_mvar_get(DocSt, modinfo, ModSt),
	ModSt = modinfo(M, Base),
	%
	docst_message("Generating library header...", DocSt),
	%
	docst_modtype(DocSt, ModuleType),
	% Exported predicates
	export_list(Base, DocSt, AllExports),
	eliminate_hidden(AllExports, Exports),
	% Multifiles
	findall(F/A, def_multifile(Base, F, A, _), RMultifiles),
	eliminate_hidden(RMultifiles, Multifiles),
	% Check if there are definitions to be documented
	check_no_definitions(ModuleType, Exports, Multifiles, DocSt),
	%
	% Imported modules
	findall(IFile, uses_file(Base, IFile), IFiles),
	% Other user files loaded
	findall(IUFile, adds(Base, IUFile), IUFiles),
	% Packages
	get_pkgs(M, Base, DocSt, PkgFiles),
	%
	% Source files whose contents should not be documented
	get_doc(nodoc, ignore, DocSt, NoDocS),
	docst_message("Not documenting: ~w", [NoDocS], DocSt),
	%
	get_ops(ModuleType, NoDocS, SOps),
	%
	% The modes (only "exported" if package or include)
	get_modes(M, ModuleType, NoDocS, NModes),
	%
	% Gather all decls to be documented.
	get_decls(Base, M, ModuleType, NoDocS, NDecls),
	%
	% Internals  
	get_doc(doinclude, ignore, DocSt, Preds),
	filter_out_exports(Preds, Exports, Internals),
	%
	classify_exports(Exports, M, Base, CExports),
	classify_files(IFiles, UFiles, SysFiles, EngFiles, DocSt),
	%
	fmt_module_usage(DocSt, CExports, Multifiles,
	    UFiles, IUFiles, SysFiles, EngFiles, PkgFiles,
	    SOps, NDecls,
	    NModes, ModuleUsageR),
	%
	% new declarations
	fmt_definitions_kind(decl, "new declarations", NDecls, DocSt, DeclsR),
	% any modes defined
	fmt_definitions_kind(modedef, "new modes", NModes, DocSt, ModesR),
	% exported predicates, props, etc.
	fmt_definitions_kind(nodecl, "exports", Exports, DocSt, ExportsR),
	% multifile predicates
	fmt_definitions_kind(nodecl, "multifiles", Multifiles, DocSt, MultifilesR),
	% predicates for which it is explicitly requested (via a
	% @tt{:- doc(doinclude,<PredName>)} directive)
	fmt_definitions_kind(_DefKind, "internals", Internals, DocSt, InternalsR),
	R = [ModuleUsageR, DeclsR, ModesR, ExportsR, MultifilesR, InternalsR].

modtype_include_or_package(include).
modtype_include_or_package(package).

% Show a warning if there are no definitions to be documented.
check_no_definitions(ModuleType, Exports, Multifiles, DocSt) :-
	( ( Exports=[], Multifiles=[],
	    \+ modtype_include_or_package(ModuleType)
	  ) ->
	    docst_inputfile(DocSt, I),
	    warning_message(loc(I, 1, 1),
		"no exported predicates to be documented", [])
	; true
	).

:- doc(fmt_module_usage(DocSt,Exports,Mults,
   UMods,IUMods,SMods,EMOds,PkgMods,
   Ops,NDecls,NModes,R), "This
   predicate defines the format of the usage info for the
   module. @var{Exports} contains the predicates
   exported by the module (taken from the @pred{module/2}
   declaration). @var{UMods} contains the user modules imported by the
   module. @var{SMods} contains the system modules imported by the
   module. @var{EMods} contains the internal (engine) modules imported
   by the module. @var{Ops} contains any exported operator
   definitions. @var{NDecls} contains any exported new
   declarations. @var{NModes} contains any new mode
   definitions.").

:- pred fmt_module_usage(DocSt, Exports, Mults,
	    UMods, IUMods, SysMods, EngMods, PkgMods,
	    Ops, NDecls, NModes, R)
	: ( docstate(DocSt), list(Exports, predname),
	    list(Exports, predname),
	    list(UMods), list(IUMods), list(SysMods), list(EngMods), list(PkgMods),
	    list(Ops), list(NDecls, atm), list(NModes, atm),
	    doctree(R) )
# "The module header info is documented as the first section of the
   chapter.".
fmt_module_usage(DocSt, CExports, Mults,
	    UMods, IUMods, SysMods, EngMods, PkgMods, Ops, NDecls,
	    NModes, R) :-
	get_doc(usage, ignore, DocSt, UsageR0),
	( is_nonempty_doctree(UsageR0) ->
	    % Usage comment to override automatic one
	    UsageR = UsageR0
	; docst_inputfile(DocSt, AbsFile),
	  get_modspec(AbsFile, ModSpec0), % e.g., library(lists)
	  modspec_nodoc(ModSpec0, ModSpec1), % omit _doc suffix, collapse '(.../)a/a' as '(.../)a'
	  docst_modtype(DocSt, ModuleType),
	  modtype_usage_command(ModuleType, Cmd),
	  % Remove library(_) from packages 
	  ( ModuleType = package, ModSpec1 = library(ModSpec2) ->
	      ModSpec = ModSpec2
	  ; ModSpec = ModSpec1
	  ),
	  % TODO: make sure that module spec is correct! (it is not now)
	  ( ModuleType = package ->
	      format_to_string(":- ~w(~w).", [Cmd, ModSpec], UseDeclR0),
	      format_to_string(":- module(...,...,[~w]).", [ModSpec], UseDeclR1),
	      % TODO: use linebreak? or p("")?
	      UsageR = [tt(string_esc(UseDeclR0)), raw_nl, raw_nl, string_esc("or"), raw_nl, raw_nl, tt(string_esc(UseDeclR1))]
	  ; format_to_string(":- ~w(~w).", [Cmd, ModSpec], UseDeclR0),
            UsageR = tt(string_esc(UseDeclR0))
	  )
	),
	gen_item(bf, string_esc("Library usage"), UsageR, Pa1),
	%
	( CExports = [] ->
	    Pa2 = nop
	; Filters = [("PREDICATE", string_esc("Predicates")),
	             ("FUNCTION", string_esc("Functions")),
		     ("PROPERTY", string_esc("Properties")),
		     ("FUNCTION", string_esc("Functions")),
		     ("REGTYPE", string_esc("Regular Types")),
		     %% (enable to document modes)
		     % ("MODE", string_esc("Modes")),
		     ("ENTRY POINT", string_esc("Entry points"))],
	  gen_classified_export_cases(Filters, CExports, E1),
	  gen_cases(em, string_esc("Multifiles"), Mults, E2),

	  gen_item(bf, string_esc("Exports"), itemize_minus([E1, E2]), Pa2)
	),
	gen_cases(bf, string_esc("New operators defined"), Ops, Ro),
	gen_cases(bf, string_esc("New modes defined"), NModes, Rm),
	gen_cases(bf, string_esc("New declarations defined"), NDecls, Rd),
	( IUMods = [], UMods = [], SysMods = [], EngMods = [], PkgMods = [] ->
	    Pa3 = nop
	; gen_cases(em, string_esc("Application modules"), UMods, L1),
	  gen_cases(em, [string_esc("Files of module "), tt(string_esc("user"))], IUMods, L2),
	  gen_cases(em, string_esc("System library modules"), SysMods, L3),
	  gen_cases(em, string_esc("Internal (engine) modules"), EngMods, L4),
	  gen_cases(em, string_esc("Packages"), PkgMods, L5),
	  gen_item(bf, string_esc("Imports"), itemize_minus([L1, L2, L3, L4, L5]), Pa3)
	),
	% TODO: this section_env contains more things!
	R = section_env(
              [with_parent,level(2)],
              local_label(_),
              string_esc("Usage and interface"),
	      cartouche(itemize_bullet([Pa1, Pa2, Ro, Rm, Rd, Pa3]))
            ).

modtype_usage_command(module,  use_module).
modtype_usage_command(user,    ensure_loaded).
modtype_usage_command(include, include).
modtype_usage_command(package, use_package).

gen_classified_export_cases([], _CExports, []).
gen_classified_export_cases([(Type, LabelR)|Xs], CExports, [R|Rs]) :-
	filter_export_by_type(CExports, Type, CExports2),
	gen_cases(em, LabelR, CExports2, R),
	gen_classified_export_cases(Xs, CExports, Rs).

gen_item(bf, TextR, B, R) :-
	R = [item(""), bf([TextR, string_esc(":")]), linebreak, B].
gen_item(em, TextR, B, R) :-
	R = [item(""), em([TextR, string_esc(":")]), linebreak, B].

gen_cases(Style, LabelR, Xs, R) :-
	( Xs = [] ->
	    R = nop
	; fmt_terms(Xs, code, Xs1), % TODO: 'code' could be made more specific (e.g. lib, op, ...)
	  fmt_commas_period(Xs1, Xs2),
	  gen_item(Style, LabelR, [Xs2, raw_nl], R)
	).

% ----------------------------------------------------------------------

% Get the list of exports with the same assertion type
% TODO: assertion type is based on comparing the text representation
filter_export_by_type([], _TypeStr, []).
filter_export_by_type([export(F/A, Type)|CExps], TypeStr, [F/A|FExps]) :-
	assrt_type_text(Type, TypeStr, _, _),
	!,
	filter_export_by_type(CExps, TypeStr, FExps).
filter_export_by_type([_|CExps], TypeStr, FExps) :-
	!,
	filter_export_by_type(CExps, TypeStr, FExps).

%% %% Version when also documenting imports:
%% handle_export_cases([],[],_DocSt,_OS) :- !.
%% handle_export_cases([],Mods,_DocSt,OS) :- 
%% 	Mods \== [], !,
%% 	format(OS,"~n@item @strong{Exports:} 
%%                     see exports of imported modules.~n~n",[]).
%% handle_export_cases(Exports,[],DocSt,OS) :- 
%% 	Exports \== [], !,
%% 	format(OS,"~n@item @strong{Exports:}~n~n",[]),
%% 	fmt_terms_commas_period(Exports,global,DocSt,OS).
%% handle_export_cases(Exports,Mods,DocSt,OS) :-
%% 	Exports \== [], Mods \== [], !,
%% 	format(OS,"~n@item @strong{Exports} (see also exports of imported 
%%                    modules):~n~n",[]),
%% 	fmt_terms_commas_period(Exports,global,DocSt,OS).

%%%% Alternative version (using a table)
%% 	format(OS,"@sp 1~n@table @strong~n",[]),
%% 	format(OS,"~n@item @strong{Library usage:}~n",[]),
%% 	format(OS,"@code{:- use_module(library(~w))}~n",[Name]),
%% 	format(OS,"~n@item @strong{Exported predicates:}~n",[]),
%% 	fmt_terms_commas_period(Exports,no_ref,OS),
%% 	( UMods = [] -> true
%%         ; 
%% 	  format(OS,"~n@item @strong{Other modules used:}~n",[]),
%% 	  fmt_terms_commas_period(UMods,no_ref,OS) ),
%% 	format(OS, "@end table~n",[]).

%% fmt_terms_commas_period(Cs, Type, Rs) :-
%% 	fmt_terms(Cs, Type, Rs0),
%% 	fmt_commas_period(Rs0, Rs).

% ---------------------------------------------------------------------------

fmt_terms([], _Type, []) :- !.
fmt_terms([C|Cs], Type, [R|Rs]) :-
	fmt_term(C, Type, R),
	fmt_terms(Cs, Type, Rs).

% Format a term of a given 'type' (if necessary, add the commands to
% include in its corresponding index too)
fmt_term(op(Prec, Assoc, EFunctor), Type, R) :- !,
	op_arity(Assoc, Arity),
	C2 = EFunctor/Arity,
	fmt_code_spec(C2, ER),
	( Type = no_ref ->
	    RR = []
	; RR = idx_env(use, Type, localnum_label(_), ER, ER)
	),
	format_to_string(" [~w,~w]", [Prec, Assoc], PrecS),
	R = [RR, string_esc(PrecS)].
fmt_term(C, Type, R) :- !,
	fmt_code_spec(C, ER),
	( Type = no_ref ->
	    RR = []
	; RR = idx_env(use, Type, localnum_label(_), ER, ER)
	),
	R = RR.

op_arity(fy,  1).
op_arity(yf,  1).
op_arity(fx,  1).
op_arity(xf,  1).
op_arity(xfx, 2).
op_arity(xfy, 2).
op_arity(yfx, 2).

fmt_code_spec(C, S1) :-
	( C = F/A ->
	    %% This is to avoid parenthesis around operators...
	    format_to_string("~w/~w", [F, A], S)
	; format_to_string("~w", [C], S)
	),
	S1 = string_esc(S).

%% ---------------------------------------------------------------------------
:- pred export_list/3 # "Builds the list of exported
   predicates. Handles the special case of @tt{user} files.".
% TODO: also 'include' and 'package'?

export_list(Base, DocSt, AllExports) :-
	docst_modtype(DocSt, ModuleType),
        ( ModuleType = module ->
	    findall(F/A, exports(Base, F, A, _, _), AllExports)
	; % We may need to add here the case of predicates which are not defined 
	  % but for which there is an assertion?
	  findall(F/A, defines(Base, F, A, _, _), DupAllExports),
	  eliminate_duplicates(DupAllExports, AllExports),
	  docst_message("Documenting all defined predicates: ~w",
	    [AllExports], DocSt)
	).

%% ---------------------------------------------------------------------------
:- pred eliminate_hidden/2 # "Eliminates from the export list those
   predicates affected by a comment with @tt{hide} in the second
   argument.".

eliminate_hidden([],           []).
eliminate_hidden([Pred|Preds], EPreds) :-
	pred_has_docprop(Pred, hide),
	!,
	eliminate_hidden(Preds, EPreds).
eliminate_hidden([Pred|Preds], [Pred|EPreds]) :-
	eliminate_hidden(Preds, EPreds).

%% ---------------------------------------------------------------------------

% Get the ops defined in the module
get_ops(ModuleType, NoDocS, SOps) :-
	% The ops (only "exported" if package or include)
	( modtype_include_or_package(ModuleType) ->
	    findall(op(P, Prec, PredNames),
	            get_ops_(P, Prec, PredNames, NoDocS),
		    Ops),
	    normalize_ops(Ops, SOps)
	; SOps=[]
	).

get_ops_(P, Prec, PredNames, NoDocS) :-
	clause_read(_, 1, op(P, Prec, PredNames), _, S, _, _),
	path_basename(S, FN),
	path_splitext(FN, BN, _),
	\+ member(BN, NoDocS).

:- pred normalize_ops/2 # "Flattens out the cases where several ops
   are defined in the same declaration.".

normalize_ops([],       []).
normalize_ops([Op|Ops], [Op|NOps]) :-
	Op = op(_, _, L),
	atom(L),
	!,
	normalize_ops(Ops, NOps).
normalize_ops([Op|Ops], TNOps) :-
	Op = op(Prec, Style, LPred),
	normalize_ops_list(LPred, Prec, Style, TNOps, TNOpsE),
	!,
	normalize_ops(Ops, TNOpsE).

normalize_ops_list([],           _Prec, _Style, NOpsE,
	    NOpsE).
normalize_ops_list([Pred|Preds], Prec,  Style,  [op(Prec, Style, Pred)|NOps],
	    NOpsE) :-
	normalize_ops_list(Preds, Prec, Style, NOps, NOpsE).

%% ---------------------------------------------------------------------------
% Get the modes defined in a module

get_modes(M, ModuleType, NoDocS, NModes) :-
	( modtype_include_or_package(ModuleType) ->
	    findall(F/A, ( assertion_read(ModeP, M, _, modedef, _, _, S, _, _),
		    path_basename(S, FN),
		    path_splitext(FN, BN, _),
		    \+ member(BN, NoDocS),
		    functor(ModeP, F, A) ),
		CModes),
	    eliminate_duplicates(CModes, NModes)
	; NModes = []
	).

%% ---------------------------------------------------------------------------
% Gather all decls to be documented. 
% TODO: ??? Not a good idea???

get_decls(Base, M, ModuleType, NoDocS, NDecls) :-
	( modtype_include_or_package(ModuleType) ->
	    % document all having an explicit comment in the module:
	    findall(F/A, 
                    ( assertion_read(DeclP, M, _, decl, _, _, S, _, _),
		        path_basename(S, FN),
		        path_splitext(FN, BN, _),
		        \+ member(BN, NoDocS),
		        functor(DeclP, F, A) ),
		CDecls),
	    % also those having a new_declaration in the module
	    findall(NDP,
		( clause_read(Base, 1, new_declaration(NDP), _, S, _, _),
		    path_basename(S, FN),
		    path_splitext(FN, BN, _),
		    \+ member(BN, NoDocS)
		),
		NDDecls),
	    append(CDecls, NDDecls, PDupDecls),
	    % E.g., in case of being in both cases above
	    eliminate_duplicates(PDupDecls, NDecls)
	; NDecls=[]
        ).

%% ---------------------------------------------------------------------------
:- pred classify_exports/4 # "Classifies exported predicates as
   predicates, declarations, etc. according to the declared type in
   associated assertions. Also captures the special case of regular
   types (by detecting the corresponding property in the comp part).".

classify_exports([], _M, _Base, []).
% if local, look in local assertions
classify_exports([F/A|Exps], M, Base, AllExps) :-
	functor(P, F, A),
	check_types_in_assertions(P, F, A, M, Exports),
	Exports \== [],
	!,
	append(Exports, CExps, AllExps),
	classify_exports(Exps, M, Base, CExps).
% if imported, look in assertions from that module
classify_exports([F/A|Exps], M, Base, AllExps) :-
	functor(P, F, A),
	imports_pred(Base, UM, F, A, _, _, _),
	check_types_in_assertions(P, F, A, UM, Exports),
	!,
	append(Exports, CExps, AllExps),
	classify_exports(Exps, M, Base, CExps).
% else, assume pred.
classify_exports([F/A|Exps], M, Base, [export(F/A, pred)|CExps]) :-
	classify_exports(Exps, M, Base, CExps).

%% Includes special case for regular types.
check_types_in_assertions(P, F, A, M, Exports) :-
	findall(export(F/A, PType),
	    ( assertion_read(P, M, _Status, Type, NAss, _Dict, _S, _LB, _LE),
		defkind_pred(Type),
		patch_special_prop(Type, NAss, PType) ),
	    DExports),
	eliminate_duplicates(DExports, Exports).

patch_special_prop(Type, NAss, NewType) :-
	Type == prop,
	assertion_body(_, _, _, _, GP, _, NAss),
	special_prop(IdentifyingProp, NewType),
	member(IdentifyingProp, GP),
	!.
patch_special_prop(Type, _NAss, Type).

%% ---------------------------------------------------------------------------
:- pred special_prop(CProp, Type) # "If a property definition has
   @var{CProp} in its comp (+) field, then it is a special property of
   type @var{Type}.".

%% Native properties should be added here also? Perhaps dynamically?

special_prop(regtype(_), regtype).

%% ---------------------------------------------------------------------------
% Get the list of imported packages

get_pkgs(M, Base, DocSt, PkgFiles) :-
	( docst_opt(no_packages, DocSt) ->
	    % do not document imported packages
	    PkgFiles = []
	; findall(PkgFile, use_pkg(Base, PkgFile), PkgFiles0),
	  filter_pkgs(PkgFiles0, M, PkgFiles1),
	  pkgs_omit_lib(PkgFiles1, PkgFiles)
	).

% Remove library(_) functor from libraries (implicit by default)
pkgs_omit_lib([], []).
pkgs_omit_lib([P|Ps], [Q|Qs]) :-
	( P = library(Q) -> true ; Q = P ),
	pkgs_omit_lib(Ps, Qs).

% Remove packages that seem to be using themselves (like
% library(assertions)). That happens because the documentation code is
% separated from the package code).
filter_pkgs([], _, []).
filter_pkgs([P0|Ps0], M, Qs) :-
	( P0 = library(P) ->
	    true
	; P = P0
	),
	( same_mod(P, M) ->	    
%	    display(user_error, same_mod(P, M)), nl(user_error),
	    Qs = Qs0
	; Qs = [P0|Qs0]
	),
	filter_pkgs(Ps0, M, Qs0).

% E.g. 
%   same_mod(assertions,user(/Users/jfran/Documents/git/ciao-devel/core/lib/assertions/assertions_doc))
%   same_mod(regtypes,user(/Users/jfran/Documents/git/ciao-devel/core/lib/regtypes/regtypes_doc))

same_mod(ModSpec, M) :-
 	( M = user(S) ->
 	    path_basename(S, FN),
 	    path_splitext(FN, M2, _)
	; M2 = M
	),
	modname_nodoc(M2, M_ND),
	%
	modspec_nodoc(ModSpec, ModSpec2),
	modspec_name(ModSpec2, Name_ND),
	%
	M_ND = Name_ND.

modspec_name(ModSpec, Name) :-
	( ModSpec =.. [_Alias, Path] ->
	    true
	; Path = ModSpec
	),
	( Path = _/Name0 -> true ; Name0 = Path ),
	path_basename(Name0, Name1),
	path_splitext(Name1, Name, _).

%% ---------------------------------------------------------------------------
:- pred classify_files/5 # "Classifies file references, such as
   @tt{library(aggregates)}, into separate lists according to whether
   they are System, Engine, User, etc.".

:- use_module(library(bundle/paths_extra),
	[fsRx_get_bundle_and_basename/3]).

classify_files(IFiles, UFiles, SysFiles, EngFiles, DocSt) :-
	classify_files_(IFiles, UFiles, SysFiles, EngFiles, DocSt).

classify_files_([], [], [], [], _).
classify_files_([File|Files], UFiles, SFiles, EFiles, DocSt) :-
	( fsRx_get_bundle_and_basename(File, Bundle, ModName) ->
	    true
	; % (no bundle! perhaps a local file without any Manifest)
	  Bundle = '',
	  ModName = File
	),
	( File = engine(_) ->
	    % Engine module
	    ( docst_opt(no_engmods, DocSt) -> % (omit module)
	        EFiles = EFiles0
	    ; EFiles = [ModName|EFiles0]
	    ),
	    UFiles = UFiles0, SFiles = SFiles0
	; get_parent_bundle(ThisBundle), % TODO: cache?
	  \+ Bundle = ThisBundle
	->
	    % System module (from other bundle)
	    ( docst_opt(no_sysmods, DocSt) -> % (omit module)
	        SFiles = SFiles0
	    ; SFiles = [ModName|SFiles0]
	    ),
	    UFiles = UFiles0, EFiles = EFiles0
	; % Otherwise, application/user module (this bundle)
	  UFiles = [ModName|UFiles0],
	  SFiles = SFiles0, EFiles = EFiles0
	),
	classify_files_(Files, UFiles0, SFiles0, EFiles0, DocSt).

%% ---------------------------------------------------------------------------

:- pred fmt_definitions_kind/5
   # "Generates documentation for definitions of predicates,
     declarations, modes, etc. of a given kind.".

fmt_definitions_kind(DefKind, Desc, Items, DocSt, R) :-
	docst_message("Documenting "||Desc, DocSt),
	( Items = [] ->
	    R = []
	; fmt_definitions(Items, DefKind, DocSt, ItemsR),
	  Title = "Documentation on "||Desc,
	  R = section_env([with_parent,level(2)], local_label(_), string_esc(Title), ItemsR)
	).

%% ---------------------------------------------------------------------------

:- pred filter_out_exports/3 # "Eliminates the predicates already
   documented as exports so that they are not documented twice.".

filter_out_exports([],          _Exports, []).
filter_out_exports([F/A|Preds], Exports,  FPreds) :-
	member(F/A, Exports),
	!,
	filter_out_exports(Preds, Exports, FPreds).
filter_out_exports([PredList|Preds], Exports, FPreds) :-
	%% doc/2 argument is list
	list(PredList),
	!,
	filter_out_exports(PredList, Exports, FilteredPreds),
	filter_out_exports(Preds,    Exports, OtherFilteredPreds),
	append(FilteredPreds, OtherFilteredPreds, FPreds).
filter_out_exports([Pred|Preds], Exports, [Pred|FPreds]) :-
	filter_out_exports(Preds, Exports, FPreds).

%% ---------------------------------------------------------------------------

:- doc(subsection, "Document Definitions (predicates or declarations)").

:- pred fmt_definitions/4
   # "Generates documentation for a list of predicates.

      One issue here, given that there may be reexports, is which
      assertions and code to use in the documentation. The best thing
      seems to be to use the assertions that are either in the file
      being documented or, if none exist, in the closest file in the
      reexport chain. This is symmetric with the fact that local code
      takes precedence over imported code.

      Thus, we treat the assertions in the current module first.
      Otherwise, we follow import chain.".

fmt_definitions([],     _,    _DocSt, []).
fmt_definitions([P|Ps], DefKind, DocSt, [R|Rs]) :-
	docst_mvar_get(DocSt, modinfo, ModSt),
	fmt_definition(P, DefKind, ModSt, DocSt, R),
	fmt_definitions(Ps, DefKind, DocSt, Rs).

%% General case:
:- pred fmt_definition/5
   # "Generates documentation for one definition (predicate or declaration).".
% TODO: ModSt is passed due to reexport chains

fmt_definition(F/A, DefKind, ModSt, DocSt, R) :-
	ModSt = modinfo(M, Base),
	%
	docst_message("Generating documentation for predicate or declaration ~w:~w/~w", [M, F, A], DocSt),
	functor(P, F, A),
	predicate_usages(P, DefKind, M, Usages),
	predicate_level_comment(F/A, DocSt, CommentR, CommentHead),
	other_assertions(P, DefKind, M, OtherAssrt),
	%
	( ( DefKind == decl
	  ; DefKind == modedef
	  ) ->
	    PType = DefKind
	; look_for_pred_type(Usages, F/A, PType)
	),
	% Check that there are assertions, get assertion type
	( ( Usages \== []
	  ; OtherAssrt \== []
	  ; is_nonempty_doctree(CommentR)
	  ) ->
	    %% If there are any assertions, then succeed and thus 
	    %% definitely document with them.
	    NCommentR = CommentR
	; ( ( \+ imports_pred(Base, _, F, A, _, _, _)
	    ; defines(Base, F, A, _, _)
	    ) -> %% No assertions, and predicate is not imported: too bad
	      NCommentR = string_esc("No further documentation available for this predicate."),
	      get_first_loc_for_pred(F, A, Loc),
	      warning_message(Loc,
	        "no assertions or comments found for ~w ~w", [PType, F/A])
	  ; %% Else, probably imported (otherwise, fail globally)
	    fail
	  )
	),
	!,
	assrt_type_text(PType, PText, _, _),
	%% In case of explicit arguments, CP should also be included...
	( member(TmpAssrt, OtherAssrt),
	    TmpAssrt = assertion_read(_, _, _, _, TmpNAss, _, _, _, _),
	    assertion_body(_, _, _, _, TGP, _, TmpNAss),
	    member(iso(_), TGP)
	->  Standard = iso
	; Standard = non_iso
	),
	fmt_head_descriptor(CommentHead, PType, Standard, HeadR),
	%% Trying to catch props that are just declared with no comment:
	( doctree_is_empty(NCommentR),
	  Usages = [assertion_read(_, _, _, _, NAss, _, _, _, _)], %% N=1,
	  assertion_body(_, _, _, _, _, [], NAss), %% I.e., no comment
	  assrt_is_prop(PType) ->
	    assrt_type_ptext(PType, PropText),
	    atom_codes(F, FS),
	    number_codes(A, AS),
	    list_concat(["A ", PropText,
		    ", defined as follows:@includedef{",
		    FS, "/", AS,
		    "}\n"], TNComment), %% Added \n
	    get_first_loc_for_pred(F, A, Loc),
	    note_message(Loc,
		"no comment text for ~s ~w, including definition", [PropText,
		    F/A]),
	    parse_docstring_loc(DocSt, Loc, TNComment, NNCommentR0)
	; NNCommentR0 = NCommentR
	),	
	( ( CommentHead = _/_ ; doctree_is_empty(NNCommentR0) ) ->
	    NNCommentR1 = NNCommentR0
	; NNCommentR1 = [p(""), NNCommentR0]
	),
	% The language declarations
	doc_native_declarations(F/A, M, Base, DocSt, NativeR),
	% (not 'pred' assertions')
	doc_other_assertions(Usages, OtherAssrt, F/A, PType, DocSt, OtherAssrtR),
	% ('pred' assertions')
	doc_usages(Usages, F/A, PType, DocSt, UsagesR),
	% Put all together
	( doctree_is_empty(NativeR),
	  doctree_is_empty(OtherAssrtR) ->
	    PredR = [UsagesR]
	; PredR = [UsagesR, NativeR, OtherAssrtR]
	),
	R = [defpred(local_label(_), PType, PText, F/A, [
               HeadR, NNCommentR1, PredR
               ]),
             sp("1"), raw_nl].
fmt_definition(F/A, DefKind, ModSt, DocSt, R) :-
	ModSt = modinfo(M, Base),
	imports_pred(Base, UFile, F, A, _DefType, _Meta, _EndFile),
	base_name(UFile, UBase),
	defines_module(UBase, UM),
	M \== UM, %% To handle engine preds: they appear as imported 
	%% in the file in which they are defined!
	!,
	( pred_has_docprop(F/A, doinclude) ->
	    docst_message("following reexport chain for ~w to ~w", [F/A, UM], DocSt),
	    fmt_definition(F/A, DefKind, modinfo(UM, UBase), DocSt, R)
	;
	    docst_message("~w reexported from ~w (not documented)", [F/A, UM], DocSt),
	    Type = udreexp,
	    assrt_type_text(Type, PText, _, _),
	    atom_codes(UM, UMS),
	    list_concat(["\n\Imported from @lib{", UMS,
		    "} (see the corresponding documentation for details)."],
		Text),
	    parse_docstring(DocSt, Text, RText),
	    add_lines(RText, RText1),
	    R = [defpred(local_label(_), Type, PText, F/A, [RText1]), sp("1"), raw_nl]
	).
fmt_definition(P, _, _ModSt, _DocSt, R) :-
	R = [],
	error_message(_, "could not document predicate or new declaration ~w", [P]).

%% ---------------------------------------------------------------------------
%% Abstracted out parts of doc_predicate:

%% Get the assertions that describe usages (defkind_pred type):
%% (do not get decl or modedef assrts; if documenting decl or modedef, 
%% then get only decl or modedef  assrts)
predicate_usages(P, DefKind, M, Usages) :-
	findall(assertion_read(P, M, Status, Type, NAss, Dict, S, LB, LE),
	    ( doc_assertion_read(P, M, Status, Type, NAss, Dict, S, LB, LE),
		( nonvar(DefKind)
		-> ( (DefKind = decl ; DefKind = modedef)
		    -> Type = DefKind
		    ; ((\+ Type = decl), (\+ Type = modedef)) )
		; true ),
		defkind_pred(Type),
		% (findall does internally backtracking, which undoes variable bindings)
		bind_dict_varnames(Dict)
	    ),
	    Usages).

%% Get any comment declarations, compute CommentHead:
predicate_level_comment(F/A, DocSt, CommentR, CommentHead) :-
	functor(CP, F, A),
	( get_doc(pred(F/A), dofail, DocSt, CommentR),
	    CommentHead = F/A
	; get_doc(pred(CP), dofail, DocSt, CommentR),
	    CommentHead = CP
	; CommentHead = F/A, empty_doctree(CommentR)
	).

%% Get any other assertions:
%% (except for decls)
other_assertions(_P, DefKind, _M, []) :-
	DefKind == decl,
	!.
other_assertions(P, _DefKind, M, OtherAssrt) :-
	findall(assertion_read(P, M, Status, Type, NAss, Dict, S, LB, LE),
	    ( doc_assertion_read(P, M, Status, Type, NAss, Dict, S, LB, LE),
		\+ (defkind_pred(Type)),
		bind_dict_varnames(Dict) ),
	    OtherAssrt).

%% ---------------------------------------------------------------------------
:- pred look_for_pred_type(L, P, T) ::
	(list(L), predname(P), defkind_pred_ext(T)) #
"@var{T} is the type of the predicate described by the assertions
   in @var{L} for predicate @var{P}.".

:- regtype defkind_pred_ext/1.
defkind_pred_ext(_).

%% If no explicit type found (e.g., only basic assertions) then assume pred
%% (unless explicitly declared as a new_declaration)
look_for_pred_type([], _, Type) :-
	nonvar(Type),
	!.
look_for_pred_type([], F/A, Type) :-
	var(Type),
	clause_read(_, 1, new_declaration(F/A), _, _, _, _),
	!,
	Type = decl.
look_for_pred_type([], _, Type) :-
	var(Type),
	!,
	Type = pred.
look_for_pred_type(
	    [assertion_read(_P, _M, _S, RType, NAss, _Dict, S, LB, LE)|R], _,
	    Type) :-
	patch_special_prop(RType, NAss, AType), %% Special case for regtypes
	handle_pred_type(AType, R, Type, loc(S, LB, LE)).

handle_pred_type(AType, R, Type, _Loc) :-
	var(Type),
	(defkind_pred(AType) ; special_prop(_, AType)),
	!,
	%% We assume that this is the type.
	Type = AType,
	look_for_pred_type(R, _, Type).
handle_pred_type(AType, R, Type, Loc) :-
	nonvar(Type),
	defkind_pred(AType),
	!,
	%% Must be identical to previously found type.
	( Type == AType -> true
	; warning_message(Loc,
	    "incompatible assertion types ~w and ~w", [Type, AType]),
	  fail
	),
	look_for_pred_type(R, _, Type).
handle_pred_type(_AType, R, Type, _Loc) :-
	%% Else, we continue looking.
	look_for_pred_type(R, _, Type).

%% ---------------------------------------------------------------------------
:- pred doc_native_declarations/5 # "Generates documentation for the
   native declarations, such as @decl{dynamic/1}, @decl{multifile/1},
   etc. Implicit is a special case.".

doc_native_declarations(F/A, _M, Base, _DocSt, NativeR) :-
	( defines(Base, F, A, Type, Meta) ->
	    fmt_native_declaration(normal(Type, Meta), DefR)
	; DefR = []
	),
	( def_multifile(Base, F, A, Mode) -> % (static, dynamic, concurrent)
	    fmt_native_declaration(multifile(Mode), MultiR)
	; MultiR = []
	),
	NativeR = [DefR, MultiR].

fmt_native_declaration(normal(Type, Meta), R) :- !,
	fmt_type_info(Type, TypeR),
	fmt_meta_info(Meta, MetaR),
	R = [TypeR, MetaR].
fmt_native_declaration(multifile(Type), R) :- !,
	MultiR = [raw_nl, string_esc("The predicate is "), em(string_esc("multifile")), string_esc("."),
	          linebreak],
	fmt_type_info(Type, TypeR),
        R = [MultiR, TypeR].
fmt_native_declaration(Mode, R) :-
	fmt_type_info(Mode, R).

%% Static and implicit not interesting to document.
fmt_type_info(static, R) :- !, R = [].
fmt_type_info(implicit, R) :- !, R = [].
fmt_type_info(Type, R) :-
	atom_codes(Type, TypeS),
	R = [raw_nl,
             string_esc("The predicate is of type "),
             em(string_esc(TypeS)),
	     string_esc("."),
	     linebreak].

fmt_meta_info(0, R) :- !, R = [].
fmt_meta_info(Meta, R) :-
	format_to_string("~w", [Meta], MetaS),
	R = [raw_nl,
	     em(string_esc("Meta-predicate")),
	     string_esc(" with arguments: "),
	     tt(string_esc(MetaS)),
	     string_esc("."),
	     linebreak].

%% ---------------------------------------------------------------------------
:- pred doc_other_assertions/6 # "Generates documentation for assertions 
   other than @tt{pred} assertions.".

doc_other_assertions(Usages, OtherAssrt, P, Type, DocSt, R) :-
	( show_other_assrt_header(Usages, OtherAssrt) ->
	    gen_other_assrt_header(R0),
	    R = [R0,Ra]
	; R = [Ra]
	),
	doc_other_assertions_(OtherAssrt, P, Type, DocSt, Ra).

doc_other_assertions_([], _P, _Type, _DocSt, []) :- !.
doc_other_assertions_([Assrt|Assrts], _P, Type, DocSt, [AssrtR|AssrtsR]) :-
	doc_usage(Assrt, _, -1, Type, DocSt, AssrtR),	
	doc_other_assertions_(Assrts, _P, Type, DocSt, AssrtsR).

% Do I need to show the 'other assertions' message?
show_other_assrt_header(Usages, OtherAssrt) :-
	Usages = [_|_], % length > 0
	OtherAssrt = [_|_]. % length > 0

gen_other_assrt_header(R) :-
	R = [raw_nl, bf(string_esc("General properties:")), string_esc(" ")].

%% ---------------------------------------------------------------------------
:- pred doc_usages/5 # "Generates documentation for each @em{usage} of
   a predicate (as declared in a @tt{pred} assertion).".

doc_usages(Usages, F/A, PType, DocSt, UsagesR) :-
	( Usages = [_,_|_] -> % length > 1
	    Multiple = 1
	; Multiple = 0
	),
	doc_usages_(Usages, 1, Multiple, F/A, PType, DocSt, UsagesR).

doc_usages_([], _N, _Multiple, _P, _PType, _DocSt, []) :- !.
doc_usages_([Usage|Usages], N, Multiple, _P, PType, DocSt, [UsageR|UsagesR]) :-
	doc_usage(Usage, N, Multiple, PType, DocSt, UsageR),
	N1 is N+1,
	doc_usages_(Usages, N1, Multiple, _P, PType, DocSt, UsagesR).

%% If no info, then don't document!
doc_usage(Assrt, _N, _Multiple, _PType, _DocSt, UsageR) :-
	Assrt = assertion_read(CP, _M, _Status, _PType, NAss, _, _, _, _),
	assertion_body(_, [], [], [], [], [], NAss),
	CP =.. [_|Args],
	allvars(Args),
	!,
	UsageR = [].
doc_usage(Assrt, N, Multiple, PType, DocSt, UsageR) :-
	Assrt = assertion_read(_P, _M, Status, AType, NAss, _Dict, S, LB, LE),
%	display(a(N, Multiple, PType, Assrt)), nl,
	Loc = loc(S, LB, LE),
	assertion_body(P, DP, CP, AP, GP, CO, NAss),
	fix_var_arg_names(P, Loc, NP),
	( member(iso(_), GP), Multiple \== -1 -> %% Done differently for gen props
	    Standard = iso
	; Standard = non_iso
	),
	( docst_opt(no_isoline, DocSt),
	  select(iso(_), GP, NNGP) ->
	    true
	; NNGP = GP
	),
	( (\+ docst_opt(regtype_props, DocSt)),
	  select(regtype(_), NNGP, NGP)	->
	    true
	; NNGP = NGP
	),
	%
	( CO=[], DP=[], CP=[], AP=[], NGP=[] ->
	    UsageR = [] % No info
	; gen_usage_header(N, Status, AType, Multiple, HeaderStr),
	  % TODO: Extract a descriptive head from the normalized assertion
	  % Documenting a general property or empty usage
	  fmt_head_descriptor(NP, PType, Standard, HeadR),
	  %
	  doc_description(CO, Loc, NP, DocSt, DescR),
	  %% Cond used to see whether calls and comp props are conditional
	  ( CP = [] -> Cond = empty ; Cond = full ),
	  %
	  doc_site(compat, Loc, Cond, DP,  NP, PType,  Status, DocSt, DPR),
	  %
	  doc_site(call,   Loc, Cond, CP,  NP, AType, Status, DocSt, CPR),
	  doc_site(answer, Loc, Cond, AP,  NP, AType, Status, DocSt, APR),
	  %
	  doc_site(global, Loc, Cond, NGP, NP, AType, Status, DocSt, NGPR),
	  UsageR = defassrt(Status, AType, HeaderStr, HeadR, DescR, assrtprops(DPR, CPR, APR, NGPR))
        ).

fmt_head_descriptor(P, PType, Standard, HeadR) :-
	( P=_F/_A ->
	    R1 = []
	; assrt_type_text(PType, _Text, Prefix, Postfix),
	  format_to_string("~w", [P], PS),
	  R1 = [string_esc(Prefix), tt(string_esc(PS)), string_esc(Postfix)]
	),
	fmt_standard(Standard, StandardR),
	( StandardR = [] ->
	    HeadR0 = R1
	; HeadR0 = left_and_right(R1, StandardR)
	),
	( HeadR0 = [] ->
	    HeadR = HeadR0
	; HeadR = [HeadR0, raw_nl]
	).

fmt_standard(iso, R) :- !,
	R = iso("").
fmt_standard(_Standard, []).

gen_usage_header(_N, _Status, test, _Multiple, HeaderStr) :- !,
	% TODO: Probably not right.
	HeaderStr = "Test:".
gen_usage_header(_N, _Status, entry, _Multiple, HeaderStr) :- !,
	% TODO: check.
	HeaderStr = "Module entry condition:".
gen_usage_header(N, check, _AType, Multiple, HeaderStr) :- !,
	( usage_str(N, Multiple, HeaderStr0) ->
	    HeaderStr = HeaderStr0
	; HeaderStr = "Check:" % TODO: Correct?
	).
gen_usage_header(N, Status, _AType, Multiple, HeaderStr) :-
	% TODO: N is not used, is it correct?
	( % Name for other assertions (take from status)
	  % TODO: check at compile time that we cover all cases
          %   in assertions_props:assrt_status/1
          ( Status = true ->  StatusStr = "True"
          ; Status = false -> StatusStr = "False"
%          ; Status = check -> StatusStr = "Check" % TODO: Already treated
          ; Status = checked -> StatusStr = "Checked"
          ; Status = trust -> StatusStr = "Trust"
	  ; throw(error(unknown_assrt_status(Status), gen_usage_header/5))
	  )
	),
	( usage_str(N, Multiple, UsageStr) ->
	    append("("||StatusStr, ") "||UsageStr, HeaderStr)
	; append(StatusStr, ":", HeaderStr)
	).

usage_str(N, Multiple, Str) :-
	( Multiple = 1 ->
	    format_to_string("Usage ~w:", N, Str)
	; Multiple = 0 ->
	    Str = "Usage:"
	; fail % (not an usage)
	).

allvars([]).
allvars([H|T]) :-
	var(H),
	allvars(T).

%% ---------------------------------------------------------------------------
:- use_module(library(assertions/assertions_props), [assrt_type/1]).

:- pred assrt_type_text(PType,Text,Prefix,Postfix) 
	: assrt_type * string * string * string
	# "@var{Text} is an appropriate text for the header for
           @var{PType}.  Same for @var{Prefix} and @var{Postfix}".

assrt_type_text(pred,    "PREDICATE",   "",    "") :- !.
assrt_type_text(compat,  "PREDICATE",   "",    "") :- !.
assrt_type_text(calls,   "PREDICATE",   "",    "") :- !.
assrt_type_text(success, "PREDICATE",   "",    "") :- !.
assrt_type_text(comp,    "PREDICATE",   "",    "") :- !.
%% assrt_type_text(func,    "FUNCTION",    "",    "") :- !.
assrt_type_text(prop,    "PROPERTY",    "",    "") :- !.
assrt_type_text(regtype, "REGTYPE",     "",    "") :- !.
assrt_type_text(decl,    "DECLARATION", ":- ", ".") :- !.
assrt_type_text(modedef, "MODE",        "",    "") :- !.
%% This one just for undocumented reexports:
assrt_type_text(udreexp, "(UNDOC_REEXPORT)",     "",    "") :- !.
assrt_type_text(_,       "UNKNOWN",     "",    "") :- !.

% Text for the assertion type that can be used in paragraphs
% TODO: incomplete
assrt_type_ptext(prop, "property").
assrt_type_ptext(regtype, "regular type").

% The assertion is a property (or some kind of property).
assrt_is_prop(prop).
assrt_is_prop(regtype).

%% ---------------------------------------------------------------------------

:- pred doc_site/9 # "Generates documentation for each program point
   site (@tt{compat}, @tt{call}, @tt{answer}, @tt{global}) of a
   predicate.".

doc_site(_T, _Loc, _Cond, Props, _P, _PType, _Status, _DocSt, R) :-
	Props = [],
	!,
	R = [].
doc_site(T, Loc, Cond, Props, P, PType, Status, DocSt, R) :-
	site_text(T, Cond, PType, Status, Text, Bullet),
	!,
	fmt_site_begin(Text, Bullet, BeginR),
	doc_properties(Props, Loc, P, DocSt, PropsR),
	R = [BeginR, PropsR, raw_nl].
doc_site(T, Loc, _Cond, Props, P, _PType, _Status, _DocSt, R) :-
	R = [],
	warning_message(Loc,
	    "error while formatting ~w properties ~w for predicate ~w",
	    [T, Props, P]).

fmt_site_begin(Text, bullet, BeginR) :-
	BeginR = [item(""), em(string_esc(Text))].
fmt_site_begin(Text, nobullet, BeginR) :-
	BeginR = [linebreak, em(string_esc(Text)), raw_nl].

site_text(compat, _Cond, pred, Status, Text, bullet) :-
	%% Special case for true/trust pred, compat properties:
	( Status = true ; Status = trust ),
	!,
	Text = "Calls should, and exit will be compatible with:".
site_text(compat, _Cond, _PType, Status, Text, bullet) :-
	!,
	status_text_infix(Status, SText),
	list_concat(["Call and exit ", SText, " compatible with:"], Text).
site_text(T, Cond, PType, Status, Text, Bullet) :-
	status_text_prefix(PType, T, Cond, PText, Bullet),
	status_text_mode(Status, PType, T, MText),
	prog_point_text(T, PPText),
	!,
	list_concat([PText, " ", MText, " ", PPText], Text).

%:- use_module(library(format), [format/3]).

status_text_infix(trust,   "are").
status_text_infix(true,    "are").
status_text_infix(false,   "are not").
status_text_infix(check,   "should be").
status_text_infix(checked, "are").

status_text_prefix(modedef, _,      _,
	           "The following properties", bullet) :- !.
status_text_prefix(pred,    _,      _,
	           "The following properties", bullet) :- !.
status_text_prefix(calls,   _,      _,
	           "The following properties", bullet) :- !.
status_text_prefix(entry,    _,      _,
	           "The following properties", bullet) :- !.
status_text_prefix(decl,    _,      _,
	           "The following properties", bullet) :- !.
% 'call' site
status_text_prefix(_,       call,   _,
	           "If the following properties", bullet).
% 'answer' site
status_text_prefix(_,       answer, full,
	           "then the following properties", nobullet).
status_text_prefix(_,       answer, empty,
	           "The following properties", bullet).
% 'global' site
status_text_prefix(_,       global, full,
	           "then the following properties", nobullet).
status_text_prefix(_,       global, empty,
	           "The following properties", bullet).

%% Introduced special case for guard
status_text_mode(_, modedef, _,    "are added") :- !.
status_text_mode(_, success, call, "hold") :- !.
status_text_mode(_, test, call, "hold") :- !.
status_text_mode(_, comp,    call, "hold") :- !.
%% Introduced special case for true/trust pred.
status_text_mode(trust,   pred, call, "should hold") :- !.
status_text_mode(trust,   _,    _,    "hold").
status_text_mode(true,    pred, call, "should hold") :- !.
status_text_mode(true,    _,    _,    "hold").
% TODO: This 'do not hold' should not be applied in the condition (i.e. in the call?) (not (p -> q)) == (p -> not q)
%       (JFMC)
status_text_mode(false,   _,    _,    "do not hold").
status_text_mode(check,   _,    _,    "should hold").
status_text_mode(checked, _,    _,    "are proved to hold").

prog_point_text(call,   "at call time:").
prog_point_text(answer, "upon exit:").
prog_point_text(global, "globally:").

%% ---------------------------------------------------------------------------
:- pred doc_properties/5
# "Generates documentation for a conjunction (list) of properties.".

doc_properties([], _Loc, _P, _DocSt, []) :- !.
doc_properties([Prop|Props], Loc, P, DocSt, [PropR|PropsR]) :- !,
	doc_property(Prop, Loc, P, DocSt, PropR),
	doc_properties(Props, Loc, P, DocSt, PropsR).
doc_properties(Prop, Loc, P, DocSt, PropR) :-
	doc_property(Prop, Loc, P, DocSt, PropR).

doc_property(true, _Loc, _P, _DocSt, PropR) :- !,
	PropR = nop.
doc_property(Prop, Loc, P, DocSt, PropR) :-
	( prop_format(DocSt, Prop, Loc, PM, BasicFormat, VarDict) ->
	    ( docst_opt(literal_props, DocSt) ->
	        empty_doctree(DocR)
	    ; DocR = BasicFormat
	    ),
	    ( PM = user(FullPath),
	      path_basename(FullPath, UFName0),
	      path_splitext(UFName0, UFName, _) ->
	        NPM = user('...'/UFName)
	    ; NPM = PM
	    )
	; NPM = undefined,
	  DocR = undefined,
	  VarDict = [],
	  warning_message(Loc, "unknown property ~w in assertion for ~w", [Prop, P]),
	  ttyflush
	),
	fmt_property(DocSt, Prop, NPM, DocR, VarDict, PropR).

%% -literalprops -nopropnames -noundefined -nopropsepln

fmt_property(DocSt, Prop, PM, DocString, VarDict, PropR) :-
	( ( docst_opt(literal_props, DocSt)
	  ; doctree_is_empty(DocString)
	  ; DocString == undefined
	  ) ->
	    fmt_propcode(PM, Prop, DocSt, PropLitR)
	; fill_vardict(VarDict),
	  PropLitR = DocString
	),
	( ( DocString\==undefined
	  ; docst_opt(no_undefined, DocSt)
	  ) ->
	    UndefR = []
	; UndefR = string_esc(" (undefined property)")
	),
	( ( DocString==undefined
	  ; docst_opt(no_propnames, DocSt)
	  ; docst_opt(literal_props, DocSt)
	  ) ->
	    DescR = []
	; functor(Prop, F, A),
	  Desc = F/A,
	  fmt_propcode(PM, Desc, DocSt, Desc2),
	  DescR = [string_esc(" ("), Desc2, string_esc(")")]
	),
	!,
	( docst_opt(no_propsepln, DocSt) ->
	    PropR = [string_esc(" "), PropLitR, UndefR, DescR, string_esc(".")]
	; PropR = [linebreak, left_and_right([PropLitR, raw_nl], [UndefR, DescR])]
	).
fmt_property(_DocSt, Prop, PM, _DocString, _VarDict, PropR) :-
	PropR = [],
	error_message("could not format property ~w:~w", [PM, Prop]).	

fmt_propcode(PM, Prop, DocSt, R) :-
	% TODO: the module is ignored for indexing; wrong
	format_to_string("~w", [Prop], Ref),
	( docst_opt(propmods, DocSt) ->
	    format_to_string("~w:~w", [PM, Prop], S)
	; S = Ref
	),
	( docst_opt(no_propuses, DocSt) ->
	    R = [string_esc(S)]
	; R = [idx_env(use, prop, localnum_label(_), string_esc(Ref), string_esc(S))]
	).

fill_vardict([]).
fill_vardict([X=V|Ds]) :-
	format_to_string("~w", [V], S),
	X = var([string_esc(S)]),
	fill_vardict(Ds).

%% ---------------------------------------------------------------------------
:- pred doc_description/5
# "Generates documentation for a predicate or prop description.".

doc_description(Desc, _Loc, P, _DocSt, DescR) :-
	Desc = [],
	( P = F/A -> true
	; functor(P, F, A)
	),
	!,
	DescR = [].
%%	note_message("no comment found for usage in ~w/~w",[F,A]).
doc_description(Desc, Loc, _P, DocSt, DescR) :-
	parse_docstring_loc(DocSt, Loc, Desc, DescR).

%% ---------------------------------------------------------------------------
:- pred prop_format(DocSt, Prop, Loc, PM, BasicFormat, VarDict)

# "Given a property @var{Prop} (which appears in an assertion), a
   string is generated which expresses in words the meaning of the
   property. In order to be able to do this, a documentation string
   must be provided (in a standard declaration) in the place where the
   property itself is defined and documented. Such property
   definitions may be in the same module or exported by any module
   visible -- through @pred{use_module/1} -- to the module being
   documented.  Some complication comes from the fact that the
   documentation should be generated in terms of the variables
   appearing in @var{Prop}, rather than the ones in the original
   definition. The output is @var{BasicFormat} (containing free
   variables in the places where the variables names should appear)
   and a list of pairs @var{VarDict} relating the free variables with
   the (possibly repeated) variable names. @var{PM} is the module in
   which the property is defined.".

prop_format(DocSt, Prop, Loc, PM, BasicFormat, VarDict) :-
	nonvar(Prop),
	% Get assertion
	doc_assertion_read(Prop, PM, _PStatus, PType, NAss, PDict, _, _, _),
	defkind_prop(PType), %% prop, ...
	%% Should add also ~imports(M,AM,F,A), but, since this is flagged 
	%% during normalization, here we use whatever we can find.
	% Get comment field
	assertion_body(Prop, _DP, _CP, _AP, _GP, Comment, NAss),
	% Rewrite the comment
	maybe_remove_full_stop(DocSt, Comment, Comment2),
	parse_docstring_loc(DocSt, Loc, Comment2, Comment3),
	doctree_putvars(Comment3, DocSt, PDict, VarDict, BasicFormat).

maybe_remove_full_stop(DocSt, DocString, DocString2) :-
	( ( docst_opt(no_propsepln, DocSt),
	    list_concat([NewDocString, "."], DocString)
	  ) -> %% Just to make it nicer: get rid of final full stop.
	    DocString2 = NewDocString
	; DocString2 = DocString
	).

%% ---------------------------------------------------------------------------

:- use_module(lpdoc(autodoc_aux), [all_vars/1]).

:- pred fix_var_arg_names(H, Loc, NH)
 # "In both @var{NH} and @var{H} the arguments of @var{H} which are
   vars are replaced with the name of their argument position, i.e.,
   @tt{fix_var_arg_names(p(X,a),p('Arg1',a)}. However, if all
   arguments of @var{H} are originally free variables, then @var{NH}
   is of the form @tt{F/A}, where @tt{F} is the principal functor of
   @var{H} and @tt{A} its arity. An exception to this occurs when
   there is a @pred{doc/2} declaration whose first argument is a
   predicate descriptor specifying argument names: these names are
   used in this case instead of 'ArgN'.".

fix_var_arg_names(H, _Loc, NH) :-
	functor(H, F, A),
	get_doc_pred_varnames(F/A, CArgs),
	H =.. [_|Args],
	( all_vars(Args) -> NH=F/A ; NH=H ),
	fix_var_arg_names_(Args, CArgs).

fix_var_arg_names_([], []) :- !.
fix_var_arg_names_([Arg|Args], [CArg|CArgs]) :-
	( var(Arg) ->
	    Arg = CArg
	; true
        ),
	fix_var_arg_names_(Args, CArgs).

% ---------------------------------------------------------------------------

% Format the input list of doctree as a comma-separated, dot-ended
% doctree.
fmt_commas_period([], []) :- !.
fmt_commas_period([A], [R]) :- !,
	R = [A, string_esc(".")].
fmt_commas_period([A|As], [R|Rs]) :-
	R = [A, string_esc(", ")],
	fmt_commas_period(As, Rs).

% ===========================================================================

:- doc(section, "Compute Global References for a Module").
% TODO: This could be optimized! It is the same for every module (profile it!)

:- export(autodoc_compute_grefs/3).
:- pred autodoc_compute_grefs/3 # "Compute the globally resolved
references (including bibliography)".
% (do not call for components)
autodoc_compute_grefs(Backend, Mod, Opts) :-
	verbose_message("{Globally resolving references", []),
	docst_new_no_src(Backend, Mod, Opts, DocSt),
	compute_refs_and_biblio(DocSt),
	verbose_message("}", []).

% ===========================================================================

:- doc(section, "doctree Translation and Output").

:- export(autodoc_translate_doctree/3).
:- pred autodoc_translate_doctree/3 # "Translate the doctree using the specific
backend".
autodoc_translate_doctree(Backend, Opts, Mod) :-
	verbose_message("{(Backend ~w) Translating intermediate representation of ~w", [Backend, Mod]),
	docst_new_no_src(Backend, Mod, Opts, DocSt),
	absfile_for_subtarget(Mod, Backend, cr, OutFile),
	doctree_restore_and_write(Mod, OutFile, DocSt),
	% TODO: This generates the infoindex if necessary; generalize for other formats
	( docst_currmod_is_main(DocSt),
	  Backend = texinfo ->
	    fmt_infodir_entry2(DocSt, Mod)
	; true
	),
	verbose_message("}", []).

% ---------------------------------------------------------------------------

%:- export(fmt_infodir_entry2/2).
fmt_infodir_entry2(DocSt, Mod) :-
	% TODO: Why don't we write the file directly?
	% Write to a '*dir.info' file
	% (this is later renamed to .infoindex; cannot use infoindex
        %  here because Mod is the same as for the main file,
        %  which makes doctree_to_file overwrite some important
        %  intermediate files)
	infodir_base(Mod, ModInfodir),
	%
	docst_backend(DocSt, Backend),
	absfile_for_subtarget(ModInfodir, Backend, cr, OutFile),
	%
	docst_set_currmod(DocSt, ModInfodir, DocSt1),
	doctree_restore_and_write_norefs(ModInfodir, OutFile, DocSt1).

% ---------------------------------------------------------------------------

doctree_restore_and_write(Mod, OutFile, DocSt) :-
	% Now that (global) references are computed, restore the
	% doctree, references, and translate.
	docst_backend(DocSt, Backend),
	absfile_for_subtarget(Mod, Backend, dr, RFile),
	doctree_restore(RFile, R),
	try_finally(open(OutFile, write, OS),
	            doctree_prepare_docst_translate_and_write(R, DocSt, OS),
		    close(OS)).

% TODO: For infoindex generation. Is it worth a special case?
doctree_restore_and_write_norefs(Mod, OutFile, DocSt) :-
	docst_backend(DocSt, Backend),
	absfile_for_subtarget(Mod, Backend, dr, RFile),
	doctree_restore(RFile, R),
	try_finally(open(OutFile, write, OS),
	            doctree_translate_and_write(R, DocSt, OS),
		    close(OS)).

% ===========================================================================

:- doc(section, "Finish Document and Generate Alternative Output").

:- multifile autodoc_finish_hook/1.

:- export(autodoc_finish/1).
autodoc_finish(Backend) :-
	autodoc_finish_hook(Backend).

:- multifile autodoc_gen_alternative_hook/2.

:- export(autodoc_gen_alternative/2).
autodoc_gen_alternative(Backend, Alt) :-
	autodoc_gen_alternative_hook(Backend, Alt).

% ===========================================================================

:- doc(section, "Auxiliar Definitions").

:- pred eliminate_duplicates(X,Y) # "@var{Y} is @var{X} where
   duplicated elements has been removed".

eliminate_duplicates(X, Y) :-
	eliminate_duplicates_(X, [], Y).

eliminate_duplicates_([],    _,    []).
eliminate_duplicates_([H|T], Seen, NT) :-
	member(H, Seen),
	!,
	eliminate_duplicates_(T, Seen, NT).
eliminate_duplicates_([H|T], Seen, [H|NT]) :-
	eliminate_duplicates_(T, [H|Seen], NT).

% ===========================================================================

:- doc(section, "Tasks and Issues (private)").

:- doc(subsection, "Future Work").

:- doc(bug, "Currently, the input from autodoc is a
   SETTINGS.pl. Currently, this is quite limited since there can exist
   only one SETTINGS.pl loaded at a time, and it interact with other
   tools (e.g., lpmake)").

:- doc(bug, "in Ciao tree we need to automatically create local
   subset of (clip) bibtex files to make manuals standalone.").

:- doc(bug, "documentation of exceptions.").

:- doc(bug, "Assertions should describe which database predicates are read
   and written by a given predicate.").

:- doc(bug, "add links from types/props to their definition in all
   formats (info, pdf, etc.).  This would be a mess in info, but in
   info it is not necessary: you can go with C-c tab or search... ").

:- doc(bug, "Local options in file -- something like :-
   doc(options,...).").
:- doc(bug, "List of local opts: :- doc(localopts,[no_changelog]).").

:- doc(bug, "Add a way to disable usage section?").

:- doc(bug, "Document implementation defined?").
:- doc(bug, "How about using :- doc(+/2,""), where + is an
   operator (?) ").

:- doc(bug, "optimize assertion reading (in assrt_lib) of frequently
   used modules (engine/libraries) -- evaluate if there is a
   bottleneck here").

:- doc(bug, "In usage text, hide (optionally, perhaps per package or
   module) the modules that are implicitly imported by some package. E.g.,

   :- use_module(library(iso_char)).
 
   (imported by default) or:

   :- use_module(library(dcg/dcg_tr)).

   imported in DCG.
").

:- doc(bug, "add a @@flag@{Flag@} command to name Prolog flags
   (interpret the @pred{define_flag/3} multifile to document flags
   defined in modules)").

:- doc(bug, "generalize @@iso command (for other labels other than ISO)").

:- doc(bug, "add declarations so that, e.g., @tt{data_facts} can
   document @tt{:- data}.").

:- doc(subsection, "Known Bugs").

:- doc(bug, "Make sure that richer paths are valid in @@image
   command").

:- doc(subsection, "Older or Unclassified Bugs (need review)").

:- doc(bug, "single-sided versus double-sided").

:- doc(bug, "Document classes: see mess from Angel").

:- doc(bug, "lpdoc should sign its manuals").

:- doc(bug, "Reexported global predicate + assertions: only the
   assertions are documented...").

:- doc(bug, "Find a way that one can specify that a given assertion
   should not appear in the documentation (nodoc/1 a global
   property?). Also, the other way around.").

:- doc(bug, "Apparently parts cannot be referenced").

% old bug? fixed?
:- doc(bug, "Make sure that comp properties (those after +) are
   correctly shown in documentation (those have an implicit
   argument)").

% old bug? fixed?
:- doc(bug, "Make that type checking do not complain about predefined
   type definitions (term, int, ...) in the engine (with a
   declaration?)").

:- doc(bug, "customization for pretty printing in lpdoc? (e.g., to
   define operators so that modes look nice) -- needed now?").

:- doc(bug, "do not capitalize index titles? (E.g., ""Library index"",
   etc.) is it possible?").

:- doc(bug, "bugs: uses without any field (with with modes) do not
   appear, e.g., in @lib{engine(atomic_basic)} or
   @lib{engine(io_basic)} do not appear (e.g., usage of
   @pred{nl/0}). (fixed?)").

:- doc(bug, "if an assertion is present for p, even if the predicate
   is reexported, it should be documented (no need for doinclude). See
   sockets.pl").

:- doc(bug, "if an imported type is redefined locally and then
   listed, all clauses appear").

:- doc(bug, "underscores in file names result in problems when
   including figures (a texinfo bug). Possible fix:

   \newcommand@{\fichero@}[1]@{\catcode\`\~=11%
            \catcode\`\_=11%
            \emph@{#1@}@}

   ").

:- doc(bug, "putting props in a prop should give errors?").

:- doc(bug, "Is this a bug or a wrong assertion (system.pl)? (old problem, fixed?)

   :- true pred umask(OldMask, NewMask)
        : (var(OldMask), var(NewMask), OldMask == NewMask)
       $=>$ (int(OldMask), int(NewMask))
        # ""Gets the process file creation mask without changing it."".

   @{WARNING (autodoc): unknown property int(OldMask) in assertion for 
   umask(OldMask,NewMask)@} 

   It does not work either with @tt{int * int}.

   ").

:- doc(bug, "Check that @@pred etc. state arity!").

:- doc(bug, "Warning: '@@verbatim' is deprecated, use '@@pre' instead.
   In the new latex it is 'alltt'.").

:- doc(bug, "if an imported predicate is redefined, the local
   version should always be the one documented!!!!").

:- doc(bug, "ops and decls from assertions should not be documented").

:- doc(bug, "we need macros").

:- doc(bug, "(using conditional compilation?) Conditional inclusion,
   in order to make several types of manuals from a single file, e.g.,
   :- doc(doinclude(refmanual),p/3) and 'refmanual' is an option
   passed to lpdoc (in SETTINGS).  ").

:- doc(bug, "Should support texinfo @@dircategory and the
   install-info method.").

:- doc(bug, "local properties (not exported) used in a predicate
   which is exported: documented correctly, but an error message is
   issued. CHECK!").

:- doc(bug, "Documentation for main file should produce *global*
   changelog.").

:- doc(bug, "Arithmetic vs. term typing (but only if -nomodes)").

:- doc(bug, "The '@@@{' and '@@@}' characters must be eliminated from
   the files generated by bibtex. Also check out what is best:
   '@@dotless@{i@}' or '@@dotless i'.").

%%% 1. Relatively easy and nice (i.e., good for having a good time):

:- doc(bug, "when including support.pl in bibutils doc,
   line_count/2 fails instead of aborting... CHECK").

:- doc(bug, "Add commands to change chapter number?").

:- doc(bug, "'.' still appears sometimes as first character of a
   line in man format: problematic!").

:- doc(bug, "Predicate whose name is composed by symbols (e.g.,
   (=:=)/2), should appear in the index as '=:= /2' (name separted
   from '/'). If not, it is parsed in a different way in Prolog (that
   is how appears in SICStus).").

:- doc(bug, "Check out haskell_doc.el").

%%% 2. More complicated (i.e., real work):

:- doc(bug, "Almost all error messages should give line numbers...").

:- doc(bug, "Add 'INCLUDES' to settings: creates dependencies from
   included files. But complex: has to be done for each
   component... => lpdoc generates a file listing 
   the includes!...").

:- doc(bug, "Module usages should say: 'assertions/assrt_lib', but this 
   is too hard to do until we get rid of the Makefile.").

:- doc(bug, "Include a directory 'ciaolib', in which, every now and
   then (on demand?, when compiling?, when installing?) the ciao
   libraries used by lpdoc are copied (this makes it
   standalone). This can now be done quite well with the new
   distribution method. Also, do autoload depending on format?").

:- doc(bug, "Should have @@includedoc@{predname/arity@} command,
   which includes inline the documentation of a predicate. Also, a
   command to prevent the automatic generation of the standard
   sections with exported predicates, etc., so that it can all be done
   by hand.").

:- doc(bug, "Disjunctions in properties not supported yet.").

%%% 3. Not really easy to fix (e.g., it is a bug in another tool):

:- doc(bug, "in basic_props, properties appear twice -- why?
   Because it includes itself!!! Discuss with Daniel...").

:- doc(bug, "Resulting info files (on-line versions) are still not
   very good regarding references (but not so easy to fix, because of
   limitations of info).").

:- doc(bug, "@@includedef should do a real verbatim... (pity that
   texinfo is so weird with this...").

:- doc(bug, "Idea: for each <module>.pl, produce a <module>_help.pl
   which extends a multifile predicate help/2 which defined help text
   for each predicate and module. <module>_help.pl would be loaded in
   the top level, but not in the executables. BUT This is covered
   however nicely now by word-help, and would slow down loading in the
   top level.").

