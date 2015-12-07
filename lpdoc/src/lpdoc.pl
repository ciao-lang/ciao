:- module(lpdoc, [make_doc/4], [assertions, regtypes, dcg, basicmodes, make, fsyntax]).

:- doc(title, "The lpdoc Documentation Generator").
:- doc(subtitle, "An Automatic Documentation Generator for (C)LP Systems").

:- doc(logo, 'lpdoc-logo-128').

:- doc(subtitle_extra, "REFERENCE MANUAL").
:- doc(subtitle_extra, "@bf{The Ciao Documentation Series}").
:- doc(subtitle_extra, "@href{http://ciao-lang.org/}").
:- doc(subtitle_extra, "@em{Generated/Printed on:} @today{}").
:- doc(subtitle_extra, "Technical Report CLIP 5/97.1-@version{}").

% TODO: Replace 'credits' by 'editor'? (JFMC)
% TODO: In this case, the people here are also the authors
:- doc(credits, "@bf{Edited by:}").
:- doc(credits, "Manuel Hermenegildo").
:- doc(credits, "Jos@'{e} Francisco Morales").

% :- include(ciao_docsrc(common/'ClipAddress')).

:- doc(copyright, "Copyright @copyright{} 1996-2015 Manuel
Hermenegildo and Jos@'{e} Francisco Morales.

@include{DocCopyright.lpdoc}
").

:- doc(summary, "@include{README_LPDOC.lpdoc}").

:- doc(module, "@include{Intro.lpdoc}

@section{lpdoc usage}
The following provides the different command line options available
when invoking @apl{lpdoc}. This description is intended only for
advanced users which might like to use @apl{lpdoc} in custom
applications. Note that the normal way to use @apl{lpdoc} is by
setting parameters in an @file{SETTINGS} file (see @ref{Generating a
manual}).

@begin{alert}
TODO: command line options not available here; need
cooperation with lpmake
@end{alert}
").

% @begin{verbatim}
% @includefact{usage_message/1}
% @end{verbatim}

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

:- doc(section, "Processing Documentation Targets").

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

:- doc(version_maintenance,dir('../Manifest')).

:- doc(version(3*0+0,2011/07/07,16:33*15+'CEST'), "
   @begin{itemize}
   @item Major redesign of the documentation generator:
         @begin{itemize}
         @item LPdoc redesigned to work internally with a 'doctree'
               representation (a-la Pillow). (Jose Morales)
         @item A native HTML backend (not generated from texi). (Jose
               Morales)
         @item Allow custom website generation from LPdoc documents.
               (Jose Morales)
         @item Two passes for document generation, allowing resolution
               of bibliographical references in all backends
               (including HTML). (Jose Morales)
         @item @tt{doc_structure/1} in @tt{SETTINGS} allows structure
               in LPdoc documents (sections can really be nested
               inside parts). (Jose Morales)
         @item @tt{:- doc(_,_)} is the recommended syntax for
               documentation comments now.
         @item Replacing @tt{:- comment} by @tt{:- doc} in LPdoc code,
               updated documentation. (Jose Morales)
         @end{itemize}

   @item General improvements and bug fixes:
         @begin{itemize}
         @item Designed a logo for LPdoc. (Jose Morales)
         @item LPdoc comments can now be written using @tt{%!}
               style comment syntax. (Manuel Hermenegildo)
         @item Now commas etc. are allowed in section names (so that
               they can be used in other formats). They are eliminated
               automatically in texi and info. This avoids wrong
               section names --and thus dangling pointers-- in
               generated texinfo files. (Manuel Hermenegildo)
         @item Eliminated superfluous copy of summary in info mode.
               (Manuel Hermenegildo)
         @item Eliminated unsupported chars that broke texi manual
               cross-referencing. (Manuel Hermenegildo)
         @item Improved treatment of accents (dotless i and dotless j,
               \o, etc.). (Manuel Hermenegildo)
         @item Initial size passed to @tt{xdvi} more appropriate for
               current @tt{xdvis}. (Manuel Hermenegildo)
         @item Accents in bibliography fixed.  (Manuel Hermenegildo)
         @item Now repeated sections are disambiguated. (Manuel
               Hermenegildo)
         @item Eliminated unnecessary escaping (especially for
               &). (Manuel Hermenegildo)
         @item Better detection of when version is not
               available. (Manuel Hermenegildo)
         @item Added new @tt{doc(address, _)} comment, which is the
               right place to put address/contact information in
               manuals (Jose Morales)
         @item Added new @tt{@@version@{@}} command (expands to the
               version of the software to be documented). (Jose
               Morales)
         @item Shorter @tt{SETTINGS.pl} files (with some rudimentary,
               assertion-based checking of options) (Jose Morales)
         @item Bug fix: '@tt{@@@@ include}' and '@tt{@@@@
               includeverbatim}' are no longer a problem (space can be
               omitted) (Jose Morales)
         @item Added and documented a new @tt{documentation} filetype
               (for some parts of the manual that contains only
               documentation). That avoids the old trick of declaring
               a fake @tt{main/0} predicate. (Jose Morales)
         @item Style for subtitle added automatically (in texinfo, it
               is @em{emph}; in HTML it is normal text with smaller
               font). The entries in @tt{subtitle_extra} are
               free-form. (Jose Morales)
         @item Bugs and changelog appear now in the global links in
               the HTML backend. (Jose Morales)
         @item Merged code that documented @tt{.pl} and @tt{.lpdoc}
               files. (Jose Morales)
         @item No copyright section if no copyright comment. (Jose Morales)
         @item Auxiliary documentation files ending in '@tt{_doc}'
               displayed incorrect names for the module (ending in
               '@tt{_doc}'). E.g., @tt{use_package(foo_doc)} was
               displayed instead of
               @tt{use_package(foo_doc)}. Fixed. (Jose Morales)
         @item In @tt{verbatim} enviroments, new-line characters are
               removed from the beginning. (Jose Morales)
         @item Fix wrong use of @tt{erase/1} for clauses (which
               resulted in segmentation fault when documentation
               generation failed) (Jose Morales)
         @item Fixed image generation (now uses @tt{.png} files for
               HTML) (Jose Morales)
         @item New code for text escape fixed some problems, like
               '@tt{@@/1}' operator not being displayed corretly in
               Info. (Jose Morales)
         @item Colors for Prolog variables (in HTML). (Jose Morales)
         @item Added @tt{@@begin@{alert@}} environment for alert
               messages (like cartouche, but in red). (Jose Morales)
         @item Supporting '@@\"' command for umlaut, in addition to
               '@@..'  (Jose Morales)
         @item Double quotes correctly translated to HTML (Jose Morales)
         @item @tt{@@author} command to reference authors (changed
               @index command referring to people by @tt{@@author}, in
               all the documentation) (Jose Morales)
         @item Simplification of documentation setting files (see the
               documentation for further details) (Jose Morales)
         @item Using @tt{open} for @tt{lpdoc htmlview} command in
               MacOS X (Jose Morales)
         @item Adding @tt{html} and @tt{pdf} formats as options for
               emacs customization of LPdoc (@tt{html} is the default
               one now) (Jose Morales)
         @item Improved detection of external tools for image
               conversion. (Manuel Hermenegildo)
         @item Added section name syntax auto-correction. This avoids
               wrong section names --and thus dangling pointers-- in
               generated texinfo files. (Manuel Hermenegildo)
         @item Document size more appropriate for current xdvi
               versions. (Manuel Hermenegildo)
         @item Lpdoc no longer adds .info filename suffix to
               .infoindex entries since it breaks Debian's
               install-info --remove and goes against standard
               practice anyway. (Jose Luis Gonzalez)
         @item Added option --cv, --comment-version, that tells lpdoc
               if the file has version comment. Formatting of lpdoc
               version comments completed. (Edison Mera)
         @item Improved handling of option values. Added -d option to
               lpdoc, that allows defining additional values in the
               argument. Added options -l and -m that are similar to
               the corresponding lpmake options.  (Edison Mera)
         @end{itemize}

   @item Support for in-code sections (experimental):
         @begin{itemize}
         @item Latex-like font-lock highlight of sectioning
               documentation comments (@tt{:- doc(C, \"...\")}, with
               @tt{C} one of @tt{title}, @tt{section}, and
               @tt{subsection}).

               Currently the @tt{section} and @tt{subsection}
               comments are still ignored by LPdoc.  (Jose Morales)
         @end{itemize}

   @item Support for mathematical notation (experimental):
         @begin{itemize}
         @item new @@math@{...@} and
               @@begin@{displaymath@}...@@end@{displaymath@}
               enviroments are supported (see the documentation for
               more details) (Jose Morales)
         @item In documentation strings, single @tt{\\} must be
               escaped (e.g. '@@math@{\\\\lambda@}') (Jose Morales)
         @item Supported in both the texinfo and HTML (using MathJax)
               backends. (Jose Morales)
         @item Added @tt{@@defmathcmd@{Cmd@}@{N@}@{Def@}} and
               @tt{@@defmathcmd@{Cmd@}@{Def@}}, both for texinfo and
               HTML backends. Those LPdoc commands define new
               mathematical environments (equivalent to
               @tt{\\newcommand}). (Jose Morales)
         @end{itemize}
   @end{itemize}

").

:- doc(version(2*1+0,2004/10/28,16:38*17+'CEST'), "

   Last version before moving to subversion. 1.9 and 2.0 were
   merged. 1.9 (based on makefiles) is deprecated.

   @begin{itemize}
   @item New functionality:
   	 @begin{itemize}

         @item Use of @tt{:- doc} declarations (as a shorthand for
               @tt{comment}) now allowed.  (Manuel Hermenegildo)

         @item Made xdvi viewer, ps viewer, and xdvi zoom size be
               paramenters (the latter since new versions of xdvi
               display sizes differently than old ones).  (Manuel
               Hermenegildo)

         @item Processing options can now be set for each file
               independently.  (Manuel Hermenegildo)

         @item Proper @concept{pdf generation} now achieved in most
               cases, thanks to newer versions of @apl{dvips}.
               (Manuel Hermenegildo)

         @item Added option -c Target in lpdoc, that treats Target as
               a separate component.  (Edison Mera) @item Added option
               -f ConfigFile in lpdoc, that uses the file ConfigFile
               instead the default LPSETTINGS.pl.  (Edison Mera)

         @item Added option ascii that generates documentation in
               ascii plain format.  (Edison Mera)

         @item Added --help option. Is equal to -h.  (Edison Mera)

         @item Added option testsettings to check that the settings
               file is correctly specified.  (Edison Mera)

         @item Changed @pred{generate_html_pointer/5} by
               @pred{generate_html_pointer/6} to let it work with any
               given directory, and not only the working directory.
               (Edison Mera)

   	 @end{itemize}

   @end{itemize}

   ").

:- doc(version(2*0+36,2004/10/28,16:38*17+'CEST'), "Updated to the
   recent changes of ciao, related to make package.  Use of package
   make_new is no longer required.  (Edison Mera)").

:- doc(version(2*0+35,2004/10/28,16:37*02+'CEST'), "Moved install
   options to the file installmkf.pl in ciao.  (Edison Mera)").

:- doc(version(2*0+33,2004/10/10,12:31*10+'CEST'), "Added option
   testsettings.  It is to check that the settings file are correctly
   specified.  (Edison Mera)").

:- doc(version(2*0+30,2004/10/08,23:35*24+'CEST'), "Solved a bug
   in option texclean to let it work even if basemain is not
   defined.  (Edison Mera)").

:- doc(version(2*0+29,2004/10/08,23:34*59+'CEST'), "Added option
   ascii that generates documentation in ascii plain format.  (Edison
   Mera)").

:- doc(version(2*0+28,2004/10/08,23:33*31+'CEST'), "Changed
   @pred{generate_html_pointer/5} by @pred{generate_html_pointer/6}
   to let it work with any given directory, and not only the
   working directory.  (Edison Mera)").

:- doc(version(2*0+27,2004/10/08,23:25*53+'CEST'), "
   @pred{basemain/1} changed with @pred{_:basemain/1}.
   @pred{_:startpage(StartPage)} changed with
   @pred{get_value(_:startpage, StartPage)}.
   @pred{_:papertype(PaperType)} changed with
   @pred{get_value(_:papertype, PaperType)}.
   @pred{_:htmlindex_headfile(HH)} changed with
   @pred{get_value(_:htmlindex_headfile, HH)}.
   @pred{_:htmlindex_tailfile(HT)} changed with
   @pred{get_value(_:htmlindex_tailfile, HT)}.  @pred{libdir/1}
   changed with @pred{_:libdir/1}.  @pred{make_directory/1} changed
   with @pred{make_dirpath/1}.  @pred{docdir/1} changed by
   @pred{_:docdir/1}.  @pred{delete_files/1} changed with
   @pred{del_files_nofail/1}. (Edison Mera)").

:- doc(version(2*0+26,2004/10/08,23:25*20+'CEST'), "Added option
   -c Target in lpdoc, that treats Target as a separate component.
   (Edison Mera)").

:- doc(version(2*0+25,2004/10/08,23:25*01+'CEST'),
   "use_module(library(TheAliasFile)) changed with
   use_module(TheAliasFile).  (Edison Mera)").

:- doc(version(2*0+24,2004/10/08,23:24*32+'CEST'), "Added option
   -f ConfigFile in lpdoc, that uses the file ConfigFile instead the
   default LPSETTINGS.pl.  (Edison Mera)").

:- doc(version(2*0+23,2004/10/08,23:23*32+'CEST'), "When importing
   @pred{'../LPDOCSETTINGS'} now uses @pred{use_module/2} to view the
   default configuration values.  (Edison Mera)").

:- doc(version(2*0+16,2004/02/05,19:53*13+'CET'), "Added --help
   option. Is equal to -h.  (Edison Mera)").

:- doc(version(2*0+10,2002/12/12,19:42*52+'CET'), "Proper
   @concept{pdf generation} now achieved in most cases, thanks to
   newer versions of @apl{dvips}.  (Manuel Hermenegildo)").

:- doc(version(2*0+8,2001/11/28,08:36*14+'CET'), "Use of @tt{:-
   doc} declarations (as a shorthand for @tt{comment}) now allowed.
   (Manuel Hermenegildo)").

:- doc(version(2*0+7,2001/11/27,22:41*53+'CET'), "Made xdvi
   viewer, ps viewer, and xdvi zoom size be paramenters (the latter
   since new versions of xdvi display sizes differently than old
   ones).  (Manuel Hermenegildo)").

:- doc(version(2*0+6,2001/11/27,19:28*33+'CET'), "Fixed a few
   lingering CIAO to Ciao in documentation.  (Manuel Hermenegildo)").

:- doc(version(2*0+3,1999/09/15,18:34*45+'MEST'), "Processing
   options can now be set for each file independently.  (Manuel
   Hermenegildo)").

:- doc(version(2*0+0,1999/08/17,17:28*52+'CEST'), "Major change to
   eliminate need for Makefiles: lpdoc is now a standalone command
   (Manuel Hermenegildo). Proceeds in parallel with further
   development of 1.9. Merge pending. Previous changes incorporated
   since 1.8:

   @begin{itemize}
   @item New functionality:
   	 @begin{itemize}

         @item A new parameter @tt{PAPERTYPE} can be set in the
               @file{SETTINGS} file which controls the format of
               printed output.  (Manuel Hermenegildo)

         @item Default @concept{pdf viewer} is now @apl{ghostview},
               sicne recent versions handle @tt{pdf} well.  (Manuel
               Hermenegildo) 

         @item Changed default style sheet in order to show <PRE>
               lines with a monospaced font.  (Daniel Cabeza Gras)

         @item Mode definitions now documented in a separate
               section. The way they are documented has been improved.
               (Manuel Hermenegildo)

         @item References in files now updated only if @tt{.refs} file
               is not empty.  (Manuel Hermenegildo)

         @item A @em{copy} of the html style sheet is now included in
               @em{distributions}. Also @em{Copies} of the html and
               info index head and tail files. (Manuel
               Hermenegildo)

         @item Made pointers relative in library html templates.
               (Manuel Hermenegildo)

   	 @end{itemize}

   @item Bug fixes and other minor improvements:
   	 @begin{itemize}

         @item Declarations now documented properly even if they have
               the same name and arity as a predicate.  (Manuel
               Hermenegildo)

         @item Accented i's now translate correctly in html.  (Manuel
               Hermenegildo)

         @item Fixed a funny installation quirk: while we want to
               install LPdoc in the Ciao group, the manuals produced
               by LPdoc should be installed in the LPdoc group.
               (Manuel Hermenegildo)

         @item Now using @tt{lpdoclib} path alias.  (Manuel Hermenegildo)

         @item Fixed bug in ordering of html indices in recent Linux
               versions, related to varying file listing order
               depending on locale.  (Manuel Hermenegildo)

   	 @end{itemize}
   @end{itemize}

").

:- doc(version(1*9+56,2001/11/28,08:35*59+'CET'), "Use of @tt{:-
   doc} declarations (as a shorthand for @tt{comment}) now allowed
   (needs compatible version of Ciao which has corresponding
   modifications in assertion-processing lib).  (Manuel
   Hermenegildo)").

:- doc(version(1*9+55,2001/11/27,22:33*09+'CET'), "Made xdvi
   viewer, ps viewer, and xdvi zoom size be paramenters (the latter
   since new versions of xdvi display sizes differently than old
   ones).  (Manuel Hermenegildo)").

:- doc(version(1*9+54,2001/11/27,19:28*05+'CET'), "Fixed a few
   lingering CIAO to Ciao in documentation.  (Manuel Hermenegildo)").

:- doc(version(1*9+53,2001/08/29,00:07*23+'CEST'), "Minor bug in
   gmake infoview fixed.  (Manuel Hermenegildo)").

:- doc(version(1*9+52,2001/08/26,01:12*09+'CEST'), "Fixed bug in
   ordering of html indices in recent Linux versions, related to
   varying file listing order depending on locale. Unfortunately, fix
   not complete yet. (Manuel Hermenegildo)").

:- doc(version(1*9+49,2000/04/16,12:47*10+'CEST'), "Accented i's
   now translated correctly in html.  (Manuel Hermenegildo)").

:- doc(version(1*9+46,2000/03/17,17:49*34+'CET'), "Paths alias
   file now also read correctly for when generating htmlindex.
   (Manuel Hermenegildo)").

:- doc(version(1*9+45,2000/03/15,23:32*09+'CET'), "Now using
   @tt{lpdoclib} path alias.  (Manuel Hermenegildo)").

:- doc(version(1*9+44,2000/03/01,21:44*27+'CET'), "A new parameter
   @tt{PAPERTYPE} can be set in the @file{SETTINGS} file which
   controls the format of printed output.  (Manuel Hermenegildo)").

:- doc(version(1*9+43,2000/02/02,14:03*56+'CET'), "Default
   @concept{pdf viewer} is now @apl{ghostview}, sicne recent versions
   handle @tt{pdf} well.  (Manuel Hermenegildo)").

:- doc(version(1*9+41,1999/12/09,22:34*21+'MET'), "Fixed a funny
   installation quirk: while we want to install LPdoc in the Ciao
   group, the manuals produced by LPdoc should be installed in the
   LPdoc group.  (Manuel Hermenegildo)").

:- doc(version(1*9+29,1999/11/24,16:47*01+'MET'), "Changed default
   style sheet in order to show <PRE> lines with a monospaced font.
   (Daniel Cabeza Gras)").

:- doc(version(1*9+24,1999/11/23,00:07*42+'MET'), "Mode
   definitions now documented in a separate section. The way they are
   documented has been improved.  (Manuel Hermenegildo)").

:- doc(version(1*9+23,1999/11/22,21:29*58+'MET'), "References in
   files now updated only if @tt{.refs} file is not empty.  (Manuel
   Hermenegildo)").

:- doc(version(1*9+22,1999/11/22,16:25*34+'MET'), "Fixed bug in
   documentation text for declarations.  (Manuel Hermenegildo)").

:- doc(version(1*9+21,1999/11/22,16:23*52+'MET'), "Declarations
   now documented properly even if they have the same name and arity
   as a predicate.  (Manuel Hermenegildo)").

:- doc(version(1*9+20,1999/11/22,13:47*54+'MET'), "Fixed minor bug
   in lib Makefile which prevented full uninstallation.  (Manuel
   Hermenegildo)").

:- doc(version(1*9+11,1999/11/17,22:19*44+'MET'), "Minor changes
   to lib Makefile.  (Manuel Hermenegildo)").

:- doc(version(1*9+10,1999/11/15,19:15*52+'MET'), "Fixed bug that
   eliminated the html index files when generating html manual.
   (Manuel Hermenegildo)").

:- doc(version(1*9+9,1999/11/11,11:31*26+'MET'), "Default info
   index head and tail in lib is now generic.  (Manuel
   Hermenegildo)").

:- doc(version(1*9+8,1999/11/07,13:45*44+'MET'), "Several
   improvements to makefiles including avoiding lpdoc being affected
   by an upper definition of @tt{LIBDIR}.  (Manuel Hermenegildo)").

:- doc(version(1*9+7,1999/11/03,11:40*08+'MET'), "@em{Copies} of
   the html and info index head and tail files are now included in
   @em{distributions}. This fixed a bug that cropped up during
   installation of manuals.  (Manuel Hermenegildo)").

:- doc(version(1*9+6,1999/11/02,11:02*52+'MET'), "A @em{copy} of
   the html style sheet is now included in @em{distributions}.  Fixes
   a bug that cropped up during installation of manuals.  (Manuel
   Hermenegildo)").

:- doc(version(1*9+3,1999/07/14,12:51*53+'MEST'), "Made pointers
   relative in library html templates.  (Manuel Hermenegildo)").

:- doc(version(1*9+0,1999/07/08,18:19*43+'MEST'), "

   In this release the name of the application has changed to @apl{lpdoc}.

   @begin{itemize}
   @item New commands:
         @begin{itemize}
         @item @@begin@{cartouche@} and @@end@{cartouche@} commands
               now supported.
         @item @@foonote command now supported.
         @item New @tt{gmake htmlview} command (makes a running
               @apl{netscape} visit the generated html
               manual). Suggested by Per Cederberg.
         @item New @tt{gmake distclean} command, intended for software
               distributions. Leaves the generated documents and
               eliminates @em{all} intermediate files (including
               @tt{.texic}/@tt{.texi} files).
         @item Adobe @tt{pdf} format now supported as a valid
               target. Unfortunately, embedded @tt{.eps} figures are
               not supported at this time in pdf output.
         @item The second argument of @tt{:- comment(hide,...).} and
               @tt{:- comment(doinclude,...).} declarations can now be
               a list of predicate names.
         @item A @tt{-u} @em{File} option is now supported so that a
               file including, e.g., path alias definitions can be
               included (this has the same functionality as the
               @tt{-u} option in @apl{ciaoc}).
         @item Now typing just @tt{gmake} does nothing. In order to do
               something at least one target should be specified. This
               was necessary so that recursive invocations with empty
               arguments did nothing.
         @item Added a new filetype: @tt{part}. This allows splitting
               large documents into parts, each of which groups a
               series of chapters.
         @end{itemize}

   @item Other new functionality:
   	 @begin{itemize}
   	 @item A style sheet can now be specified which allows modifying
   	       many characteristics of the html output (fonts, colors,
   	       background, ...) (thanks to Per Cederberg).
   	 @item Added limited support for changing page numbering (in
   	       @file{SETTINGS} file).
   	 @item The concept indexing commands (@@index, @@cindex, and
   	       @@concept) now work somewhat differently, to make them
   	       consistent with other indexing commands.
   	 @item The old @em{usage} index is now called, more appropriately,
   	       @em{global} index. Correspondingly, changed things so that
   	       now every definition goes to the global index in addition to
   	       its definitional index.
   	 @item Imported files from module @tt{user} are now documented
   	       separately.
   	 @item Now a warning is issued if characters unsupported by info are
   	       used in section names.
   	 @item Navigation in html docs was improved.
   	 @item The table of contents in printed manuals now contains entries
   	       for the individual descriptions of predicates, props,
   	       regtypes, declarations, etc.  This can be shut off with the
   	       @tt{-shorttoc} option.
   	 @item Made more silent in normal conditions: file inclusion is
   	       muted now unless @tt{-v} option is selected.
   	 @item A single @tt{.texi} file is now constructed (by grouping the
   	       @tt{.texic} files generated for all components) in which the
   	       references and menus are resolved. This has the advantage
   	       that the process of resolving references and menus has now
   	       been sped up very significantly.  Also, @tt{texi} is now a
   	       valid target (perhaps useful for distributions). The
   	       generated files now have @tt{texic} (@em{texinfo component}).
   	 @item Now, declarations are always documented as long as there is a
   	       @tt{decl} assertion.  Also, they are now documented in a
   	       separate section.
   	 @end{itemize}

   @item Bug fixes and other minor improvements:
   	 @begin{itemize}
   	 @item The directory containing html manual is now called
   	       @em{BASENAME}@tt{_html} instead of just @em{BASENAME}, which
   	       was confusing.
   	 @item Now requesting building a .ps only does not leave a .dvi
   	       behind (useful for distributions).
   	 @item File names can now include the symbol @tt{_} even if they
   	       contain figures.
   	 @item @apl{TeX}-related intermediate files are now cleaned up after
   	       each run in order to avoid clutter.
   	 @item Fixed @tt{-modes}, which was broken since going to the new
   	       normalizer (was normalizer problem). Fixed problem with no
   	       documentation when only modes given.
   	 @item Fixed duplication of documentation for internal predicates
   	       when also exported.
   	 @item Minor formatting problem when no documentation nor definition
   	       found for a regtype fixed.
   	 @item Determining exports, imports, etc. now done solely by calls
   	       to @lib{c_itf} library (and, thus, synchronized with
   	       @apl{ciaoc} compiler).
   	 @end{itemize}
   @end{itemize}
   (Manuel Hermenegildo)

").

:- doc(version(1*8+42,1999/06/30,14:02*12+'MEST'), "Changed texi2html
   to generate better HTML with attributes for styling (instead of <font>
   tags). (Per Cederberg)").

:- doc(version(1*8+41,1999/06/22,19:33*51+'MEST'), "Added
   @tt{gmake htmlview} command (makes a running @apl{netscape} visit
   the generated html manual). Suggested by Per Cederberg. (Manuel
   Hermenegildo)").

:- doc(version(1*8+40,1999/06/17,20:06*00+'MEST'), "File names can
   now include the symbol @tt{_} even if they contain figures (fixes a
   reported bug).  (Manuel Hermenegildo)").

:- doc(version(1*8+39,1999/06/10,13:16*43+'MEST'), "Added default style
   sheet for html and introduced conditional copying.  (Per Cederberg)").

:- doc(version(1*8+38,1999/06/09,19:34*10+'MEST'), "Now using style
   sheets for html output.  (Per Cederberg)").

:- doc(version(1*8+36,1999/05/27,17:05*04+'MEST'), "Directory
   containing html manual is now called @em{BASENAME}@tt{_html}
   instead of just @em{BASENAME}, which was confusing.  (Manuel
   Hermenegildo)").

:- doc(version(1*8+28,1999/04/21,10:40*55+'CEST'), "Eliminated
   unnecessary loading of iso_byte_char and dcg_expansion.  (Manuel
   Hermenegildo)").

:- doc(version(1*8+23,1999/04/08,14:53*34+'MEST'), "A @tt{-u}
   @em{File} option is now supported so that a file including path
   alias definitions can be included (this has the same functionality
   as the @tt{-u} option in @apl{ciaoc}).  (Manuel Hermenegildo)").

:- doc(version(1*8+22,1999/04/08,01:23*48+'MEST'), "A background
   can now be specified for the @tt{html} manuals (needs a modified
   version of @apl{texi2html} included with the @apl{lpdoc}
   library).  (Manuel Hermenegildo)").

:- doc(version(1*8+21,1999/04/07,23:19*33+'MEST'),
   "@apl{TeX}-related intermediate files are now cleaned up after each
   run in order to avoid clutter.  (Manuel Hermenegildo)").

:- doc(version(1*8+20,1999/04/07,21:11*14+'MEST'), "Adobe @tt{pdf}
   format now supported as a valid target. Unfortunately, embedded
   @tt{.eps} figures are not supported at this time in pdf
   output. (Manuel Hermenegildo)").

:- doc(version(1*8+19,1999/04/07,21:06*55+'MEST'), "A single
   @tt{.texi} file is now constructed (by grouping the @tt{.texic}
   files generated for all components) in which the references and
   menus are resolved. This has the advantage that the process of
   resolving references and menus has now been sped up very
   significantly.  Also, @tt{texi} is now a valid target (perhaps
   useful for distributions).  (Manuel Hermenegildo)").

:- doc(version(1*8+17,1999/04/05,23:24*04+'MEST'), "Now requesting
   building a .ps only does not leave a .dvi behind (useful for
   distributions).  (Manuel Hermenegildo)").

:- doc(version(1*8+3,1999/03/26,20:00*47+'MET'), "Changed
   @tt{Makefile.skel} so that now typing just @tt{gmake} does
   nothing. In order to do something at least one target should be
   specified. This was necessary so that recursive invocations with
   empty arguments did nothing.  (Manuel Hermenegildo)").

:- doc(version(1*8+1,1999/03/25,10:12*49+'MET'), "New @tt{gmake
   distclean}, intended for software distributions. Leaves the
   generated documents and eliminates @em{all} intermediate files
   (including @tt{.texi} files). (Manuel Hermenegildo)").

:- doc(version(1*8+0,1999/03/24,21:15*33+'MET'), "

   This version completes the port to using the ciao 0.8 modular
   assertion processing library. In addition, it includes the
   following improvements:

   @begin{itemize}
   @item Now, if the name of a file being documented ends in
         @tt{_doc}, the @tt{_doc} part is left out when referring to
         the file in the documentation (useful if one would like to
         place the documentation declarations in different file).
   @item It is now possible to declare (via a @decl{comment/2}
         declaration) the intended use of a file which is not a module
         (i.e. a package, user, or include file), which results in
         correct documentation of operator definitions, new
         declarations, etc. The declaration is only needed for 'user'
         files (i.e., files to be loaded with @pred{ensure_loaded/1}).
   @item Separated generation of the manuals from their
         installation. I.e., @tt{gmake install} now does not force a
         @tt{gmake all}, which has to be done by hand. This was
         necessary to ensure correct installation of distributed
         manuals, even if modification dates are changed during
         installation. Previously, in some cases generation was
         triggered unnecessarily.
   @item New @tt{-v} option allows using quieter by default operation
         when not debugging.
   @item New option @tt{-propmods} makes the name of the module in
         which a property is defined appear in front of the property
         in the places where the property is used.
   @item New option @tt{-noisoline} makes the textual explanation of
         the @prop{iso/1} property not appear in the description of
         the usage (but the @iso symbol does appear)
   @item Two new options, @tt{-nosysmods} and @tt{-noengmods},
         selectively avoid listing the system or engine libraries
         used.
   @item If there is no declaration for a predicate, now a line is
         output with the name and arity and a simple comment saying
         that there is no further documentation available (this has
         the great advantage that then it goes in the index, and, for
         example in ciao, they get added to completion commands!).
   @item Now, if a property or regtype declaration has no textual
         comment, the actual definition is given (first level only) in
         the place where it is documented, and a simple generic
         message where it is used.
   @item Added @@noindent and @@iso commands.
   @item Nicer spacing now when printing predicate names which are
         operators, as well as modes, etc.
   @item Reporting of versions in libraries has been improved: now
         both the global version and the last version in which the
         library itself was changed are reported.
   @item Exported new declarations also documented now for
         include-type files.
   @item A module is now documented even if exports nothing at all.
   @item Engine modules used now documented even if no other modules
         used (was a reported bug).
   @item Fixed indexing of names containing @@ etc. for newer versions
         of texinfo.
   @item Tabs in verbatim modes now converted to a number of spaces
         (8). Not perfect, but produces better output than leaving the
         tabs in.
   @item Tex is now run in 'nonstopmode' which means it will typically
         not stop if there are minor errors (but some errors may go
         unnoticed...).
   @item The full path of the version maintenance directory is now
         computed (correctly) using the directory of the @tt{.pl} file
         being documented as base.
   @item Notices for missing subtitle, copyright, and summary now only
         given from main file and not for components.
   @item Added special handling of regtype and generalized it to
         handle some props specially if there is a certain comp
         property present.
   @end{itemize}

   (Manuel Hermenegildo)").

:- doc(version(1*7+34,1999/03/22,16:53*03+'CET'), "In
   @file{Makefile.skel}, separated generation of the manuals from
   their installation. I.e., @tt{gmake install} now does not force a
   @tt{gmake all}, which has to be done by hand. This was necessary to
   ensure correct installation of distributed manuals, even if
   modification dates are changed during installation. Previously, in
   some cases generation was triggered unnecessarily.  (Manuel
   Hermenegildo)").

:- doc(version(1*7+14,1999/03/01,11:03*24+'MET'), "Minor fix to
   dependencies in Makefile.  (Manuel Hermenegildo)").

:- doc(version(1*7+0,1998/12/02,17:43*50+'MET'), "Major port to
   use the ciao 0.8 modular assertion processing library.  (Manuel
   Hermenegildo)").

:- doc(version(1*6+9,1998/09/16,12:14*01+'MEST'), "Added
   @tt{DOTcshrc} files to library.  (Manuel Hermenegildo)").

:- doc(version(1*6+8,1998/09/16,12:13*31+'MEST'), "Updated
   documentation. Separated intro stuff into chapters: more logical,
   faster compilation.  (Manuel Hermenegildo)").

:- doc(version(1*6+7,1998/09/15,19:17*53+'MEST'), "Nicely
   formatted @tt{README} and @tt{INSTALL} files now generated
   automatically on installation.  (Manuel Hermenegildo)").

:- doc(version(1*6+4,1998/09/11,10:28*34+'MEST'), "Trouble
   shooting section added to documentation.  (Manuel Hermenegildo)").

:- doc(version(1*6+0,1998/09/08,12:49*26+'MEST'), "

   Added support for @concept{inserting images} (.eps files) in text
   via @@image command, @concept{email addresses} via @@email command,
   and @concept{url references} via @@uref command.

   Unix 'man' output much improved. Also, it now includes a
   @concept{usage section}. The correspoding text must be given in a
   string contained in the first argument of a fact of the
   @pred{usage_message/1} predicate which appears in the
   program. Also, formatting of 'man' pages has been greatly improved.

   A new 'ascii' format is now supported: a simple minded ascii manual
   (basically, an info file without pointers).

   (Manuel Hermenegildo)").

:- doc(version(1*5+6,1998/08/31,13:52*36+'MET DST'), "@tt{Makefile} can
   now use printf instead of echo for portability (unfortunately,
   different versions of echo behave very differently).  (Manuel
   Hermenegildo)").

:- doc(version(1*5+0,1998/08/23,20:30*32+'EST'), "

   Now supporting a @@cite command (YES!). It automatically accesses
   the bib entries in @tt{.bib} files (using @apl{bibtex}) and
   produces a 'References' appendix. @@cite can be used in the text
   strings exactly as \cite in LaTeX. The set of bib files to be used
   is given in the @tt{SETTINGS} file.

   Defining the type of version maintenance that should be performed
   by the @apl{emacs} ciao.el mode (i.e., whether version numbers are
   in a given directory or in the file itself) is controlled now via a
   standard @decl{commment/2} declaration. You should now write a
   declaration such as:

     @tt{:- comment(version_maintenance,dir('../version')).}

   to state that control info is kept in directory
   @tt{../version}. This has the advantage that it is shorter than the
   previous solution and that lpdoc can read this info easily. Using
   this guarantees that the version numbers of the manuals always
   concide with those of the software.

   Generation of indices of manuals (.htmlbullet files): if several
   manuals are installed in the same directory, an index to them is
   now generated at the beginning of the html cover page describing
   the directory.

   (Manuel Hermenegildo)").

:- doc(version(1*4+5,1998/08/14,09:22*06+'MET DST'), "More
   improvements to @tt{Makefile}. (Manuel Hermenegildo)").

:- doc(version(1*4+3,1998/08/07,14:27*20+'MET DST'), "Several
   changes in @tt{Makefile} (Manuel Hermenegildo)").

:- doc(version(1*4+0,1998/08/04,19:10*35+'MET DST'), "

   The set of paths defined in @tt{SETTINGS} for finding the source files
   are now also used to find 'included' files. As a result, full path
   is not needed any more in, e.g, @@include command.  

   New @@ref command which can be used to refer to chapeter, sections,
   subsections, etc..

   Support for recent minor changes in assertion format, including '#'
   as comment separator.

   Used modules are now separated in documentation (in the interface
   description) by type (user, system, engine...).  

   Supports new 'hide' option in comments, to prevent an exported
   predicate from being documented. This is useful for example for
   avoiding mentioning in the documentation multifile predicates which
   are not intended to be modified by the user.

   (Manuel Hermenegildo)").

:- doc(version(1*3+2,1998/07/10,20:42*12+'MET DST'), "Added
   overall handling of -s option -- separating system and user
   libraries.  (Manuel Hermenegildo)").

:- doc(version(1*3+0,1998/07/10,16:35*02+'MET DST'), "

   Exports are now listed in the chapter header separated by kind
   (pred, types, properties, ...).

   The list of other modules used by a module is now separated in the
   chapter header into User and System modules (controlled by two sets
   of paths in @tt{SETTINGS}).

   New @em{hide} option of comment/2 decl prevents an exported
   predicate from being included in the documentation: @tt{:-
   comment(hide,p/3)}.

   (Manuel Hermenegildo)").

:- doc(version(1*2+1,1998/06/10,10:15*28+'MET DST'), "More
   improvements to documentation.  (Manuel Hermenegildo)").

:- doc(version(1*2+0,1998/06/04,09:12*19+'MET DST'), "Major
   overall improvements...  (Manuel Hermenegildo)").

:- doc(version(1*1+26,1998/05/23,04:57*00+'EST'), "Links now
   preserved when making distribution tar files.  (Manuel
   Hermenegildo)").

:- doc(version(1*1+24,1998/05/06,09:08*13+'MET DST'), "Improved
   makefiles (no explicit calls to gmake).  (Manuel Hermenegildo)").

:- doc(version(1*1+8,1998/4/9), "Enhanced documentation.  (Manuel
   Hermenegildo)").

:- doc(version(1*1+3,1998/4/7), "Fixed document makefile so that
   no formatting is repeated (by creating .ltxi files).  (Manuel
   Hermenegildo)").

:- doc(version(1*1+2,1998/4/6), "Replaced atom_chars/2 with
   atom_codes/2 (to make code more compatible with older Prologs).
   (Manuel Hermenegildo)").

:- doc(version(1*1+1,1998/4/3), "Added treatment of @tt{comment}
   command.  (Manuel Hermenegildo)").

:- doc(version(1*1+0,1998/3/31), "Incorporated autodoc and
   autodoformats library to source in order to make distribution
   standalone. Improvements to installation and
   documentation. @tt{Makefile}s now also install documentation in public
   areas and produce global indices. Several documents can cohexist in
   the same installation directory. (Manuel Hermenegildo)").

:- doc(version(1*0+6,1998/3/30), "Improved installation.  (Manuel
   Hermenegildo)").

:- doc(version(1*0+5,1998/3/19), "Minor changes to
   Doc@tt{Makefile} to fix some bugs.  (Manuel Hermenegildo)").

:- doc(version(1*0+4,1998/3/11), "Enhaced documentation.  (Manuel
   Hermenegildo)").

:- doc(version(1*0+3,1998/2/26), "Fixed minor bug in @tt{Makefile}s.
   (Manuel Hermenegildo)").

:- doc(version(1*0+2,1998/2/25), "Added version reporting at
   startup.  (Manuel Hermenegildo)").

:- doc(version(1*0+1,1998/2/25), "Made major fixes to makefiles to
   improve installation and deinstallation.  (Manuel Hermenegildo)").

:- doc(version(1*0+0,1998/2/24), "First Ciao-native distribution,
   with installation. (Manuel Hermenegildo)").

:- doc(version(0*9+1,1998/2/24), "File version is now global
   lpdoc version.  (Manuel Hermenegildo)").

:- doc(version(0*9+0,1998/2/24), "Intermediate version, preparing
   for first major release. Modified @tt{Makefile} and @tt{SETTINGS} to handle
   installation of manuals. (Manuel Hermenegildo)").

:- doc(version(0*6+1,1998/2/20), "Modified @tt{Makefile} and @tt{SETTINGS}
   to handle installation of manuals. (Manuel Hermenegildo)").

:- doc(version(0*6+0,1998/2/10), "Added new indices and options,
   as well as more orthogonal handling of files. (Manuel
   Hermenegildo)").

:- doc(version(0*4+0,1998/2/24), "Added support for nroff -m
   formatting (e.g., for man pages). Added support for optional
   selection of indices to be generated. Added support for reexported
   predicates. Added (low level) ascii format. Added option handling
   (-nobugs -noauthors -noversion -nochangelog -nopatches -modes and
   -headprops ...).  -literalprops. Fixed presentation when there are
   multiple kinds of assertions. Better error checking for
   includefact/includedef. (Manuel Hermenegildo) ").

:- doc(version(0*3+0,1998/2/10), "Changed file reader to use Ciao
   native builtins. As a result, syntax files and full Ciao syntax now
   supported. Major reorganization of the code to make formatting more
   orthogonal. Now applications and libraries can be components or
   main files, standalone or with components
   interchangeably. @@includefact, new predicate types, used libraries
   now precisely detected, @tt{docinclude} option. (Manuel
   Hermenegildo)").

:- doc(version(0*2+1,1998/1/26), "Changed file reader to use Ciao
   native builtins. As a result, syntax files and full Ciao syntax now
   supported. (Manuel Hermenegildo)").

:- doc(version(0*2+0,1997/12/16), "Ported to native ciao.  Version
   handling, selection of indices, @@include. Added generation of an
   html brief description for a global index. Added unix manual page
   generation. Added support for specifying library paths. -l option
   for htmlindex and man. Installation improved: now all files for one
   application in the same directory. (Manuel Hermenegildo)").

:- doc(version(0*1+9,1997/11/10), "Added handling of -l option for
   htmlindex and man. (Manuel Hermenegildo)").

:- doc(version(0*1+8,1997/9/19), "Changed argument parsing to use
   DCGs -- much nicer! (Manuel Hermenegildo)").

:- doc(version(0*1+7,1997/9/19), "Added support for specifying 
   library paths. (Manuel Hermenegildo)").

:- doc(version(0*1+6,1997/9/16), "Using new version of autodoc
   library. (Manuel Hermenegildo)").

:- doc(version(0*1+5,1997/9/15), "Modified towards making it run
   under both SICStus and Ciao native. (Manuel Hermenegildo)").

:- doc(version(0*1+4,1997/8/20), "Version number automatically
   included in files if available.").

:- doc(version(0*1+3,1997/8/12), "Added index option handling.").

:- doc(version(0*1+2,1997/8/11), "Installation improved: now all
   files for one application in the same directory.").

:- doc(version(0*1+1,1997/8/07), "Changed installation method to
   include automatic handling of version numbers.").

:- doc(version(0*1+0,1997/07/30), "First official version (major
   rewrite from several previous prototypes, autodocumented!). (Manuel
   Hermenegildo)").

:- doc(version(0*0+0,1996/10/10),"First prototype.").

