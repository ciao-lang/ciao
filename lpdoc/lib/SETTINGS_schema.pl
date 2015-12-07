% :- module(_, _, [assertions, regtypes, fsyntax]).
%
% ***************************************************************************
% * A schema file for LPdoc settings.                                       *
% * It has to be included as part of a real SETTINGS.pl file                *
% ***************************************************************************
% TODO: This declares that the host module has been merged with this schema.
% TODO: Do it automatically
:- doc(nodoc, '$schema'/1).
:- export('$schema'/1).
'$schema'('SETTINGS_schema').

%
% :- doc(title, "Base Configuration Definitions for LPdoc").
% 
% :- doc(module, "This file provides the base configuration definitions
%    and documentation for a @apl{lpdoc} settings file.").
% 
% :- doc(author, "Manuel Hermenegildo").
% :- doc(author, "Jose F. Morales").
% 
% :- doc(bug, "Write a package that really tests that we provide all
%    the definitions specified here.")
%
% :- doc(bug, "This should be a template or interface module (however
%    those are not defined yet in Ciao)").
%
% :- doc(filetype, user).
%
% :- use_module(library(system)).
% :- use_module(library(bundle/paths_extra), [fsR/2]).

% ----------------------------------------------------------------------------
% PATHS 
% ----------------------------------------------------------------------------

:- pred filepath/1 => dirpath

# "Defines the directories where the @tt{.pl} files to be documented
   can be found.  You also need to specify all the paths containing
   files which are used by the files being documented. For example,
   the paths to files included by an @tt{@@include} command or to
   figures.".

:- doc(dirpath/1, "An atom describing a path to a
   directory. Should be a full, explicit path (i.e., not containing
   environment variables).").

:- regtype dirpath(P) # "@var{P} is a full path to a directory.".

dirpath(P) :- atm(P).

:- regtype filename(P) # "@var{P} is a full path to a file.".

filename(P) :- atm(P).

% ----------------------------------------------------------------------------

:- pred output_name(Base) => sourcename
   # "Defines the base file name used to be part of the output name of
     the generated files. By default it is equal to the root file of
     the document structure @pred{doc_structure/1}.

     If the @tt{no_versioned_output} option is not specified in
     @pred{doc_mainopts/1}, the bundle version number is appended
     to the output name".

% ----------------------------------------------------------------------------
% The document structure
% ----------------------------------------------------------------------------

:- pred doc_structure(Term)
   # "Defines the document structure as a tree. The tree is defined as
      a root node with optional childs. Nodes can be atoms or pairs
      (@tt{N-Cs}), where @tt{Cs} is a list of nodes. The root of the
      tree is the main file of the manual, i.e., the file which
      determines the manual's cover page, and first chapter. The child
      files are used as components, i.e., which will constitute the
      subsequent chapters of the manual.".

% ----------------------------------------------------------------------------
% Setting processing options for the different files
% ----------------------------------------------------------------------------

:- pred doc_mainopts(Option) :: supported_option
# "@var{Option} is a processing option which should be activated when
   processing the main file.".

:- pred doc_compopts(Option) :: supported_option
# "@var{Option} is a processing option which should be activated when
   processing the secondary files (all except the main file).".

:- regtype supported_option/1 #
	"Possible options: @includedef{supported_option/1}".

:- doc(supported_option/1, "In general, selecting none of these
	options generates the most verbose manuals, i.e., each option
	generally supresses the production of a certain kind of output
	(on the other hand, @tt{'verbose'} selects verbose output from
	@apl{ciaoc} when processing the file).").

% TODO: Duplicated in lpdoc/src/autodoc_state.pl
supported_option := verbose.
supported_option := no_bugs.
supported_option := no_authors.
supported_option := no_version.
supported_option := no_versioned_output.
supported_option := no_lpdocack.
supported_option := no_changelog.
supported_option := no_patches.
supported_option := modes.
supported_option := head_props.
supported_option := literal_props.
supported_option := no_prop_names.
supported_option := no_undefined.
supported_option := no_propsepln.
supported_option := no_biblio.
supported_option := no_bullet.
supported_option := no_sysmods.
supported_option := no_engmods.
supported_option := no_packages.
supported_option := no_isoline.
supported_option := propmods.
supported_option := shorttoc.

% ----------------------------------------------------------------------------
% Default document formats
% ----------------------------------------------------------------------------

:- pred docformat(Format) => supported_format

# "Defines the documentation formats to be generated by default when
   running @apl{lpdoc}, among the following (@em{they should be kept
   in this order}): @includedef{docformat/1}".

:- regtype supported_format/1
# "Available formats: @includedef{supported_format/1}".

supported_format := texi.
supported_format := dvi.
supported_format := ps.
supported_format := pdf.
supported_format := ascii.
supported_format := manl.
supported_format := info.
supported_format := html.

% ----------------------------------------------------------------------------
% Indices to be generated
% ----------------------------------------------------------------------------

:- pred index(Format) => supported_index

# "Defines the indices to be generated by default when running
   @apl{lpdoc}, among the following: 

   @noindent @tt{concept lib apl pred prop regtype decl op modedef file global}

   Selecting @tt{all} generates all the supported indices. However,
   note that this (as well as selecting many indices explicitely)
   exceeds the capacity of most texinfo installations.".

:- regtype supported_index/1
# "Supported indexes: @includedef{supported_index/1}".

supported_index := concept.
supported_index := lib.
supported_index := apl.
supported_index := pred.
supported_index := prop.
supported_index := regtype.
supported_index := decl.
supported_index := op.
supported_index := modedef.
supported_index := file.
supported_index := global.
supported_index := all.

% ----------------------------------------------------------------------------
% References
% ----------------------------------------------------------------------------

:- pred bibfile(Format) => filename

# "If you are using bibliographic references, define in this way the
   @tt{.bib} files to be used to find them.".

% ----------------------------------------------------------------------------
% Other settings
% ----------------------------------------------------------------------------

:- pred startpage(PageNumber) => int

# "Setting this to a different value allows changing the page number of
   the first page of the manual. This can be useful if the manual is to
   be included in a larger document or set of manuals.
   Typically, this should be an odd number.".

:- pred papertype(PageNumber) => supported_papertype

# "Selects the type of paper/format for printed documents.  See also
   the @tt{-onesided} and @tt{-twosided} options for the main file.".

:- regtype supported_papertype/1
# "Possible papertypes: @includedef{supported_papertype/1}".

supported_papertype := letterpaper.
supported_papertype := smallbook.
supported_papertype := afourpaper.
supported_papertype := afourlatex.
supported_papertype := afourwide.
supported_papertype := afourthesis.

:- pred libtexinfo/1 => yesno

# "If set to yes the @file{texinfo.tex} file that comes with the
   lpdoc distribution will be used when generating manuals in
   formats such as @tt{dvi} and @tt{ps}. Otherwise, the texinfo file
   that comes with your @apl{tex} installation will be used. It is
   recommended that you leave this set to @tt{'yes'}.".

:- regtype yesno/1
# "Enumerated type: @includedef{yesno/1}".

yesno := yes|no.

% ============================================================================
% Installation options
% (You only need to change these if you will be installing the docs somewhere)
% ============================================================================

%-----------------------------------------------------------------------------
% WHERE MANUALS WILL BE INSTALLED
%-----------------------------------------------------------------------------

:- pred htmldir/1 => dirpath 
   # "Directory where the @tt{html} manual will be generated.".

:- pred docdir/1 => dirpath
   # "Directory in which you want the document(s) installed.".

:- pred infodir/1 => dirpath
   # "Directory in which you want the @tt{info} file installed.".

:- pred mandir/1 => dirpath 
   # "Directory in which the @tt{man} file will be installed.".

% ---------------------------------------------------------------------------
% Permissions

:- pred datamode(DataPermissions) => permission_term
   # "Define this to be the mode for automatically generated data
      files.".

:- pred execmode(ExecPermissions) => permission_term
   # "Define this to be the mode for automatically generated
      directories and executable files.".

:- regtype permission_term/1 # "Permisions: @includedef{permission_term/1}.".

permission_term(perms(User, Group, Others)) :-
	permission(User),
	permission(Group),
	permission(Others).

:- regtype permission(P) # "Possible permisions:
                            @includedef{permission/1}.".

permission := x.
permission := w.
permission := wx.
permission := r.
permission := rx.
permission := rX.
permission := rw.
permission := rwx.
permission := 'X'.
permission := wX.
permission := rwX.

% ----------------------------------------------------------------------------
% End of SETTINGS_schema
