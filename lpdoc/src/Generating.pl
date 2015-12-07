
:- use_package([assertions]).

:- doc(filetype, documentation).

:- doc(title,"Generating Installing and Accessing Manuals").

:- doc(author,"Manuel Hermenegildo").

:- doc(module,"

@cindex{generating manuals}

@begin{alert}   
@bf{Note: significant parts of this are obsolete.  They must be
updated to describe lpdoc version 2.0.}
@end{alert}

This section describes how to generate a manual (semi-)automatically from
a set of source files using @apl{lpdoc}, how to install it in a
public area, and how to access it on line. It also includes some
recommendations for improving the layout of manuals, @concept{usage
tips}, and @concept{troubleshooting} advice.

@section{Generating a manual from the Ciao Emacs mode} 

@cindex{Emacs, generating manuals from}
@cindex{generating from Emacs}
@cindex{Ciao} 
@cindex{Prolog, Ciao} 
@cindex{Emacs, LPdoc mode} 

If you use the @apl{Emacs} editor (highly recommended in all
circumstances), then the simplest way to quickly generate a manual is
by doing it from the @concept{Ciao Emacs mode} (this mode comes with
the Ciao Prolog distribution and is automatically installed with
Ciao). The Ciao Emacs mode provides menu- and keyboard-binding driven
facilities for generating a standalone document with the
documentation corresponding to the file in the buffer being visited by
Emacs.  This is specially useful while modifying the source of a file,
in order to check the output that will be produced when incorporating
this file into a larger document. It is also possible to generate more
complex documents, by editing the (automatically provided)
@index{SETTINGS.pl} in the same way as when generating a manual from
the command line (see below). However, when generating complex
documents, it is best to devote an independent, permanent directory to
the manual, and the full procedure described in the rest of this text
is preferred.

@section{Generating a manual} 

Two possible scenarios are described in this section. The first one is
indicated to document quickly a single module and the second one
targets the documentation of a larger application or library, in which
the settings (which define how the documentation is to be generated,
etc.) are read from a file, so that they can be reused as the
application / library evolves.

In order to make @apl{lpdoc} generate quickly the documentation of a
single file it suffices to execute the command @tt{lpdoc -d
doc_structure=modulename dvi}, where @tt{modulename} is the module to be
documented (without extension) and (in this example) @tt{dvi} is the
desired format of the manual (other accepted formats include html,
pfd, ps, etc. -- see later). @apl{lpdoc} will generate a manual with
the name of the module and the format extension (in the example it
would be modulename.dvi) in the same directory where it is executed.

For the second scenario, the @apl{lpdoc} library directory includes a
generic file which is quite useful for the generation of complete
manuals: the @index{SETTINGS.pl} file.  Use of this file is strongly
recommended. Generating a manual using this file involves the
following steps:

@begin{itemize}

@item Create a directory (e.g., @tt{doc}) in which the documentation
will be built. The creation of this directory is recommended, as it
will be populated with intermediate files which are best kept
separate.  This directory is typically created in the top directory of
the distribution of the application or library to be documented."||

% @item Copy the file @file{SETTINGS} and copy (or, much better, link,
% since you typically will not need to change it) the file
% @file{Makefile} from the @concept{lpdoc library directory} into the
% @tt{doc} directory just created. The location of the @apl{lpdoc}
% library directory is installation-dependent (see @ref{Installing the
% source distribution (lpdoc)}).

"@item Execute the command @tt{lpdoc lpsettings} in the directory
where the documentation is to be created (e.g., @tt{doc} in the
previous point). @apl{lpdoc} will create an
@file{SETTINGS.pl.generated} file with the default settings.  This
file should be renamed to @file{SETTINGS.pl} once the user agrees
with its contents.

@item Edit @file{SETTINGS.pl} to suit your needs. It is recommended that
you review, at least, the following points:

@begin{itemize}

@item Set the variable @tt{filepath} to include all the directories
where the files to be documented can be found."||

%% (JFMC - lpdoc loads paths from bundle definitions now)
%
% @item Set the variable @tt{systempath} to include all the @em{system}
% directories where system files used can be found, regardless whether
% they are to be documented or not.  This will be used to access
% definitions of types, etc.
% 
% It is very important to include @em{all} related directories either in
% @tt{filepath} or in @tt{systempath} because on startup @apl{lpdoc}
% has @em{no default search paths for files} defined (not even those
% typically defined by default in the @apl{Prolog} system under which it
% was compiled! -- this allows documenting @apl{Prolog} systems other
% than that under which @apl{lpdoc} was compiled).
% 
% The effect of putting a path in @tt{systempaths} instead of in
% @tt{filepaths} is that the modules and files in those paths are
% documented as @index{system modules} (this is useful when
% documenting an application to distinguish its parts from those which
% are in the system libraries).

"@item Set @tt{doc_structure} to be the @index{document structure}
(@pred{doc_structure/1}).

@end{itemize}

For the rest of the settings in the @file{SETTINGS.pl} file you can
simply use the default values indicated. You may however want to
change several of these:

@begin{itemize}

@item @tt{doc_mainopts} can be set to a series of options which allow more
detailed control of what is included in the documentation for the main
file and how (i.e., including bug information @cindex{including or not
bug info}, versions and patches or only patches @cindex{including or
not versions, patches}, authors @cindex{including or not authors},
changelog @cindex{including or not changelog}, explanation of modes,
@index{one-sided printing} (@index{two-sided} is the default), etc.).
See @pred{option_comment/2} in @lib{autodoc} or type @tt{lpdoc
-help} for a list of these options.

@item In the same way @tt{doc_compopts} sets options for the component
files. Currently these options are common to all component files but
they can be different from @tt{doc_mainopts}. The allowable options are
the same as above.

@item @tt{docformat} determines the set of formats (@tt{dvi},
@tt{ps}, @tt{ascii}, @tt{html}, @tt{info}, @tt{manl}, ...) in which
the documentation should be generated by default when typing
@apl{lpdoc all}. Selecting @tt{htmlindex} and/or @tt{infoindex}
requests the generation of (parts of) a master index to be placed in
an installation directory and which provide pointers to the documents
generated (see below). If the main file is an @bf{application}, and
the @tt{manl} option is selected, then @apl{lpdoc} looks for a
@pred{usage_message/1} fact, which should contain a string as
argument, and will use that string to document the @index{usage of
the application} (i.e., it will be used to fill in the
@index{synopsis section of the man page}).

@item @tt{output_name} determines the base file name of the main
documents generated by lpdoc.  By default it is equal to the main file
name, or, if the main file name ends with @tt{_doc}, then it is equal
to the name without the @tt{_doc} suffix.  This is useful when the
name of the documentation file to be produced needs to have a name
that is not directly related to the main file being documented.

@item @tt{index} determines the list of indices to be included at
the end of the document.  These can include indices for defined
predicates, modules, concepts, etc.  For a complete list of the types
of indices available see @pred{index_comment/2} in
@lib{autodoc} or type @apl{lpdoc -help} for a listing.  A
setting of @tt{all} generates all the supported indices -- but
@em{beware of limitations in the number of simultaneous indices}
supported in many @apl{texinfo} installations.

@item @tt{bibfile} determines a list of @index{.bib files} (one file
per path), i.e., files containing @index{bibliographic entries} in
@apl{bibtex} @cindex{bibtex} format. This is only relevant if you are
@concept{using citations} in the text (using the @concept{@@cite
command}). In that case those will be the files in which the citations
will be searched for. All the references will appear together in a
@em{References} appendix at the end of the manual.

If you are not using citations, then select the @tt{-nobiblio} option
on the main file, which will prevent an empty 'References' appendix
from appearing in the manual.

@cindex{page numbering, changing}

@item @tt{startpage} (default value 1) allows changing the page number
of the first page of the manual. This can be useful if the manual is
to be included in a larger document or set of manuals.  Typically,
this should be an @em{odd} number.

@cindex{page size, changing}
@cindex{page style, changing}

@item @tt{papertype} (default value @tt{afourpaper}) allows select
several paper sizes for the printable outputs (@tt{dvi}, @tt{ps},
etc.). The currently supported outputs (most of them inherited from
@apl{texinfo}) are:

  @begin{description}

  @item{@tt{afourpaper}} 

     The default, usable for printing on @index{A4 paper}. Rather busy, but
     saves trees.

  @item{@tt{afourwide}} 

     This one crams even more stuff than @tt{afourpaper} on an A4
     page. Useful for generating manuals in the least amount of space.
     It saves more trees.

  @item{@tt{afourlatex}} 

     This one is a little less compressed than @tt{afourpaper}.

  @item{@tt{smallbook}} 

     Small pages, like in a handbook. 

  @item{@tt{letterpaper}} 

     For printing on American @index{letter size paper}.

  @item{@tt{afourthesis}} 

     A @index{thesis-like style} (i.e., double spaced, wide margins
     etc.). Useful -- for inserting @apl{lpdoc} output as appendices 
     of a thesis or similar document. It does not save trees. 

  @end{description}

@end{itemize}

@item Type @apl{lpdoc all} to generate all the formats
defined. @tt{lpdoc dvi}, @tt{lpdoc html}, @tt{lpdoc ps} or @tt{lpdoc
info}, etc. will force the generation of a single target format.

@end{itemize}

@section{Working on a manual} 

In order to speed up processing while developing a manual, it is
recommended to work by first generating a @tt{.dvi} version only
(i.e., by typing @tt{lpdoc dvi}). The resulting output can be easily
viewed by tools such as @apl{xdvi} (which can be started by simply
typing @tt{lpdoc view}). Note that once an @apl{xdvi} window is
started, it is not necessary to restart it every time the document is
reformatted (@tt{lpdoc dvi}), since @apl{xdvi} automatically updates
its view every time the @tt{.dvi} file changes. This can also be
forced by typing @key{R} in the @apl{xdvi} window.  The other formats
can be generated later, once the @tt{.dvi} version has the desired
contents.

@section{Cleaning up the documentation directory}

@apl{lpdoc} can also take care of tidying up the directory where the
documentation is being generated:

" ||

%% Several cleanup procedures are provided by the @file{Makefile}:

"@begin{itemize}

@item @tt{lpdoc clean} deletes all intermediate files, but leaves the
targets (i.e., the @tt{.ps}, @tt{.dvi}, @tt{.ascii}, @tt{.html},
etc. files), as well as all the generated @tt{.texic} files.

@item @tt{lpdoc distclean} deletes all intermediate files and the
generated @tt{.texic} files, leaving only the targets (i.e., the
@tt{.ps}, @tt{.dvi}, @tt{.ascii}, @tt{.html}, etc. files). This is the
option normally used when building software distributions in which the
manuals come ready made in the distribution itself and will not need
to be generated during installation.

@item @tt{lpdoc docsclean} deletes all intermediate files and the
generated targets, but leaves the @tt{.texic} files. This option can
be used in software distributions in which the manuals in the
different formats will be generated during installation. This is
generally more compact, but requires the presence of several tools,
such as @tt{tex}, @tt{Emacs}, etc., in order to generate the manuals
in the target formats during installation.

@item @tt{lpdoc realclean} performs a complete cleanup, deleting also
the .texic files, i.e., it typically leaves only the
@file{SETTINGS.pl} file.  This is is the most compact, but requires
the presence of the tools mentioned above, the source files from which
the manuals are generated and @apl{lpdoc} in order to re generate the
manuals in the target formats during installation.

@end{itemize}" ||

% NOTE: This part is obsolete. LPdoc does not generate on-line indices
%       of applications anymore (see website generation for more
%       details).

% @bf{Note: This part is obsolete.  It must be
% updated to describe lpdoc version 2.0. -- EMM}
% @end{alert}
% "@section{Installing a generated manual in a public area} 
% 
% @cindex{installation, of manuals}
% 
% Once the manual has been generated in the desired formats, the
% @file{Makefile} provided also allows automatic installation in a
% different area, specified by the @tt{docdir} option in the
% @file{SETTINGS.pl} file. This is done by typing @tt{lpdoc install}.
% 
% As mentioned above, @apl{lpdoc} can generate directly brief
% descriptions in html or @apl{Emacs} info formats suitable for
% inclusion in an on-line index of applications. In particular, if the
% @tt{htmlindex} and/or @tt{infoindex} options are selected, @tt{lpdoc
% install} will create the installation directory, place the
% documentation in the desired formats in this directory, and produce
% and place in the same directory suitable @tt{index.html} and/or
% @tt{dir} files. These files will contain some basic info on the manual
% (extracted from the summary and title, respectively) and include
% pointers to the relevant documents which have been installed.  The
% @tt{infodirheadfile} / @tt{infodirtailfile} (default examples, used in
% the CLIP group at UPM, are included with the distribution) should
% point to files which will be used as head and tail templates when
% generating the @tt{dir} files." ||

% A variable @tt{extrafiles} allows defining a list of additional files
% which will be copied to the @tt{docdir} installation directory. This
% is useful to place figures or other files which the HTML header files
% use in the installation directory (so that paths can be local). These
% files must reside in the directory in which the documentation is being
% generated.

"Several manuals, coming from different @tt{doc} directories, can be
installed in the same @tt{docdir} directory. In this case, the
descriptions of and pointers to the different manuals will be
automatically combined (appearing in alphabetic order) in the
@tt{index.html} and/or @tt{dir} indices, and a @index{contents area}
will appear at the beginning of the @index{html index page}." ||

% NOTE: It should not be a problem nowadays (we do most things in Prolog)
	
% "@cindex{index pages out of order} 
% @noindent
% @bf{Important Note:} In order for the different components to appear
% in the correct positions in the index pages mentioned above the
% traditional ('C') Lexical order must be active.  In recent Un*x
% systems (e.g., in most current Linux systems) this may not be the
% case. There are several possible fixes:
% 
% @begin{itemize}
% @item For @apl{csh} put @tt{setenv LC_COLLATE C} in your @file{.cshrc}.
% @item For @apl{bash} put @tt{export LC_COLLATE=C} in your @file{.profile}.
% @item In many systems this can be done globally by the super-user. E.g., 
%       in many Linux systems set @tt{LANG=""C""} in @tt{/etc/sysconfig/i18n}. 
% @end{itemize}
% 
% "||

"Note that, depending on the structure of the manuals being generated,
some formats are not very suitable for public installation. For
example, the @tt{.dvi} format has the disadvantage that it is not self
contained if images are included in the manual."||

% The @file{Makefile} also makes provisions for manual deinstallation from
% the installation area. 

"Typing @tt{lpdoc uninstall} in a @tt{doc} directory will uninstall
from @tt{docdir} the manuals corresponding to the @file{Makefile} in
that @tt{doc} directory.  If a manual is already installed and changes
in the number of formats being installed are desired, @tt{lpdoc
uninstall} should be made before changing the @tt{docformats} variable
and doing @tt{lpdoc install} again. This is needed in order to ensure
that a complete cleanup is performed.

@section{Enhancing the documentation being generated} 

The quality of the documentation generated can be greatly enhanced by
including within the program text:

@begin{itemize}

@item @em{assertions}, and 

@item @em{machine-readable comments}. 

@end{itemize}

@bf{Assertions} are declarations which are included in the source
program and provide the compiler with information regarding
characteristics of the program. Typical assertions include type
declarations, modes, general properties (such as @em{does not fail}),
standard compiler directives (such as @decl{dynamic/1}, @decl{op/3},
@decl{meta_predicate/1}...), etc.  When documenting a module,
@apl{lpdoc} will use the assertions associated with the module
interface to construct a textual description of this interface.  In
principle, only the exported predicates are documented, although any
predicate can be included in the documentation by explicitly
requesting it (see the documentation for the @decl{doc/2}
declaration).  Judicious use of these assertions allows at the same
time documenting the program code, documenting the external use of the
module, and greatly improving the debugging process. The latter is
possible because the assertions provide the compiler with information
on the intended meaning or behaviour of the program (i.e., the
specification) which can be checked at compile-time (by a suitable
preprocessor/static analyzer) and/or at run-time (via checks inserted
by a preprocessor).

@bf{Machine-readable comments} are also declarations included in the
source program but which contain additional information intended to be
read by humans (i.e., this is an instantiation of the
@index{literate programming} style of Knuth
@cite{knuth-lit}). Typical comments include title, author(s), bugs,
changelog, etc.  Judicious use of these comments allows enhancing at
the same time the documentation of the program text and the manuals
generated from it.

@apl{lpdoc} requires these assertions and comments to be written
using the @apl{Ciao} system @em{assertion language}.  A simple compatibility
library is available in order to make it possible to compile programs
documented using assertions and comments in traditional (constraint)
logic programming systems which lack native support for them (see the
@tt{compatibility} directory in the @apl{lpdoc} library). Using this
library, such assertions and comments are simply ignored by the
compiler. This compatibility library also allows compiling
@apl{lpdoc} itself under (C)LP systems other than the @apl{Ciao} system
under which it is developed.

@section{Accessing on-line manuals} 

As mentioned previously, it is possible to generate on-line manuals
automatically from the @tt{.texic} files, essentially @tt{.html},
@tt{.info}, and @apl{man} files. This is done by simply including the
corresponding options in the list of @tt{docformats} in the
@file{SETTINGS.pl} file and typing @apl{lpdoc all}. We now address the
issue of how the different manuals can be read on-line.

@subsection{Accessing html manuals} 

Once generated, the @tt{.html} files can be viewed using any standard
@apl{WWW} browser, e.g., @apl{Firefox} (a command @tt{lpdoc htmlview}
is available which, if there is an instance of a web browser running
in the machine, will make that instance visit the manual in @tt{html}
format). To make these files publicly readable on the WWW, they
should be copied into a directory visible by browsers running in other
machines, such as @tt{/home/clip/public_html/lpdoc_docs},
@tt{/usr/home/httpd/htmldocs/lpdoc_docs}, etc. As mentioned before,
this is easily done by setting the @tt{docdir} variable in the
@file{SETTINGS.pl} file to this directory and typing @tt{lpdoc install}.

@subsection{Accessing info manuals} 

Generated @tt{.info} files are meant to be viewed by the @apl{Emacs}
editor or by the standalone @apl{info} application, both publicly
available from the GNU project sites. To view the a generated
@apl{info} file from @apl{Emacs} manually (i.e., before it is
installed in a common area), type @tt{C-u M-x info}. This will prompt
for an info file name. @cindex{Emacs, accessing info files} Input the
name of the info file generated by @apl{lpdoc} (@tt{main}@tt{.info})
and @apl{Emacs} will open the manual in info mode.

There are several possibilities in order to install an @tt{.info} file
so that it is publicly available, i.e., so that it appears
automatically with all other @apl{info} manuals when starting
@apl{info} or typing @tt{C-u M-x info} in Emacs:

  @begin{itemize}

  @item @bf{Installation in the common info directory:} 

        @begin{itemize}

        @item Move the @tt{.info} file to the common info directory
        (typically @tt{/usr/info}, @tt{/usr/local/info}, ..). This can
        be done automatically by setting the @tt{docdir} variable in
        the @file{SETTINGS.pl} file to this directory and typing
        @tt{lpdoc install}.

        @bf{Warning:} if you are installing in an @apl{info} directory
        that is not maintained automatically by @apl{lpdoc}, make
        sure that you have not selected the @tt{infoindex} option in
        @tt{docformats}, since this will overwrite the existing
        @file{dir} file).

        @item Add an entry to the @apl{info} index in that directory
        (normally a file in that directory called @tt{dir}). The
        manual should appear as part of the normal set of manuals
        available when typing @tt{M-x info} in @apl{Emacs} or
        @apl{info} in a shell. See the @apl{Emacs} manual for details.

        @end{itemize}

  @item @bf{Installation in a different info directory:} you may want
  to place one or more manuals generated by @tt{lpdoc} in their own
  directory. This has the advantage that @tt{lpdoc} will maintain
  automatically an index for all the @apl{lpdoc} generated manuals
  installed in that directory. In order for such manuals to appear
  when typing @tt{M-x info} in @apl{Emacs} or @apl{info} in a shell
  there are two requirements:

        @begin{itemize}

        @item This directory must contain a @tt{dir} index. The first
        part of the process can all be done automatically by setting
        the @tt{docdir} variable in the @file{SETTINGS.pl} file to this
        directory, including the @tt{infoindex} option in
        @tt{docformats}, and typing @tt{lpdoc install}. This will
        install the info manual in directory @tt{docdir} and update
        the @tt{dir} file there. @tt{lpdoc uninstall} does the
        opposite, eliminating also the manual from the index.

        @item The directory must be added to the @index{info path
        list}.  The easiest way to do this is to set the @tt{INFOPATH}
        environment variable. For example, assuming that we are
        installing the @apl{info} manual in
        @tt{/home/clip/public_html/lpdoc_docs} and that @tt{/usr/info} is
        the common @tt{info} directory, for @apl{csh} in @tt{.cshrc}:

        @tt{setenv INFOPATH /usr/info:/home/clip/public_html/lpdoc_docs}

        Adding the directory to the info path list can also be done
        within Emacs, by including the following line in the
        @tt{.Emacs} file:

@begin{verbatim}
(defun add-info-path (newpath)
  (setq Info-default-directory-list
	(cons (expand-file-name newpath) Info-default-directory-list)))
(add-info-path ""/home/clip/public_html/lpdoc_docs"")
(add-info-path ""/usr/info/"")
@end{verbatim}

        However, this has the disadvantage that it will not be seen by
        the standalone @apl{info} command.

        @end{itemize}
@end{itemize}

Automatic, direct on-line access to the information contained in the
info file (e.g., going automatically to predicate descriptions by
clicking on predicate names in programs in an @apl{Emacs} buffer) can
be easily implemented via existing @tt{.el} packages such as
@tt{word-help}, written by Jens T. Berger Thielemann
(@email{jensthi@@ifi.uio.no}). @file{word-help} may already be in your
@apl{Emacs} distribution, but for convenience the file
@file{word-help.el}, providing suitable initialization are included in
the @tt{lpdoc} library.  A suitable interface for @file{word-help} is
also provided by the @tt{ciao.el} @apl{Emacs} file that comes with the
@apl{Ciao} system distribution (i.e., if @tt{ciao.el} is loaded it is
not necessary to load or initialize @tt{word-help}).

@subsection{Accessing man manuals} 

The @apl{Unix} @apl{man} format manuals generated by @apl{lpdoc} can
be viewed using the @apl{Unix} @apl{man} command. In order for
@apl{man} to be able to locate the manuals, they should be copied to
one of the subdirectories (e.g., @tt{/usr/local/man/manl}) of one of
the main man directories (in the previous case the main directory
would be @tt{/usr/local/man}).  As usual, any directory can be used as
as a man main directory, provided it is included in the environment
variable @tt{MANPATH}. Again, this process can be performed
automatically by setting the @tt{docdir} variable in the @file{SETTINGS.pl}
file to this directory and typing @tt{lpdoc install}.

@subsection{Putting it all together} 

A simple, powerful, and very convenient way to use the facilities
provided by @apl{lpdoc} for automatic installation of manuals in
different formats is to install all manuals in all formats in the same
directory @tt{docdir}, and to choose a directory which is also
accessible via @tt{WWW}. After setting @tt{docdir} to this directory
in the @file{SETTINGS.pl} file, and selecting @tt{infoindex} and
@tt{htmlindex} for the @tt{docformats} variable, @tt{lpdoc
install}/@tt{lpdoc uninstall} will install/uninstall all manuals in
all the selected formats in this directory and create and maintain the
corresponding @tt{html} and @tt{info} indices. Then, setting the
environment variables as follows (e.g., for @apl{csh} in @tt{.cshrc}):

@begin{verbatim}
setenv DOCDIR   /home/clip/public_html/lpdoc_docs
setenv INFOPATH /usr/local/info:$@{DOCDIR@}
setenv MANPATH  $@{DOCDIR@}:$@{MANPATH@}
@end{verbatim}

Example files for inclusion in user's or common shell initialization
files are included in the @tt{lpdoc} library.

More complex setups can be accommodated, as, for example, installing
different types of manuals in different directories. However, this
currently requires changing the @tt{docformats} and @tt{docdir}
variables and performing @tt{lpdoc install} for each installation
format/directory.

@section{Some usage tips}

This section contains additional suggestions on the use of
@apl{lpdoc}.

@subsection{Ensuring Compatibility with All Supported Target Formats}

One of the nice things about @apl{lpdoc} is that it allows
generating manuals in several formats which are quite different in
nature. Because these formats each have widely different requirements
it is sometimes a little tricky to get things to work successfully for
all formats. The following recommendations are intended to help in
achieving useful manuals in all formats:

@begin{itemize} 

@item The best results are obtained when documenting code organized as
a series of libraries, and with a well-designed module structure.

@item @apl{texinfo} supports only a limited number of indices. Thus,
if you select too many indices in the @file{SETTINGS.pl} file you may
exceed @apl{texinfo}'s capacity (which it will signal by saying
something like ``No room for a new @@write'').

@item The GNU info format requires all @em{nodes} (chapters, sections,
etc.) to have different names. This is ensured by @apl{lpdoc} for
the automatically generated sections (by appending the module or file
name to all section headings). However, care must be taken when
writing section names manually to make them different. For example,
use ``lpdoc usage'' instead of simply ``Usage'', which is much more
likely to be used as a section name in another file being documented.

@item Also due to a limitation of the @apl{info} format, do not use
@tt{:} or @tt{,} or @tt{-}@tt{-} in section, chapter, etc. headings.

@item The character ``@tt{_}'' in names may sometimes give problems in
indices, since current versions of @apl{texinfo} do not always handle
it correctly.

@end{itemize}

@subsection{Writing comments which document version/patch changes}

When writing version comments (@tt{:- doc(version(...),
""..."").}), it is useful to keep in mind that the text can often be
used to include in the manual a list of improvements made to the
software since the last time that it was distributed. For this to work
well, the textual comments should describe the significance of the
work done for the user. For example, it is more useful to write
""added support for @tt{pred} assertions"" than ""modifying file so
@tt{pred} case is also handled"". 

Sometimes one would like to write version comments which are internal,
i.e., not meant to appear in the manual. This can easily be done with
standard Prolog comments (which @apl{lpdoc} will not read). An
alternative and quite useful solution is to put such internal comments
in @em{patch} changes (e.g., 1.1#2 to 1.1#3), and put the more general
comments, which describe major changes to the user and should appear
in the manual, in @em{version} changes (e.g., 1.1#2 to
1.2#0). Selecting the appropriate options in @apl{lpdoc} then allows
including in the manual the version changes but not the patch changes
(which might on the other hand be included in an @index{internals
manual}).

@subsection{Documenting Libraries and/or Applications}

As mentioned before, for each a @tt{.pl} file, @apl{lpdoc} tries to
determine whether it is a library or the main file of an application,
and documents it accordingly. Any combination of libraries and/or main
files of applications can be used arbitrarily as components or main
files of a @apl{lpdoc} manual. Some typical combinations are:

@begin{itemize}

@item @em{Main file is a library, no components:} A manual of a simple
library, which appears externally as a single module. The manual
describes the purpose of the library and its interface.

@item @em{Main file is an application, no components:} A manual of a
simple application.

@item @em{Main file is a library, components are also libraries:} This
can be used for example for generating an @concept{internals manual}
of a library. The main file describes the purpose and use of the
library, while the components describe the internal modules of the
library.

@item @em{Main file is an application, components are libraries:} This
can be used similarly for generating an @concept{internals manual} of
an application. The main file describes the purpose and use of the
application, while the components describe the internal modules which
compose the application.

@item @em{Main file is a (pseudo-)application, components are
libraries:} A manual of a complex library made up of smaller libraries
(for example, the @apl{Prolog} library). The (pseudo-)application file
contains the introductory material (title, version, etc.).  Each
chapter describes a particular library.

@item @em{Main file is a (pseudo-)application, components are
applications:} This can be used to generate a manual of a set of
applications (e.g., a set of utilities). The (pseudo-)application file
contains the introductory material (title, version, etc.).  Each
chapter describes a particular component application.

@end{itemize}

@subsection{Documenting files which are not modules}

Sometimes it is difficult for @apl{lpdoc} to distinguish
@concept{include files} and Ciao @concept{packages} from normal
@em{user} files (i.e., normal code files but which are not
modules). The distinction is important because the former are quite
different in their form of use (they are loaded via @decl{include/1}
or @decl{use_package/1} declarations instead of
@decl{ensure_loaded/1}) and effect (since they are included, they
'export' operators, declarations, etc.), and should typically be
documented differently.  There is a special @decl{doc/2}
declaration (@tt{:- doc(filetype,...).}) which provides a way of
defining the intended use of the file. This declaration is normally
not needed in modules, include files, or packages, but should be added
in user files (i.e., those meant to be loaded using
@decl{ensure_loaded/1}). Adding this declaration will, for example,
avoid spurious documentation of the declarations in the
@lib{assertions} package themselves when this package is included in a
user file.

@subsection{Splitting large documents into parts}

As mentioned before, in @apl{lpdoc} each documented file (each
component) corresponds to a chapter in the generated manual. In large
documents, it is sometimes convenient to build a super-structure of
parts, each of which groups several chapters. There is a special value
of the second argument of the @tt{:- doc(filetype,...).}
declaration mentioned above designed for this purpose. The special
@em{filetype} value @tt{part} can be used to flag that the file in
which it appears should be documented as the start of one of the major
@index{parts in a large document}. In order to introduce such a part,
a @tt{.pl} file with a declaration @tt{:- doc(filetype,part).}
should be inserted in the sequence of files that make up the
@tt{components} variable of the @file{SETTINGS.pl} file at each point in
which a major part starts. The @tt{:- doc(title,""..."").}
declaration of this file will be used as the part title, and the
@tt{:- doc(module,""..."").}  declaration text will be used as the
introduction to the part.

@subsection{Documenting reexported predicates}

Reexported predicates, i.e., predicates which are exported by a module
@tt{m1} but defined in another module @tt{m2} which is used by
@tt{m1}, are normally not documented in the original module, but
instead a simple reference is included to the module in which it is
defined. This can be changed, so that the documentation is included in
the original module, by using a @decl{doc/2} declaration with
@tt{doinclude} in the first argument (see the @lib{comments} library).
This is often useful when documenting a library made of several
components.  For a simple user's manual, it is often sufficient to
include in the @apl{lpdoc} @file{SETTINGS.pl} file the principal
module, which is the one which users will do a @decl{use_module/1} of,
in the manual. This module typically exports or reexports all the
predicates which define the library's user interface.  Note, however,
that currently, due to limitations in the implementation, only the
comments inside @concept{assertions} (but not those in
@decl{doc/2} declarations) are included for reexported predicates.

@subsection{Separating the documentation from the source file}

Sometimes one would not like to include long introductory comments in
the module itself but would rather have them in a different file. This
can be done quite simply by using the @@include command. For example,
the following declaration:

@begin{verbatim}
:- doc(module,""@@include@{Intro.lpdoc@}"").
@end{verbatim}

@noindent will include the contents of the file
@tt{Intro.lpdoc} as the module description.

Alternatively, sometimes one may want to generate the documentation from
a completely different file. Assuming that the original module is
@tt{m1.pl}, this can be done by calling the module containing the
documentation @tt{m1_doc.pl}. This @tt{m1_doc.pl} file is the one that
will be included in the @apl{lpdoc} @file{SETTINGS.pl} file, instead
of @tt{m1.pl}. @apl{lpdoc} recognizes and treats such @tt{_doc} files
specially so that the name without the @tt{_doc} part is used in the
different parts of the documentation, in the same way as if the
documentation were placed in file @tt{m1}.

@subsection{Generating auxiliary files (e.g. READMEs)}

@begin{alert}   
@bf{Note: significant parts of this are obsolete.  They must be
updated to describe lpdoc version 2.0.}
@end{alert}

Using @apl{lpdoc} it is often possible to use a common source for
documentation text which should appear in several places. For example,
assume a file @file{INSTALLATION.lpdoc} contains text (with @tt{lpdoc}
formatting commands) describing an application. This text can be
included in a section of the main file documentation as follows:

@begin{verbatim}
:- doc(module,""
   ...
   @@section@{Installation instructions@}
   @@include@{INSTALLATION.lpdoc@}
   ...
   "").
@end{verbatim}

@noindent At the same time, this text can be used to generate a nicely
formatted @tt{INSTALLATION} file in ascii, which can perhaps be included in
the top level of the source directory of the application. To this end, an
@tt{INSTALL.pl} file as follows can be constructed: 

@begin{verbatim}
:- use_package([assertions]).
:- doc(filetype, application). %% forces file to be documented as an application
:- doc(title,""Installation instructions"").
:- doc(module,""@@include@{INSTALLATION.lpdoc@}"").
@end{verbatim}

Then, the ascii @tt{INSTALLATION} file can be generated by simply 
running @tt{lpdoc ascii} in a directory with a 
@tt{SETTINGS.pl} file where @tt{MAIN} is set to @tt{INSTALLATION.pl}.

@section{Troubleshooting}

These are some common errors which may be found using @apl{lpdoc}
and the usual fix:

@begin{itemize}

@item  Sometimes, messages of the type: 
@begin{verbatim}
  gmake: *** No rule to make target `myfile.texic', needed by
  `main.texic'.  Stop.
@end{verbatim}
appear (i.e., in the case above when running (@tt{g})@tt{make}
@tt{main.}@em{target}).  Since @apl{lpdoc} definitely knows how to
make a @tt{.texic} file given a @tt{.pl} file, this means (in make's
language) that it @em{cannot find the corresponding} @tt{.pl}
@em{file} (@tt{myfile.pl} in the case above). The usual reason for
this is that there is no directory path to this file declared in the
@tt{SETTINGS.pl} file.

@item Messages of the type:
@begin{verbatim}

 ! No room for a new @@write .

@end{verbatim} while converting from @tt{.texi} to @tt{.dvi} (i.e.,
while running @apl{tex}). These messages are @apl{tex}'s way of saying
that an internal area (typically for an index) is full. This is
normally because more indices were selected in the @tt{INDICES}
variable of the @tt{SETTINGS.pl} file than the maximum number
supported by the installed version of @apl{tex}/@apl{texinfo}
installations, as mentioned in @ref{Generating a manual}. The easiest
fix is to reduce the number of indices generated.  Alternatively, it
may be possible to recompile your local @apl{tex}/@apl{texinfo}
installation with a higher number of indices.

@item Missing links in @apl{info} files (a section which exists in the
printed document cannot be accessed in the on-line document) can be
due to the presence of a colon (@tt{:}), a comma (@tt{,}), a double
dash (@tt{--}), or other such separators in a section name. Due to
limitations of @apl{info} section names cannot contain these symbols.

@item Menu listings in @apl{info} which @em{do not work} (i.e., the
menu listings are there, but they cannot be followed): see if they are
indented. In that case it is due to an @tt{itemize} or @tt{enumerate}
which was not closed.

@end{itemize}

").

% :- doc(bug, "Missing documentation for @tt{documentation} filetype. It
% behaves like @tt{part} but does not split the document.").


