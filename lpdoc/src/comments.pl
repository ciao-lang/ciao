:- module(comments,
	[
	    %% doc/2,
            docstring/1,stringcommand/1,version_descriptor/1,filetype/1
	],
	[dcg,assertions,regtypes,fsyntax]).

:- use_module(library(strings), [string/3]).

%% Actually, already in assertions
%% :- new_declaration(doc/2). 

:- doc(bug, "I think that There should be a dependency from here to
   autodoc_doctree.  This is the textual representation of a doctree
   (JFMC)").

:- doc(title,"Documentation Mark-up Language and Declarations").
% TODO: The following title looks more like a subtitle
%:- doc(title,"Enhancing Documentation with Machine-Readable Comments").

:- doc(author,"Manuel Hermenegildo").

:- doc(module,"This defines the admissible uses of the
   @decl{doc/2} declaration (which is used mainly for adding
   @concept{machine readable comments} to programs), the
   @concept{formatting commands} which can be used in the text strings
   inside these comments, and some related properties and data
   types. These declarations are ignored by the compiler in the same
   way as classical comments.  Thus, they can be used to document the
   program source in place of (or in combination with) the normal
   comments typically inserted in the code by programmers. However,
   because they are more structured and they are machine-readable,
   they can also be used to generate printed or on-line documentation
   automatically, using the @apl{lpdoc} automatic documentation
   generator.  These @index{textual comments} are meant to be
   complementary to the formal statements present in
   @index{assertions} (see the @lib{assertions} library).

   ").

:- doc(usage,"It is not necessary to use this library in user
   programs.  The recommended procedure in order to make use of the
   @decl{doc/2} declarations that this library defines is to
   include instead the @lib{assertions} package, which provides
   efficient support for all assertion- and comment-related
   declarations, using one of the following declarations, as appropriate:

@begin{verbatim}
   :- module(...,...,[assertions]).
   :- use_package(assertions).
@end{verbatim}
   ").

% ----------------------------------------------------------------------------
% Comment-related definitions
% ----------------------------------------------------------------------------

% This forces documentation (otherwise, it stays undocumented!).
:- doc(doinclude,doc/2).

% A definition to provide meta-information about 'doc' declarations. 
% TODO: Ideally, this should be written in the assertions
:- export(doc_id_type/3).
:- discontiguous(doc_id_type/3).
% Typing for each doc directive:
%   doc_id_type(Id, Type, ValueType)
% TODO: This should be part of comments.pl

:- doc(doc/2,"This declaration provides one of the main means
   for adding @index{machine readable comments} to programs (the
   other one is adding @index{documentation strings} to assertions).").

doc_id_type(title, single, docstr). % warning, note (manpage)
:- decl doc(CommentType,TitleText) => =(title) * docstring

   # "Provides a @index{title} for the module, library, or
     application. When generating documentation automatically, the
     text in @var{TitleText} will be used appropriately (e.g., in the
     cover page as document title or as chapter title if part of a
     larger document). This will also be used as a brief description
     of the manual in on-line indices. There should be at most one of
     these declarations per module.

     @item @em{Example:}
@begin{verbatim}
:- doc(title,""Documentation-Oriented Assertions"").
@end{verbatim}
     ".

doc_id_type(subtitle, multiple, docstr). % ignore
:- decl doc(CommentType,SubtitleText) => =(subtitle) * docstring

   # "Provides a @index{subtitle}, an explanatory or alternate
     @index{title}. The subtitle will be displayed under the proper
     title.

     @item @em{Example:}
@begin{verbatim}
:- doc(title,""Dr. Strangelove"").
:- doc(subtitle,""How I Learned to Stop Worrying and Love the Bomb"").
@end{verbatim}
     ".

doc_id_type(subtitle_extra, multiple, docstr). % ignore
:- decl doc(CommentType,SubtitleText) => =(subtitle_extra) * docstring

   # "Provides additional @index{subtitle} lines. This can be, e.g., an
     explanation of the application to add to the title, the address
     of the author(s) of the application, etc. When generating
     documentation automatically, the text in @var{SubtitleText} will
     be used accordingly. Several of these declarations can appear per
     module, which is useful for, e.g., multiple line addresses.

     @item @em{Example:}
@begin{verbatim}
:- doc(subtitle_extra,""A Reference Manual"").
:- doc(subtitle_extra,""Technical Report 1/1.0"").
@end{verbatim}
     ".

% TODO: Refine types
% TODO: This could specify the logo of manual subparts/components.
doc_id_type(logo, single, term). % ignore
:- decl doc(CommentType,SubtitleText) => =(logo) * term

   # "The name of the logo image for the manual.".

doc_id_type(author, multiple, docstr). % warning
:- decl doc(CommentType,AuthorText) => =(author) * docstring

   # "Provides the @index{author}(s) of the module or application. If
     present, when generating documentation for the module
     automatically, the text in @var{AuthorText} will be placed in the
     corresponding chapter or front page. There can be more than one
     of these declarations per module. In order for @concept{author
     indexing} to work properly, please use one author declaration per
     author. If more explanation is needed (who did what when, etc.)
     use an acknowledgements comment.

     @item @em{Example:}
@begin{verbatim}
:- doc(author,""Alan Robinson"").
@end{verbatim}
     ".

doc_id_type(address, multiple, docstr). % ignore
:- decl doc(CommentType,Text) => =(address) * docstring

   # "Provides the physical and electronic @index{address}, or any
     other contact information for the authors of the module or
     application.

     @item @em{Example:}
@begin{verbatim}
:- doc(address,""Syracuse University"").
@end{verbatim}
     ".

doc_id_type(credits, multiple, docstr). % ignore
%
% % TODO: Undocumented since I am not sure if this is the best way to
% % do it. Maybe 'editors' is better.
%
% :- decl doc(CommentType,Text) => =(credits) * docstring
% 
%     # "Additional documentation that follows to the
%       authors. Contrary the @index{author} declaration, its text is not
%       indexed nor documented as author names.
%  
%       @item @em{Example:}
%  @begin{verbatim}
%  :- doc(credits,""@em{Edited by:}"").
%  :- doc(credits,""Someone"").
%  @end{verbatim}
%       ".

doc_id_type(ack,      single, docstr). % ignore
:- decl doc(CommentType,AckText) => =(ack) * docstring

   # "Provides @index{acknowledgements} for the module. If present,
     when generating documentation for the module automatically, the
     text in @var{AckText} will be placed in the corresponding chapter
     or section. There can be only one of these declarations per
     module.

     @item @em{Example:}
@begin{verbatim}
:- doc(ack,""Module was written by Alan, but others helped."").
@end{verbatim}
     ".

doc_id_type(copyright, single, docstr). % ignore,note
:- decl doc(CommentType,CopyrightText) => =(copyright) * docstring

   # "Provides a @index{copyright} text. This normally appears
     somewhere towards the beginning of a printed manual. There should
     be at most one of these declarations per module.

     @item @em{Example:}
@begin{verbatim}
:- doc(copyright,""Copyright @copyright{} 2001 FSF."").
@end{verbatim}
     ".

doc_id_type(summary,   single, docstr). % ignore,note
:- decl doc(CommentType,SummaryText) => =(summary) * docstring

   # "Provides a brief global explanation of the application or
     library. The text in @var{SummaryText} will be used as the
     @index{abstract} for the whole manual. There should be at most
     one of these declarations per module.

     @item @em{Example:}
@begin{verbatim}
:- doc(summary,""This is a @@bf@{very@} important library."").
@end{verbatim}
     ".

doc_id_type(module, single, docstr). % note
:- decl doc(CommentType,CommentText) => =(module) * docstring

   # "Provides the main comment text for the module or
     application. When generating documentation automatically, the
     text in @var{CommentText} will be used as the
     @index{introduction} or @index{main body} @cindex{module comment}
     of the corresponding chapter or manual. There should be at most
     one of these declarations per module. @var{CommentText} may use
     @bf{sections} if substructure is needed.

     @item @em{Example:}
@begin{verbatim}
:- doc(module,""This module is the @@lib@{comments@} library.""). 
@end{verbatim}
     ".

doc_id_type(appendix, single, docstr). % ignore
:- decl doc(CommentType,CommentText) => =(appendix) * docstring

   # "Provides additional comments text for a module or
     application. When generating documentation automatically, the
     text in @var{CommentText} will be used in one of the last
     sections or appendices @cindex{appendix} of the corresponding
     chapter or manual. There should be at most one of these
     declarations per module. @var{CommentText} may use @bf{subsections} if
     substructure is needed.

     @item @em{Example:}
@begin{verbatim}
:- doc(appendix,""Other module functionality...""). 
@end{verbatim}
     ".

doc_id_type(usage, single, docstr). % ignore
:- decl doc(CommentType,CommentText) => =(usage) * docstring

   # "Provides a description of how the library should be
     loaded. Normally, this information is gathered automatically when
     generating documentation automatically. This declaration is meant
     for use when the module needs to be treated in some special way.
     There should be at most one of these declarations per module.

     @item @em{Example:}
@begin{verbatim}
:- doc(usage,""Do not use: still in development!""). 
@end{verbatim}
     ".

doc_id_type(section, single, docstr). % dofail
:- decl doc(CommentType,Section) => =(section) * docstring

   # "Insert a @index{program section} with name
      @var{Section}. Sectioning commands allow a structured separation
      of the program into parts. The division is only for
      documentation purposes, so visibility and scope of definitions
      is not affected by sectioning commands.

     @item @em{Example:}
@begin{verbatim}
:- doc(section,""Main Steps of the Algorithm"").
@end{verbatim}
     ".

doc_id_type(subsection, single, docstr). % dofail
:- decl doc(CommentType,SubSection) => =(subsection) * docstring

   # "Insert a @index{program subsection} with name @var{SubSection}
     (see @index{program section} command for more details).

     @item @em{Example:}
@begin{verbatim}
:- doc(subsection,""Auxiliary Definitions"").
@end{verbatim}
     ".

doc_id_type(subsubsection, single, docstr). % dofail
:- decl doc(CommentType,SubSubSection) => =(subsubsection) * docstring

   # "Insert a @index{program subsubsection} with name
     @var{SubSubSection} (see @index{program section} command for more
     details).

     @item @em{Example:}
@begin{verbatim}
:- doc(subsubsection,""Auxiliary Definitions"").
@end{verbatim}
     ".

doc_id_type(pred(_), single, docstr). % dofail
:- decl doc(PredName,CommentText) => predname * docstring

   # "Provides an introductory comment for a given predicate,
     function, property, type, etc., denoted by @var{PredName}. When
     generating documentation for the module automatically, the text
     in @var{Text} will be used as the introduction of the
     corresponding predicate/function/... description. There should be
     at most one of these declarations per predicate, function,
     property, or type.

     @item @em{Example:}
@begin{verbatim}
:- doc(doc/2,""This declaration provides one of the main 
   means for adding @@concept@{machine readable comments@} to 
   programs.""). 
@end{verbatim}
     ".

doc_id_type(bug, multiple, docstr). % ignore
:- decl doc(CommentType,CommentText) => =(bug) * docstring

   # "Documents a known @index{bug} or @index{planned improvement}
     in the module or application. Several of these declarations can
     appear per module. When generating documentation automatically,
     the text in the @var{Text} fields will be used as items in an
     itemized list of module or application bugs.

     @item @em{Example:}
@begin{verbatim}
:- doc(bug,""Comment text still has to be written by user."").
@end{verbatim}
     ".

doc_id_type(version(_,_), single, docstr).
doc_id_type(version(_,_,_), single, docstr).
:- decl doc(Version,CommentText) => version_descriptor * docstring

   # "Provides a means for keeping a @index{log of
     changes}. @var{Version} contains the @index{version number} and
     date corresponding to the change and @var{CommentText} an
     explanation of the change. Several of these declarations can
     appear per module. When generating documentation automatically,
     the texts in the different @var{CommentText} fields typically
     appear as items in an itemized list of changes. The
     @concept{emacs Ciao mode} helps tracking version numbers by
     prompting for version comments when files are saved. This mode
     requires version comments to appear in reverse chronological
     order (i.e., the topmost comment should be the most recent
     one).

     @item @em{Example:}
@begin{verbatim}
:- doc(version(1*1+21,1998/04/18,15:05*01+'EST'), ""Added some
   missing comments.  (Manuel Hermenegildo)"").
@end{verbatim}
     ".

doc_id_type(version_maintenance, single, docstr).
:- decl doc(CommentType,VersionMaintenanceType) 
   => =(version_maintenance) * version_maintenance_type

   # "Defines the type of version maintenance that should be performed
      by the @apl{emacs} Ciao mode. 

     @item @em{Example:}
@begin{verbatim}
:- doc(version_maintenance,dir('../version')).
@end{verbatim}

     Version control info is kept in directory @tt{../version}. See
     the definition of @pred{version_maintenance_type/1} for more
     information on the different version maintenance modes. See the
     documentation on the @index{emacs Ciao mode} in the Ciao manual
     for information on how to automatically insert version control
     @decl{doc/2} declarations in files.

     The version maintenance mode can also be set alternatively by
     inserting a comment such as:

@begin{verbatim}
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: ""off""
%% End:
@end{verbatim}

      The lines above instruct emacs to put the buffer visiting the
      file in @concept{emacs Ciao mode} and to turn version
      maintenance off.  Setting the version maintenance mode in this
      way has the disadvantage that @apl{lpdoc} will not be aware of
      the type of version maintenance being performed (the lines above
      are comments for Prolog). However, this can be useful in fact
      for setting the @index{version maintenance mode for packages}
      and other files meant for inclusion in other files, since that
      way the settings will not affect the file in which the package
      is included.

".

doc_id_type(doinclude, multiple, term). % ignore
:- decl doc(CommentType,PredName) => =(doinclude) * predname

   # "This is a special case that is used to control which predicates
     are included in the documentation. Normally, only exported
     predicates are documented. A declaration @tt{:-
     doc(doinclude,}@var{PredName}@tt{).} forces documentation for
     predicate (or type, property, function, ...)  @var{PredName} to be
     included even if @var{PredName} is not exported. Also, if
     @var{PredName} is reexported from another module, a declaration
     @tt{:- doc(doinclude,}@var{PredName}@tt{).} will force the
     documenation for @var{PredName} to appear directly in this
     module.

     @item @em{Example:}
@begin{verbatim}
:- doc(doinclude,p/3).
@end{verbatim}
     ".

:- decl doc(CommentType,PredName) => =(doinclude) * list(predname)

   # "A different usage which allows the second argument of @tt{:-
     doc(doinclude,...)} to be a list of predicate names.".

doc_id_type(hide, multiple, term).
:- decl doc(CommentType,PredName) => =(hide) * predname

   # "This is similar to the previous usage but has the opposite
     effect: it signals that an exported predicate should @em{not} be
     included in the documentation.

     @item @em{Example:}
@begin{verbatim}
:- doc(hide,p/3).
@end{verbatim}
     ".

:- decl doc(CommentType,PredName) => =(hide) * list(predname)

   # "A different usage which allows the second argument of @tt{:-
     doc(hide,...)} to be a list of predicate names.".

doc_id_type(filetype, single, term). % dofail
:- decl doc(CommentType,FileType) => =(filetype) * filetype

   # "Provides a way of defining the intended use of the file. This
     use is normally easily inferred from the contents of the file
     itself, and therefore such a declaration is in general not
     needed. The exception is the special case of @concept{include
     files} and Ciao @concept{packages}, which are typically
     indistiguishable from normal @em{user} files (i.e., files which
     are not modules), but are however quite different in their form
     of use (they are loaded via @decl{include/1} or
     @decl{use_package/1} declarations instead of
     @decl{ensure_loaded/1}) and effect (since they are included, they
     'export' operators, declarations, etc.). Typically, it is assumed
     by default that files which are not modules will be used as
     include files or packages. Thus, a @decl{doc/2} declaration
     of this kind strictly only needs to be added to user-type files.

     @em{Example:}
@begin{verbatim}
:- doc(filetype,user).
@end{verbatim}

     There is another special case: the value @tt{part}. This
     @em{filetype} is used to flag files which serve as introductions
     to boundaries between major @index{parts in large
     documents}. See @ref{Splitting large documents into parts} for
     details.

     ".

doc_id_type(nodoc, multiple, term). % ignore
:- decl doc(CommentType,FileName) => =(nodoc) * atm

   # "Do not document anything that comes from a file whose name
      (after taking away the path and the suffix) is
      @var{FileName}. This is used for example when documenting
      packages to avoid the documenter from including documentation of
      certain other packages which the package being documented uses.

     @item @em{Example:}
@begin{verbatim}
:- doc(nodoc,assertions).
@end{verbatim}
     ".

% TODO: undocumented
doc_id_type(pragma, multiple, term). % dofail
% :- decl doc(CommentType,FileName) => =(pragma) * term
% 
%    # "Internal documentation parameter for this module.
% 
%      @item @em{Example:}
% @begin{verbatim}
% :- doc(pragma,...).
% @end{verbatim}
%      ".

% ----------------------------------------------------------------------------
% Types and properties
% ----------------------------------------------------------------------------

:- doc(version_descriptor/1,"A structure denoting a complete
   version description: @includedef{version_descriptor/1}").

:- regtype version_descriptor(Descriptor) 
   # "@var{Descriptor} is a complete version descriptor.".

version_descriptor([]). % If no version available
version_descriptor(version(Version,Date)) :-
	version_number(Version),
	ymd_date(Date).
version_descriptor(version(Version,Date,Time)) :-
	version_number(Version),
	ymd_date(Date),
	time_struct(Time).

:- doc(filetype/1,"Intended uses of a file: @includedef{filetype/1}"). 

:- regtype filetype(Type) 
   # "@var{Type} describes the intended use of a file.".

filetype(module).
filetype(user).
filetype(include).
filetype(package).
filetype(part).

% ----------------------------------------------------------------------------

:- doc(doinclude,version_number/1).

:- doc(version_number/1,"@var{Version} is a structure
   denoting a complete version number (major version, minor version,
   and patch number): 

   @includedef{version_number/1}

   ").

:- regtype version_number(Version) 
   # "@var{Version} is a complete version number".

version_number(Major*Minor+Patch) :-
	int(Major),
	int(Minor),
	int(Patch).

% ----------------------------------------------------------------------------

:- doc(doinclude,ymd_date/1).

:- doc(ymd_date/1,"A Year/Month/Day structure denoting a date:
      @includedef{ymd_date/1}.").

:- regtype ymd_date(Date) 
   # "@var{Date} is a Year/Month/Day structure denoting a date.".

ymd_date(Y/M/D) :-
	int(Y),
	int(M),
	int(D).

% ----------------------------------------------------------------------------

:- doc(doinclude,time_struct/1).

:- doc(time_struct/1,"A struture containing time information:
   @includedef{time_struct/1}").

:- regtype time_struct(Time) # "@var{Time} contains time information.".

time_struct(Hours:Minutes*Seconds+TimeZone) :-
	int(Hours),
	int(Minutes),
	int(Seconds),
	atm(TimeZone).

% ----------------------------------------------------------------------------

:- doc(doinclude,version_maintenance_type/1).

:- doc(version_maintenance_type/1,"Possible kinds of version
   maintenance for a file: 

   @includedef{version_maintenance_type/1}

   @begin{itemize}

   @item @tt{on}: version numbering is maintained locally in the file
   in which the declaration occurs, i.e., an independent version
   number is kept for this file and the current version is given by
   the most recent @tt{doc(version(...),...)}  declaration.

   @item @tt{off}: no version numbering maintained.

   @item @tt{dir(Path)}: version numbering is maintained (globally) in
   directory @var{Path}. This is useful for maintaining a common
   global version for an application which involves several files.

   @end{itemize}

   The automatic maintenance of version numbers is typically done by
   the Ciao @apl{emacs} mode.

  ").

:- regtype version_maintenance_type(Type) # "@var{Type} a type of
   version maintenance for a file.".

version_maintenance_type(on).
version_maintenance_type(off).
version_maintenance_type(dir(Path)) :-
	atm(Path).

% ----------------------------------------------------------------------------

:- doc(doinclude,docstring/1).

:- doc(docstring/1,"Defines the format of the character strings
   which can be used in machine readable comments (@decl{doc/2}
   declarations) and assertions. These character strings can include
   certain @index{formatting commands}.

   @begin{itemize}

   @item All printable characters are admissible in documentation
   strings except ``@tt{@@}'', ``@tt{@{},'' and ``@tt{@}}''. To
   produce these characters the following @index{escape sequences}
   should be used, respectively: @tt{@@@@}, @tt{@@@{}, and @tt{@@@}}.

   @item In order to allow better formatting of on-line and printed
   manuals, in addition to normal text, certain formatting commands
   can be used within these strings. The syntax @cindex{syntax of
   formatting commands} of all these commands is:

   @tt{@@}@em{command} 

   (followed by either a space or @tt{@{@}}), or 

   @tt{@@}@em{command}@tt{@{}@em{body}@tt{@}} 

   where @em{command} is the command name and @em{body} is the
   (possibly empty) command body.

   The set of commands currently admitted can be found in the
   documentation for the predicate @pred{stringcommand/1}.

   @end{itemize}

").

:- prop docstring(Text) # "@var{Text} is a @em{documentation string}.".

docstring(CO) :- 
	docstring(CO,[]).

:- set_prolog_flag(multi_arity_warnings, off).

docstring --> "" 
          |   normalchar, !, docstring 
          |   escapeseq, !, docstring
          |   "@", string(Command), " ", 
              {atom_codes(Com, Command), C =.. [Com,""], stringcommand(C) }, !,
                docstring 
          |   "@", string(Command), "{", string(Body), "}", 
              {atom_codes(Com, Command), C =.. [Com,Body], stringcommand(C) },
                docstring.

:- set_prolog_flag(multi_arity_warnings, on).

normalchar --> [X], {X \== 0'@, X \== 0'{, X \== 0'} }.

escapeseq --> "@{" 
          |   "@}" 
          |   "@@".


:- doc(doinclude,stringcommand/1).

:- prop stringcommand(CO) # "@var{CO} is a structure denoting a
   command that is admissible in strings inside assertions.".

:- doc(stringcommand/1,"Defines the set of structures which can
   result from parsing a formatting command admissible in comment
   strings inside assertions.

   In order to make it possible to produce documentation in a wide
   variety of formats, the command set is kept small. The names of the
   commands are intended to be reminiscent of the commands used in the
   @concept{LaTeX} text formatting system, except that ``@tt{@@}'' is
   used instead of ``@tt{\\}.'' Note that @tt{\\} would need to be
   escaped in ISO-Prolog strings, which would make the source less
   readable (and, in any case, many ideas in LaTeX were taken from
   @concept{scribe}, where the escape character was indeed @tt{@@}!).

   The following are the currently admissible commands. 

   @begin{itemize}

   @item @bf{Formatting commands:} 

   The following commands are used to format certain words or
   sentences in a special font, build itemized lists, introduce
   sections, include examples, etc.

   @begin{description}

   @item{@tt{@@comment@{}@em{text}@tt{@}}} @cindex{@@comment command}
   @em{text} will be treated as a @index{comment} and will be ignored.

   @item{@tt{@@begin@{itemize@}}} @cindex{@@begin@{itemize@} command}
   marks the beginning of an @index{itemized list}. Each item should
   be in a separate paragraph and preceded by an @tt{@@item} command.

   @item{@tt{@@item}} @cindex{@@item command} marks the
   beginning of a new @index{item in an itemized list}.

   @item{@tt{@@end@{itemize@}}} @cindex{@@end@{itemize@} command}
   marks the end of an itemized list.

   @item{@tt{@@begin@{enumerate@}}} @cindex{@@begin@{enumerate@}
   command} marks the beginning of an @index{enumerated list}. Each
   item should be in a separate paragraph and preceded by an
   @tt{@@item} command.

   @item{@tt{@@end@{enumerate@}}} @cindex{@@end@{enumerate@}
   command} marks the end of an enumerated list. 

   @item{@tt{@@begin@{description@}}} @cindex{@@begin@{description@}
   command} marks the beginning of a @index{description list}, i.e., a
   list of items and their description (this list describing the
   different allowable commads is in fact a description list). Each
   item should be in a separate paragraph and contained in an
   @tt{@@item@{}@em{itemtext}@tt{@}} command.

   @item{@tt{@@item@{}@em{itemtext}@tt{@}}} @cindex{@@item command}
   marks the beginning of a @index{new item in description list}, and
   contains the header for the item.

   @item{@tt{@@end@{description@}}} @cindex{@@end@{description@}
   command} marks the end of a description list. 

   @item{@tt{@@begin@{verbatim@}}} @cindex{@@begin@{verbatim@}
   command} marks the beginning of @index{fixed format text},
   @cindex{verbatim text} such as a program example. A fixed-width,
   typewriter-like font is used.

   @item{@tt{@@end@{verbatim@}}} @cindex{@@end@{verbatim@} command}
   marks the end of formatted text.

   @item{@tt{@@begin@{cartouche@}}} @cindex{@@begin@{cartouche@}
   command} marks the beginning of a section of text in a
   @index{framed box}, with round corners.

   @item{@tt{@@end@{cartouche@}}} @cindex{@@end@{cartouche@} command}
   marks the end of a section of text in a @concept{framed box}.

   @item{@tt{@@begin@{alert@}}} @cindex{@@begin@{alert@}
   command} marks the beginning of a section of text in a
   @index{framed box}, for alert messages.

   @item{@tt{@@end@{alert@}}} @cindex{@@end@{alert@} command}
   marks the end of the alert message.

   @item{@tt{@@section@{}@em{text}@tt{@}}} @cindex{@@section command}
   starts a @index{section} whose title is @em{text}. Due to a limitation of
   the @apl{info} format, do not use @tt{:} or @tt{-} or @tt{,} in
   section, subsection, title (chapter), etc. headings.

   @item{@tt{@@subsection@{}@em{text}@tt{@}}} @cindex{@@subsection
   command} starts a @index{subsection} whose title is @em{text}.

   @item{@tt{@@subsubsection@{}@em{text}@tt{@}}}
   @cindex{@@subsubsection command} starts a @index{subsubsection}
   whose title is @em{text}.

   @item{@tt{@@footnote@{}@em{text}@tt{@}}} @cindex{@@footnote
   command} places @em{text} in a @index{footnote}.

   @item{@tt{@@hfill}} @cindex{@@hfill command} introduces horizontal
   filling space @cindex{spcae, horizontal fill} (may be ignored in
   certain formats).

   @item{@tt{@@bf@{}@em{text}@tt{@}}} @cindex{@@bf command} @em{text}
   will be formatted in @index{bold face} or any other @index{strong
   face}.

   @item{@tt{@@em@{}@em{text}@tt{@}}} @cindex{@@em command} @em{text}
   will be formatted in @index{italics face} or any other
   @index{emphasis face}.

   @item{@tt{@@tt@{}@em{text}@tt{@}}} @cindex{@@tt command} @em{text}
   will be formatted in a @index{fixed-width font} (i.e.,
   @index{typewriter-like font}).

   @item{@tt{@@key@{}@em{key}@tt{@}}} @cindex{@@key command} @em{key}
   is the identifier of a @index{keyboard key} (i.e., a letter such as
   @tt{a}, or a special key identifier such as @tt{RET} or @tt{DEL})
   and will be formatted as @key{LFD} or in a fixed-width,
   typewriter-like font.

   @item{@tt{@@sp@{}@em{N}@tt{@}}} @cindex{@@sp command} generates
   @em{N} @index{blank lines} of space. @cindex{space, extra lines}
   Forces also a paragraph break.

   @item{@tt{@@p}} @cindex{@@p command} forces a @index{paragraph
   break}, in the same way as leaving one or more blank lines.

   @item{@tt{@@noindent}} @cindex{@@noindent command} used at the
   beginning of a paragraph, states that the first line of the
   paragraph should not be indented. @cindex{indentation, avoiding}
   Useful, for example, for @index{avoiding indentation} on paragraphs
   that are continuations of other paragraphs, such as after a
   verbatim.

   @end{description}

   @item @bf{Indexing commands:} 

   The following commands are used to
   mark certain words or sentences in the text as concepts, names of
   predicates, libraries, files, etc. The use of these commands is
   highly recommended, since it results in very useful indices with
   little effort.

   @begin{description}

   @item{@tt{@@index@{}@em{text}@tt{@}}} @cindex{@@index command}
   @em{text} will be printed in an emphasized font and will be
   included in the concept definition index (and also in the usage
   index). This command should be used for the first or
   @em{definitional} appearance(s) of a concept. The idea is that the
   concept definition index can be used to find the definition(s) of a
   concept.

   @item{@tt{@@cindex@{}@em{text}@tt{@}}} @cindex{@@cindex command}
   @em{text} will be included in the concept index (and also in the
   usage index), but it is not printed. This is used in the same way
   as above, but allows sending to the index a different text than the
   one that is printed in the text.

   @item{@tt{@@concept@{}@em{text}@tt{@}}} @cindex{@@concept command}
   @em{text} will be printed (in a normal font). This command is used
   to mark that some text is a defined concept. In on-line manuals, a
   direct access to the corresponding concept definition may also be
   generated.  A pointer to the place in which the @@concept command
   occurs will appear only in the usage index.

   @item{@tt{@@pred@{}@em{predname}@tt{@}}} @cindex{@@pred command}
   @em{predname} (which should be in functor/arity form) is the name
   of a predicate and will be printed in fixed-width, typewriter-like
   font. This command should be used when referring to a predicate (or
   a property or type) in a documentation string. A reference will be
   included in the usage index. In on-line manuals, a direct access to
   the corresponding predicate definition may also be generated.

   @item{@tt{@@op@{}@em{operatorname}@tt{@}}} @cindex{@@op command}
   @em{operatorname} (which should be in functor/arity form) is the
   name of an operator and will be printed in fixed-width,
   typewriter-like font. This command should be used when referring to
   an operator in a documentation string. A reference will be included
   in the usage index. In on-line manuals, a direct access to the
   corresponding operator definition may also be generated.

   @item{@tt{@@decl@{}@em{declname}@tt{@}}} @cindex{@@decl command}
   @em{declname} (which should be in functor/arity form) is the name
   of a declaration and will be printed in fixed-width,
   typewriter-like font. This command should be used when referring to
   a declaration in a documentation string. A reference will be
   included in the usage index. In on-line manuals, a direct access to
   the corresponding declaration definition may also be generated.

   @item{@tt{@@lib@{}@em{libname}@tt{@}}} @cindex{@@lib command}
   @em{libname} is the name of a library and will be printed in
   fixed-width, typewriter-like font. This command should be used when
   referring to a module or library in a documentation string. A
   reference will be included in the usage index. In on-line manuals,
   a direct access to the corresponding module definition may also be
   generated.

   @item{@tt{@@apl@{}@em{aplname}@tt{@}}} @cindex{@@apl command}
   @em{aplname} is the name of an application and will be printed in
   fixed-width, typewriter-like font. This command should be used when
   referring to an application in a documentation string. A reference
   will be included in the usage index. 

   @item{@tt{@@file@{}@em{filename}@tt{@}}} @cindex{@@file command}
   @em{filename} is the name of a file and will be printed in
   fixed-width, typewriter-like font. This command should be used when
   referring to a file in a documentation string. A reference will be
   included in the usage index.

   @item{@tt{@@var@{}@em{varname}@tt{@}}} @cindex{@@var command}
   @em{varname} is the name of a variable and will be formatted in an
   emphasized font. Note that when referring to variable names in a
   @pred{pred/1} declaration, such names should be enclosed in
   @tt{@@var} commands for the automatic documentation system to work
   correctly.

   @end{description}

   @item @bf{Referencing commands:} 

   The following commands are used to
   introduce @index{bibliographic citations} and @index{references} to
   @index{sections}, @index{urls}, @index{email addresses}, etc.

   @begin{description}

   @item{@tt{@@cite@{@em{keyword}@}}} @cindex{@@cite command}
   @em{keyword} is the identifier of a @index{bibliographic entry}.
   Such entry is assumed to reside in on of a number of @apl{bibtex}
   files (@index{.bib files}) @cindex{bibtex}. A reference in brackets
   (@bf{[ ]}) is inserted in the text an the full reference is
   included at the end, with all other references, in an appendix. For
   example, @tt{@@cite@{iso-prolog@}} will introduce a citation to a
   bibliographic entry whose keyword is @tt{iso-prolog}. The list of
   bibliography files which will be searched for a match is determined
   by @tt{bibfile/1} fact of the @apl{lpdoc} @file{SETTINGS} file.

   @item{@tt{@@ref@{@em{section title}@}}} @cindex{@@ref command}
   introduces at point a reference to the section or node @em{section
   title}, where @em{section title} must be the exact @em{text} of the
   section title. 

   @item{@tt{@@href@{@em{URL}@}}} @cindex{@@href command}
   introduces at point a reference to the @index{Universal Resource
   Locator} (i.e., a @index{WWW address} '@index{URL}'.

   @item{@tt{@@href@{}@em{URL}@tt{@}@{}@em{text}@tt{@}}}
   @cindex{@@href command} introduces at point a reference to the
   @concept{Universal Resource Locator} @concept{URL}, associated to
   the text @em{text}.

   @item{@tt{@@email@{}@em{address}@tt{@}}} @cindex{@@email command}
   introduces at point a reference to @index{email address}
   @em{address}.

   @item{@tt{@@email@{}@em{text}@tt{@}@{}@em{address}@tt{@}}}
   @cindex{@@email command} introduces at point a reference to the
   @concept{email address} @concept{address}, associated to the text
   @em{text}.

   @item{@tt{@@author@{}@em{text}@tt{@}}} @cindex{@@author
   command} @em{text} will be printed (in a normal font). This command
   is used to reference the name of an author (not necessarily
   establishing the module authorship).

   @end{description}

   @item @bf{Date and Version:} 

   @begin{description}

   @item{@tt{@@today}} @cindex{@@today command} prints the
   current @index{date}.

   @item{@tt{@@version}} @cindex{@@version command} prints the
   @index{version} of the current manual.

   @end{description}

   @item @bf{Mathematics:} 

   The following commands are used to format text in mathematical
   @cindex{LaTeX notation}.

   @begin{description}

   @item{@tt{@@math@{}@em{text}@tt{@}}} @cindex{@@math command}
   in-line typeset the @em{text} formula.

   @item{@tt{@@begin@{displaymath@}}} @cindex{@@begin@{displaymath@}
   command} marks the beginning of a formula (useful for long
   formulas).

   @item{@tt{@@end@{displaymath@}}} @cindex{@@end@{displaymath@}
   command} marks the end of the (long) formula.

   @item{@tt{@@defmathcmd@{}@em{cmd}@tt{@}@{}@em{n}@tt{@}@{}@em{def}@tt{@}}}
   @cindex{@@defmathcmd/3 command}
   defines the math command @em{cmd}, taking @em{n} arguments, which is expanded as @em{def}.
   Arguments are denotated as @tt{#1}, ..., @tt{#n} inside @em{def}.

   @item{@tt{@@defmathcmd@{}@em{cmd}@tt{@}@{}@em{def}@tt{@}}}
   @cindex{@@defmathcmd/2 command}
   defines the math command @em{cmd}, which is expanded as @em{def} (with no arguments).

   @end{description}

   @item @bf{Inclusion commands:}
  
   @cindex{including code} @cindex{including files} @cindex{including images}

   The following commands are used to include code or strings of text
   as part of documentation. The latter may reside in external files
   or in the file being documented. The former must be part of the
   module being documented. There are also commands for inserting and
   scaling images.

   @begin{description}

   @item{@tt{@@include@{}@em{filename}@tt{@}}} @cindex{@@include
   command} the contents of @em{filename} will be included in-line, as
   if they were part of the string. This is useful for common pieces
   of documentation or storing in a separate file long explanations if
   they are perceived to clutter the source file.

   @item{@tt{@@includeverbatim@{}@em{filename}@tt{@}}}
   @cindex{@@includeverbatim command} as above, but the contents of
   the file are included verbatim, i.e., commands within the file are
   not interpreted. This is useful for including code examples which
   may contain @tt{@@}'s, etc.  Note that this only means that the
   file will be included as is.  If you want the string to be
   represented in verbatim mode in the output, you must surround the
   @tt{@@includeverbatim@{}@em{filename}@tt{@}} with
   @tt{@@begin@{verbatim@}} and @tt{@@end@{verbatim@}}.

   @item{@tt{@@includefact@{}@em{factname}@tt{@}}}
   @cindex{@@includefact command} it is assumed that the file being
   documented contains a fact of the predicate @em{factname}@tt{/1},
   whose argument is a character string. The contents of that
   character string will be included in-line, as if they were part of
   the documentation string. This is useful for @index{sharing pieces
   of text} between the documentation and the running code. An example
   is the text which explains the @index{usage of a command} (options,
   etc.).

   @item{@tt{@@includedef@{}@em{predname}@tt{@}}} @cindex{@@includedef
   command} it is assumed that the file being documented contains a
   definition for the predicate @em{predname}. The clauses defining
   this predicate will be included in-line, in verbatim mode, as if
   they were part of the documentation string. @cindex{including a
   predicate definition}

   @item{@tt{@@image@{}@em{epsfile}@tt{@}}} @cindex{@@image command}
   @index{including an image} at point, @cindex{images, inserting}
   contained in file @em{epsfile}. The @index{image file} should be in
   @index{encapsulated postscript} format.

   @item{@cindex{images, scaling}
   @tt{@@image@{}@em{epsfile}@tt{@}@{}@em{width}@tt{@}@{}@em{height}@tt{@}}}
   @cindex{@@image command} same as above, but @em{width} and
   @em{height} should be integers which provide a size (in points) to
   which the image will be scaled. 

   @end{description}
   
   @item @bf{Accents and special characters:} 

   The following commands can be used to insert @index{accents} and
   @index{special characters}.

   @begin{description}

   @item{@tt{@@`@{o@}}}    @result  @cindex{@@` command}          @`{o} 
   @item{@tt{@@'@{o@}}}    @result  @cindex{@@' command}          @'{o} 
   @item{@tt{@@^@{o@}}}    @result  @cindex{@@^ command}          @^{o} 
   @item{@tt{@@..@{o@}}}   @result  @cindex{@@.. command}         @..{o} 
   @item{@tt{@@\"@{o@}}}   @result  @cindex{@@\" command}         @\"{o} 
   @item{@tt{@@~@{o@}}}    @result  @cindex{@@~ command}          @~{o} 
   @item{@tt{@@=@{o@}}}    @result  @cindex{@@= command}          @={o} 
   @item{@tt{@@.@{o@}}}    @result  @cindex{@@. command}          @.{o} 
   @item{@tt{@@u@{o@}}}    @result  @cindex{@@u command}          @u{o} 
   @item{@tt{@@v@{o@}}}    @result  @cindex{@@v command}          @v{o} 
   @item{@tt{@@H@{o@}}}    @result  @cindex{@@H command}          @H{o} 
   @item{@tt{@@t@{oo@}}}   @result  @cindex{@@t command}          @t{oo}
   @item{@tt{@@c@{o@}}}    @result  @cindex{@@c command}          @c{o} 
   @item{@tt{@@d@{o@}}}    @result  @cindex{@@d command}          @d{o} 
   @item{@tt{@@b@{o@}}}    @result  @cindex{@@b command}          @b{o} 
   @item{@tt{@@oe}}        @result  @cindex{@@oe command}         @oe{} 
   @item{@tt{@@OE}}        @result  @cindex{@@OE command}         @OE{} 
   @item{@tt{@@ae}}        @result  @cindex{@@ae command}         @ae{} 
   @item{@tt{@@AE}}        @result  @cindex{@@AE command}         @AE{} 
   @item{@tt{@@aa}}        @result  @cindex{@@aa command}         @aa{} 
   @item{@tt{@@AA}}        @result  @cindex{@@AA command}         @AA{} 
   @item{@tt{@@o}}         @result  @cindex{@@o command}          @o{} 
   @item{@tt{@@O}}         @result  @cindex{@@O command}          @O{} 
   @item{@tt{@@l}}         @result  @cindex{@@l command}          @l{} 
   @item{@tt{@@L}}         @result  @cindex{@@L command}          @L{} 
   @item{@tt{@@ss}}        @result  @cindex{@@ss command}         @ss{} 
   @item{@tt{@@?}}         @result  @cindex{@@? command}          @?{} 
   @item{@tt{@@!}}         @result  @cindex{@@! command}          @!{} 
   @item{@tt{@@i}}         @result  @cindex{@@i command}          @i 
   @item{@tt{@@j}}         @result  @cindex{@@j command}          @j 
   @item{@tt{@@copyright}} @result  @cindex{@@copyright command}  @copyright
   @item{@tt{@@iso}}       @result  @cindex{@@iso command}        @iso
   @item{@tt{@@bullet}}    @result  @cindex{@@bullet command}     @bullet
   @item{@tt{@@result}}    @result  @cindex{@@result command}     @result

   @end{description}

   @end{itemize}

").

%% Approximate... (specially, calls to string/1)
stringcommand(concept(B)        ) :- string(B).
stringcommand(cindex(B)         ) :- string(B).
stringcommand(cite(B)           ) :- string(B).
stringcommand(pred(B)           ) :- string(B).
stringcommand(op(B)             ) :- string(B).
stringcommand(decl(B)           ) :- string(B).
stringcommand(lib(B)            ) :- string(B).
stringcommand(apl(B)            ) :- string(B).
stringcommand(file(B)           ) :- string(B).
stringcommand(var(B)            ) :- string(B).

stringcommand(comment(B)        ) :- string(B).
stringcommand(begin("itemize")  ).
stringcommand(item("")          ).
stringcommand(item(B)           ) :- string(B).
stringcommand(end("itemize")    ).
stringcommand(begin("enumerate")).
stringcommand(end("enumerate")  ).
stringcommand(begin("description")).
stringcommand(end("description")  ).
stringcommand(begin("verbatim") ).
stringcommand(end("verbatim")   ).
stringcommand(begin("cartouche")).
stringcommand(end("cartouche")  ).
stringcommand(begin("alert")).
stringcommand(end("alert")  ).
stringcommand(section(B)        ) :- string(B).
stringcommand(subsection(B)     ) :- string(B).
stringcommand(subsubsection(B)  ) :- string(B).
stringcommand(footnote(B)       ) :- string(B).
stringcommand(ref(B)            ) :- string(B).
stringcommand(href(B)           ) :- string(B).
stringcommand(href(A,B)         ) :- string(A),string(B).
stringcommand(email(B)          ) :- string(B).
stringcommand(email(A,B)        ) :- string(A),string(B).
stringcommand(author(B)         ) :- string(B).
stringcommand(image(B)          ) :- string(B).
stringcommand(image(A,B,C)      ) :- string(A),string(B),string(C).
stringcommand(today("")         ).
stringcommand(hfill("")         ).
stringcommand(bf(B)             ) :- string(B).
stringcommand(em(B)             ) :- string(B).
stringcommand(tt(B)             ) :- string(B).
stringcommand(key(B)            ) :- string(B).
stringcommand(p(B)              ) :- string(B).
stringcommand(noindent(B)       ) :- string(B). % TODO: noindent("")?
stringcommand(math(B)           ) :- string(B).
stringcommand(displaymath(B)    ) :- string(B).
stringcommand('`'([_X])         ).
stringcommand(''''([_X])        ).
% NOTE: Escaped ^ due to fsyntax!
stringcommand(^'^'([_X])         ).
stringcommand('..'([_X])        ).
stringcommand('"'([_X])         ).
% NOTE: Escaped ~ due to fsyntax!
stringcommand(^'~'([_X])         ).
stringcommand('='([_X])         ).
stringcommand('.'([_X])         ).
stringcommand('u'([_X])         ).
stringcommand('v'([_X])         ).
stringcommand('H'([_X])         ).
stringcommand('t'([_X,_Y])      ).
stringcommand('c'([_X])         ).
stringcommand('d'([_X])         ).
stringcommand('b'([_X])         ).
stringcommand('oe'("")          ).
stringcommand('OE'("")          ).
stringcommand('ae'("")          ).
stringcommand('AE'("")          ).
stringcommand('aa'("")          ).
stringcommand('AA'("")          ).
stringcommand('o'("")           ).
stringcommand('O'("")           ).
stringcommand('l'("")           ).
stringcommand('L'("")           ).
stringcommand('ss'("")          ).
stringcommand('?'("")           ).
stringcommand('!'("")           ).
stringcommand('i'("")           ).
stringcommand('j'("")           ).
stringcommand(copyright("")     ).
stringcommand(iso("")           ).
stringcommand(bullet("")        ).
stringcommand(result("")        ).
stringcommand('@'               ).
stringcommand(index(B)          ) :- string(B).
stringcommand('{'               ).

stringcommand(include(B)        ) :- string(B).
stringcommand(includeverbatim(B)) :- string(B).
stringcommand(includefact(B)    ) :- string(B).
stringcommand(includedef(B)     ) :- string(B).

% (see autodoc_doctree.pl)
stringcommand(bibitem(A,B)      ) :- string(A), string(B).
stringcommand(newblock(B)       ) :- string(B).
