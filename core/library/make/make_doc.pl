:- use_package([assertions]).
:- doc(nodoc, assertions).

:- doc(title,    "The Ciao Make Package").
:- doc(subtitle_extra, "REFERENCE MANUAL").
:- doc(subtitle_extra, "@em{Generated/Printed on:} @today{}").

:- doc(author, "Manuel Hermenegildo").

%% :- include(ciao_docsrc(common/'ClipAddress')).

:- doc(copyright, "Copyright @copyright{} Manuel Hermenegildo

@include{DocCopyright.lpdoc}

").

%% :- doc(summary, "...").

:- doc(module, "@noindent This package is used mainly in two main ways:

@begin{itemize} 

@item When writing @file{Makefile}s for @apl{lpmake}.

@item When writing @em{applications} which use the @lib{make} library.

@end{itemize}

@noindent In both cases, this is the package that defines the syntax
and meaning of the dependency rules used.

").

:- doc(usage, "

@begin{itemize} 

@item When writing @file{Makefile}s for @apl{lpmake}, such makefiles
      start with:

@begin{verbatim} 
:- module(_,_,[make]).
@end{verbatim} 

or
 
@begin{verbatim} 
:- make(_,_).
@end{verbatim} 
 
      @noindent (The latter uses the feature that an undefined declaration
      at the beginning of a file is interpreted by Ciao as a
      @decl{use_module/3} including as third argument a package with 
      the same name, in this case @tt{make}.) 

@item When writing @em{applications} which use the @lib{make} package,
      then it is loaded as any other package within the application.

@end{itemize}

@noindent @bf{Note}: It is often useful to use the @lib{fsyntax}
package inside a @file{Makefile} (or when when using the @lib{make}
library in other applications). If both @lib{make} and @lib{fsyntax}
are used, then @lib{make} should appear @bf{before} @lib{fsyntax} in
the list of packages.

").

:- use_package(library(make)).

:- doc(appendix, "

@subsection{The Dependency Rules}

@noindent
The package allows defining the following types of rules:

@begin{description}

@item{@tt{@em{TargetSuffix} <= @em{SourceSuffix} :: @em{SourceRoot} :-
  @em{BodyLiterals}.}}

A rule of this form declares that in order to produce the file with
suffix @em{TargetSuffix} from a source file with the suffix
@em{SourceSuffix} and root name @em{SourceRoot} the commands in
@em{BodyLiterals} must be executed. @em{BodyLiterals} is a standard
Ciao Prolog clause body, i.e., a comma-separated conjunction of
literals. When writing the script, @em{SourceRoot} is typically left
as a variable, to be instantiated by @apl{lpmake} when the script is
run to the root of name of the file to be processed. This allows using
the value of @em{SourceRoot} in @em{BodyLiterals}.  For example, the
following rule:

@begin{verbatim}
:- use_module(library(terms), [atom_concat/2]).

dvi <= tex :: FileRoot :-
        atom_concat(['latex ',FileRoot,'.tex'],Command),
        system(Command).
@end{verbatim}

states that we can generate a file @em{File}@tt{.dvi} if we have a
file named @em{File}@tt{.tex} and that the command to do so is
@tt{latex }@em{File}@tt{.tex}. Thus, if this rule appears in file
@file{Makefile.pl} and we issue the command @tt{lpmake paper.dvi} 
the following occurs:

@begin{itemize} 

@item If @tt{paper.dvi} does not exist and @tt{paper.tex} exists, then
      @tt{paper.dvi} is generated from @tt{paper.tex} by issuing the
      system command @tt{latex paper.tex}.

@item If @tt{paper.dvi} already exists, nothing is done.

@item If @tt{paper.tex} does not exist, an error is reported.

@end{itemize}

@item{@tt{@em{Target} <- :- @em{BodyLiterals}.}}

A rule of this form declares that in order to produce the file
@em{Target} the commands in @em{BodyLiterals} must be executed.
@em{Target} need not be a real file: it can also be simply the name of
the rule, which is used to invoke it (as a procedure name).  For
example, the following rule, when the command @tt{lpmake realclean} is
issued, deletes temporary files in the LaTeX application:

@begin{verbatim}
:- use_module(library(source_tree), [delete_glob/2]).

clean <- :-
        delete_glob('.', '*.aux|*.log|*~').
@end{verbatim}

@item{@tt{@em{Target} <- @em{Deps} :- @em{BodyLiterals}.}}

A rule of this form declares that in order to produce the file
@em{Target}, first targets @em{Deps} will be called (i.e., the
elements of @em{Deps} are either other targets with rules defined for
them, or a file or files which are already present or which can --and
will be-- generated from other available files using other
rules). Then, the commands in @em{BodyLiterals} will be
executed. @em{Deps} may be one target or a list of targets. For
example, the following rule, when the command @tt{lpmake realclean} is
issued, cleans all the temporary files in the LaTeX application
(including @tt{.dvi} and @tt{.ps} files). It requires that clean be
executed first:

@begin{verbatim}
:- use_module(library(source_tree), [delete_glob/2]).

realclean <- clean :-
        delete_glob('.', '*.dvi|*.ps').

@end{verbatim}

The following rule states that in order to meet the target @tt{view},
target @tt{paper.ps} must be available or generated.  For example,
@tt{lpmake view} can be used to call the @apl{ghostview} visualizer on
@tt{paper.ps}.  Note the use of a globally defined @em{predicate}
@tt{main} which is called in two places in the rule, and could be used
in other rules in the same file (@tt{main := paper.}  is equivalent to
the fact @tt{main(paper).}  --see the @lib{fsyntax} library):

@begin{verbatim}
:- use_package(fsyntax).
:- use_module(library(system)).
:- use_module(library(system_extra)).
:- use_module(library(terms), [atom_concat/2]).

main := paper.

view <- ~atom_concat([~main,'.ps']) :-
        system(~atom_concat(['ghostview ',~main,'.ps'])).
@end{verbatim}

@end{description}

In addition to these rules, the configuration file can define normal
predicates in the usual way, or import predicates from other modules,
all of which can be called from the bodies of the dependency
rules. For example, the @lib{system_extra} library (an extension of
the @lib{system} library) defines many system predicates in a form
which makes them very useful inside @file{Makefile}s, specially if the
@lib{fsyntax} package is used (see the examples below).

If @apl{lpmake} is called without an explicit target as argument, then
the first target rule in the Makefile is used. This is useful in that
the first rule can be seen as the default rule.

@subsection{Specifying Paths}

Using the @pred{vpath/1} predicate it is possible in configuration
files to define several paths in which files related to the rules can
be located. In this way, not all files need to be in the same
directory as the configuration file. For example:

@begin{verbatim}
:- use_package(fsyntax).

vpath := '/home/clip/Systems/ciao/lib'.
vpath := '/home/clip/Systems/ciao/library'.
vpath := '/home/clip/Systems/lpdoc/lib'.
@end{verbatim}

@subsection{Documenting Rules}

@noindent
It is also possible to define documentation for the rules:

@begin{description}

@item{@tt{target_comment(@em{Target}) :- @em{BodyLiterals}.}}

A rule of this form allows documenting the actions related to the
target. The body (@em{BodyLiterals}) will be called in two
circumstances: 

@begin{itemize}

@item If @em{Target} is called during execution of '@tt{lpmake}
      @em{commands}'.

@item When calling '@tt{lpmake -h}'.

@end{itemize}

Using noun forms (@em{generation of foo} instead of @em{generating
foo}) in comments helps this dual purpose.  For example, the
following rule:

@begin{verbatim}
target_comment(realclean) :- 
        display('Cleanup of all generated files.').
@end{verbatim}

will produce output in the two cases pointed out above.

@item{@tt{dependency_comment(@em{SourceSuffix}, @em{TargetSuffix},
  @em{SourceRoot}) :- @em{BodyLiterals}.}}

Same as the previous rule, but for suffix rules. See, for example, the
following generic rule:

@begin{verbatim}
:- use_module(library(terms), [atom_concat/2]).

dependency_comment(SSuffix,TSuffix,FileBase) :- 
        display(~atom_concat(['Generation of ',FileBase,'.',
	        TSuffix, ' from ',FileBase,'.',SSuffix])).
@end{verbatim}

@end{description}

@subsection{An Example of a Makefile}

The following is a simple example of a Makefile showing some basic
functionality (this is @tt{MakefileExample.pl} in the
@tt{example_simple} directory in the @lib{make} library.):

@begin{verbatim}
@includeverbatim{make/example_simple/MakefileExample.pl}
@end{verbatim}

The following are a few commands that can be used on the previous file 
(see file @tt{CommandsToTry} in the @tt{example_simple}
directory in the @lib{make} library):

@begin{verbatim}
@includeverbatim{make/example_simple/CommandsToTry}
@end{verbatim}

See also the LaTeX example in the @tt{example_latex} directory in the
@lib{make} library.

").
