:- use_package(assertions).
:- doc(nodoc,assertions).
:- doc(nodoc,assertions_basic).

:- doc(title, "Documentation comments").
:- doc(subtitle, "Comment-style syntax for machine-readable comments").

:- doc(author, "Jose F. Morales").
:- doc(author, "Manuel Hermenegildo").

:- doc(stability, devel("This is still a beta version for
   experimentation. Much functionality is implemented but syntax may
   change in the future.")).

:- doc(module,"
   This package allows including machine-readable documentation
   (including assertions) inside code comments.  Additionally, a
   simpler lightweight markup syntax is enabled for (@apl{LPdoc})
   documentation.

   The overall objective is to allow speeding up the process of
   documentation for many cases that do not require the full power of
   the documentation system and assertion language, as well as
   improving the portability (as documentation comments are simply
   ignored when not supported by other Prolog systems).

   The syntax is partially inspired in the mark up syntax for
   @href{http://www.stack.nl/~dimitri/doxygen/markdown.html}{Doxygen},
   @href{http://coq.inria.fr/doc/Reference-Manual018.html#toc97}{Coqdoc},
   and
   @href{http://www.haskell.org/haddock/doc/html/ch03s08.html}{Haddock}.

@section{Documentation comments as terms}

   This package enables grammar extensions that allow some special
   operators, which annotate the source code with documentation, are
   then translated as @em{documentation assertions}.

   The following pieces of text are understood as both prefix or
   postfix operators:

@begin{verbatim}
%! Comment      (or)    /*! Comment */
%  ...                  
@end{verbatim}

   @noindent which is used to write arbitrary chunks of documentation
   (usually referring to the code after them).

@begin{verbatim}
%< Comment      (or)    /*< Comment */
%  ...                  
@end{verbatim}

   @noindent which is used to write chunks of documentation (usually
   referring to the code before them).

Comments appear in the abstract syntax tree of the parsed programs as
special terms. The @lib{doccomments} package extracts them from the
program to generate the documentation.

Note that reading comments symbolically requires cooperation with the
internal parsing routines. For more details, see the @tt{doccomments}
Prolog flag and its use in the @lib{read} and @lib{tokenize} modules.

@begin{note} 
  As @tt{# \"...\"} comments in LPdoc, this approach continues the
  @em{documentation in the AST}. Other systems take a similar
  approach (for example, see
  @href{http://docs.racket-lang.org/scribble/text.html}{Scribble}). A
  simpler approach could just parse documentation in one pass and
  generate clean code. It is not clear which one is better in the long
  term.
@end{note}

@section{Relation with comment assertions}

This package allows using an alternative syntax for machine-readable
comments. Essentially, most comments of the form:

@begin{verbatim}
:- doc(CommentType, Body).
@end{verbatim}

@noindent
can be written as:

```
%! @CommentType Body
```
 
@noindent
@em{Body} can expand over several lines but each must have a @tt{\%}
in the first column. For example, the following:

```
%! @title  A nice module
% 
%  @author Pro Grammer
% 
%  @module This is a very nice module indeed. 
%          It can be used for several purposes.
%
%  @hide   internal/3
```

@noindent
is equivalent to:

@begin{verbatim}
:- doc(title, \"A nice module\").
:- doc(author,\"Pro Grammer\").
:- doc(module,\"This is a very nice module indeed. 
            It can be used for several purposes.\").
:- doc(hide,internal/3).
@end{verbatim}

See files distributed at @tt{markdown/examples/} for more examples.
").

:- doc(bug, "The column number of the line of multiline @tt{/*...*/}
   comments is not correctly computed due to limitations of the Ciao
   tokenizer. We strongly recomend using @tt{%...} comments instead.
   ").

:- doc(bug, "Syntax highlight of code is not supported").
:- doc(bug, "Doccomments within code are simply ignored at this moment").

%:- doc(_, "Foo").

