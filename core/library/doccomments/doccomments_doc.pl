:- use_package(assertions).
:- doc(nodoc,assertions).

:- doc(title,"Documentation comments").
:- doc(subtitle, "Comment-style syntax for machine-readable comments").

:- doc(author,"Jose F. Morales").
:- doc(author,"Manuel Hermenegildo").
:- doc(module,"

@begin{alert} 
This is still a beta version for experimentation. Much functionality
is implemented but syntax may change in the future. The wiki part will
be integrated soon into LPdoc so that it can be used in bare
@tt{.lpdoc} files too.
@end{alert}

   This package allows using an alternative syntax for (@apl{LPdoc})
   machine-readable documentation inside specially marked code
   comments, and provides a simplified lightweight markup (wiki)
   syntax for documentation.

@section{Motivation and Related Work}

   In @apl{LPdoc}, documentation comments are read as part of the
   program assertions, as normal Prolog terms (similarly to code
   comments in many languages like Lisp variants or Python). Although
   this is convenient in practice, it is sometimes less flexible (no
   doccomments allowed within the code) and less portable (when using
   tools that do not understand the assertion language).

   This package tries to solve this issue by introducing a
   backwards-compatible comment syntax and a simplified lightweight
   markup. The syntax is partially inspired in the mark up syntax for
   @href{http://www.stack.nl/~dimitri/doxygen/markdown.html}{Doxygen},
   @href{http://coq.inria.fr/doc/Reference-Manual018.html#toc97}{Coqdoc},
   and
   @href{http://www.haskell.org/haddock/doc/html/ch03s08.html}{Haddock}.

@section{Documentation comments}

   This package enables grammar extensions that allow some special
   operators, which annotate the source code with documentation, are
   then translated as @em{documentation assertions} understood
   natively by LPdoc.

   The following pieces of text are understood as both prefix or
   postfix operators:

@begin{verbatim}
@tt{\%!} @em{Comment}      (or)    @tt{/*!} @em{Comment} @tt{*/}
@tt{\%}  @em{...}                  
@end{verbatim}

   @noindent which is used to write arbitrary chunks of documentation
   (usually referring to the code after them).

@begin{verbatim}
@tt{\%<} @em{Comment}      (or)    @tt{/*<} @em{Comment} @tt{*/}
@tt{\%}  @em{...}                  
@end{verbatim}

   @noindent which is used to write chunks of documentation (usually
   referring to the code before them).

Comments appear in the abstract syntax tree of the parsed programs as
special terms. The @lib{doccomments} package extracts them from the
program to generate the documentation.

@bf{Note:} Reading comments symbolically requires cooperation with the
internal parsing routines. For more details, see the @lib{read} and
@lib{tokenize} modules, and the @tt{doccomments} Prolog flag.

@begin{alert} 
  As @tt{# \"...\"} comments in LPdoc, this approach continues the
  @em{documentation in the AST}. Other systems take a similar
  approach (for example, see
  @href{http://docs.racket-lang.org/scribble/text.html}{Scribble}). A
  simpler approach could just parse documentation in one pass and
  generate clean code. It is not clear which one is better in the long
  term.
@end{alert}

@section{Lightweight markup}

@begin{alert}
@begin{itemize}
@item Move this treatment of wiki-like syntax to LPdoc, not here.
@item Extract the manual part from @tt{examples/markup_test.pl} and
  @tt{examples/lightweight_markup_test.pl}
@end{itemize}
@end{alert}

@section{Relation with LPdoc comment assertions}

This package allows using an alternative syntax for machine-readable
comments. Essentially, most comments of the form:

@begin{verbatim}
:- doc(@em{CommentType},@em{Body}).
@end{verbatim}

@noindent
can be written as:

@begin{verbatim}
@tt{\%!} @@@em{CommentType} @em{Body}
@end{verbatim}
 
@noindent
@em{Body} can expand over several lines but each must have a @tt{\%}
in the first column. For example, the following:

@begin{verbatim}
\%! @@title  A nice module
\% 
\%  @@author Pro Grammer
\% 
\%  @@module This is a very nice module indeed. 
\%          It can be used for several purposes.
\%
\%  @@hide   internal/3
@end{verbatim}

@noindent
is equivalent to:

@begin{verbatim}
:- doc(title, \"A nice module\").
:- doc(author,\"Pro Grammer\").
:- doc(module,\"This is a very nice module indeed. 
                It can be used for several purposes.\").
:- doc(hide,internal/3).
@end{verbatim}
").

:- doc(bug, "The column number of the line of multiline @tt{/*...*/}
   comments is not correctly computed due to limitations of the Ciao
   tokenizer. We strongly recomend using @tt{%...} comments instead.
   ").

:- doc(bug, "Syntax highlight of code is not supported").
:- doc(bug, "Doccomments within code are simply ignored at this moment").

%:- doc(_, "Foo").

