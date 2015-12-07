:- use_package([assertions]).
:- doc(nodoc, assertions).

:- doc(title, "Conditional Compilation").

:- doc(author, "Jose F. Morales").

:- doc(bug, "Syntax and semantics of conditions for conditional code
   may change in the future (@tt{:- compilation_fact(X)}). Avoid using that
   in production code.").

:- doc(bug, "Errors in this package do not show the program line
   numbers.").

% NOTE: All code (even that not included) must be syntactically
% correct.
%
% WARNING: Do not use it to select code for different architectures or
% your bootstraps won't be portable! (you will make bytecode depends
% on architecture)
%
% Little example:
% :- compilation_fact(testing).
% :- if(testing ; unstable).
% :- else.
% :- endif.

:- doc(module, "This package defines a serie of directives for
conditional compilation that allow the inclusion or exclusion of code
blocks (which may contain nested conditional directives) based on the
truth value at compile time of special goals called
@em{conditions}. The syntax for conditional directives is:

@begin{verbatim}
:- if(Cond1).
  <<Block1>>
:- elif(Cond2).
  <<Block2>>
:- else.
  <<BlockN>>
:- endif.
@end{verbatim}

@noindent where @tt{elif(_)} can appear zero or more times and the
@tt{else} part is optional. The sentences in @tt{Block1} are included
if the condition in @tt{Cond1} is satisfied, else @tt{Block2} is
included if @tt{Cond2} is satisfied (and so on for each @tt{elif}),
and @tt{BlockN} if no previous condition is satisfied.

@section{Conditional Conditions}

The valid conditions are restricted to a subset of goals that can be
safely evaluated at compile time. At this moment, we only accept the
following ones:

@begin{itemize}
@item Compile-time values of prolog flags (@tt{current_prolog_flag/2}).
@item Conjunctions, disjunctions, or negations of conditions.
@item Calls to facts previously defined with @tt{:-
  compilation_fact(Fact)}. This is a experimental feature that may change
  in the future.
@item @tt{defined(F/N)} (or @tt{defined(F)}, equivalent to
  @tt{defined(F/0)}), which succeeds only if there is a definition for
  the compilation fact @tt{F/N}.
@end{itemize}
").


