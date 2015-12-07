:- module(test_math, [], [regtypes, assertions]).

:- use_module(library(lists)).

:- doc(title, "Testing Mathematical Notation in LPdoc").
:- doc(author, "Jose F. Morales").

%@defmathcmd{@neck}{@textbf{:-}}
:- doc(module, "This module tests the mathematical environments in LPdoc.

@defmathcmd{@neck}{@!@!:@!@!-@!}

@section{Testing}

When @math{a @ne 0}, there are two solutions to @math{ax^2 + bx + c = 0} and they are

@begin{displaymath}
x = {-b @pm @sqrt{b^2-4ac} @over 2a}
@end{displaymath}

@section{More Testing}

A regular program is defined by a set of clauses, each of the
form:
@begin{displaymath}
p(x, v_1, @ldots, v_n) ~@neck~ @textit{body}_1, @ldots, @textit{body}_k.
@end{displaymath}
where:
@begin{enumerate}
@item @math{x} is a term whose variables (which are called @em{term
   variables}) are unique, i.e., it is not allowed to introduce
   equality constraints between the variables of @math{x}.

   For example,
    @math{p(f(X, Y)) ~@neck~ @ldots} is valid, but
    @math{p(f(X, X)) ~@neck~ @ldots} is not. 

@item in all clauses defining @math{p/n+1} the terms @math{x} do not unify
   except maybe for one single clause in which @math{x} is a variable.

@item @math{n @ge 0} and @math{p/n} is a
  @index{parametric type functor} (whereas the predicate defined by the
  clauses is @math{p/n+1}).
  
@item @math{v_1, @ldots, v_n} are unique variables, which are
  called @em{parametric variables}.
     
 @item Each @math{@textit{body}_i} is of the form:

  @begin{enumerate}
    @item @math{t(z)} where @math{z} is one of the
      @em{term variables} and @math{t} is a
      @em{regular type expression}; 

    @item @math{q(y, t_1, @ldots, t_m)} where @math{m @ge 0}, @math{q/m}
      is a @em{parametric type functor}, not in the set of functors
       @tt{=/2}, @tt{^/2}, @tt{./3}.

        @math{t_1, @ldots, t_m} are @em{regular type expressions},
        and @math{y} is a @em{term variable}.
  @end{enumerate}

 @item Each term variable occurs at most once in the clause's body
   (and should be as the first argument of a literal). 
@end{enumerate}
A @index{regular type expression} is either a parametric variable or a
parametric type functor applied to some of the parametric variables.

A parametric type functor is a regular type, defined by a regular
program, or a basic type.
Basic types are defined in @ref{Basic data types and properties}.
").

:- export(harrop_formula/1).
:- prop harrop_formula(X) # "
In @em{intuitionistic logic}, the @bf{Harrop formulae}, named after
@author{Ronald Harrop}, are the class of formulae inductively defined
as follows:

@begin{itemize}
@item Atomic formulae are Harrop, including falsity (@math{@bot});

@item @math{A @wedge B} is Harrop provided @math{A} and @math{B} are;

@item @math{@neg F} is Harrop for any well-formed formula @math{F};

@item @math{F @rightarrow A} is Harrop provided @math{A} is, and @math{F} is any well-formed formula;

@item @math{@forall x. A} is Harrop provided @math{A} is.
@end{itemize}
".
harrop_formula(_). % TODO: define

:- export(sequent/1).
:- prop sequent(X) # "@var{X} is @math{@Gamma @vdash @Delta}".
:- regtype sequent/1.
sequent(sequent(X,Y)) :- term(X), term(Y).

:- export(sum/2).
:- pred sum(Xs, Z) # "@math{z = \\sum_i x_i}".
sum([], 0).
sum([X|Xs], Z) :- sum(Xs, Z1), Z is X + Z1. % (not tail recursive)

