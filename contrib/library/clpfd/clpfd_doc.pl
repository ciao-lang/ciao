:- use_package(assertions).
:- doc(nodoc,assertions). 
:- use_package(regtypes).
:- doc(nodoc,regtypes). 

:- doc(title, "Constraint programming over finite domains (new)").
:- doc(author, "Emilio Jes@'{u}s Gallego Arias").
:- doc(author, "R@'{e}my Haemmerl@'{e}").
:- doc(author, "Jose F. Morales").
:- doc(copyright,"@include{DocCopyright.lpdoc}").

:- doc(summary, "An implementation of constraint programming over
   finite domains.").

:- use_module(library(clpfd/clpfd_rt)).
:- include(library(clpfd/clpfd_ops)). 

:- doc(module,"This package extends Ciao with constraints over finite
domains (FD). The solver is an instance of the Constraint Logic
Programming (CLP) scheme as introduced by Jaffar and Lassez
@cite{jaff87-short}. It uses classical propagation techniques
as described in Van Hentenryck's book @cite{VanHen} and Diaz's clp(FD)
implementation @cite{clpfd}.

The package provides predicates for checking consistency of FD
constraints. A FD is a small subset of integers, and FD constraints are
relations over integer. Hence only integer or variables are allowed in
such constraints. FD variables (i.e., variables that occur in an FD
constraint) get associated with a domain either explicitly declared by the
program or implicitly imposed by the solver.  As soon as variables get
an empty domain the computation fails, hence forcing backtracking.

The package defines basic operators, automatically imports basic
constraints and enumerating predicates form the module
@lib{clpfd_rt}, and provides high-level
@ref{Meta-Constraints} through a transparent compilation process.

@section{Completeness Considerations}

For efficiency reason, the solver is not complete on non-ground
constraints, in the sense that it may not be able to determine that a
set of constraints is actually satisfiable. In such cases, the system
silently succeeds. To ensure full completeness, the programmer may use
the @pred{labeling/2} predicate that uses an automatic backtracking
search to find ground solutions for a list of FD variables. Labeling is
complete, always terminates, and yields no redundant solutions.
See an example of use of labeling in the following @ref{Example}.

On success, the top-level will display the domain associated with each FD 
variable remaining free in the query. This domain should not be
understood as values permitted for the corresponding variable, but
only as values not excluded by the incomplete propagation mechanism of
the solver. Note that the answer output by the top-level is by itself
incomplete as the remaining constraints are not showed. 


@section{Meta-Constraints}

There are five meta-constraints, namely @pred{#=/2}, the constraint
equal, @pred{#=/2}, the constraint not equal, @\pred{#</2}, the
constraint less than, the constraint not equal, @pred{#=</2}, the
constraint less or equal, @pred{#>/2}, the constraint more than, and
@pred{#>/2}, the constraint more or equals. These meta-constraint are
defined over arithmetic expression with FD variables (see regular type
@pred{fd_expr/1} in module @lib{clpfd_rt}.).  Such constraints are
\"meta\" in the sens that their arguments are interpreted at
compile-time and all variables occurring free in the arguments will be
implicitly constrained to take integer values only. In particular, note
that variables constrained in such a way would not be unifiable with 
complex FD expressions. For instance, the call:

@begin{verbatim}X + Y #> Z.@end{verbatim} 

is not equivalent to the call:

@begin{verbatim}A = X + Y, A #> Z.@end{verbatim} 

While the first call succeeds, the second one will throw an exception
to indicate that @var{A} cannot be unified with the non-integer term.
It is possible to view meta-constraints as a convenient way to
define an infinite number of FD constraints. For instance
@tt{A #> Z} and @tt{X + Y #> Z} can be considered respectively as 
binary and ternary constraints over FD variables. 

It is possible to delay interpretation of meta-constraints at call-time
by explicitly prefixing the call with @lib{clpfd_rt}. For instance, the
following call will not throw any exception:

@begin{verbatim}A = X + Y, clfd_rt:(A #> Z).@end{verbatim} 

@section{Example}

The problem is to put N queens on an NxN chessboard so that there is
no pair of queens threatening each other.  Each variable is a
queen. Each queen has a designated row. The problem is to find a
different column for each one.

The main constraint of the problem is that no queen threaten
another. This is encoded by the @tt{diff/3} predicate and should hold
for any pair of queens.

The main call is @tt{queens(N, L, Lab)} which looks for a solution
@tt{L} for the @tt{N} queens problem using labeling @tt{Lab}.  Observe
the call to @tt{labeling/2} at the end of definition of @tt{queens/3},
which tries to find a solution for the problem.

@begin{verbatim}
@includeverbatim{clpfd/examples/queens.pl}
@end{verbatim}

").

:- doc(bug, "See clpfd/clpfd_options for a list of configurable
   options in the solver. This part is undocummented.").

%:- regtype clpfd_expr(Exp) # "@var{Exp} is a an clpfd expression".
