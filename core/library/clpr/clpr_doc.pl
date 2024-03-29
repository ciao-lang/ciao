:- use_package(assertions).
:- doc(nodoc,assertions).
:- doc(nodoc,assertions_basic).

:- doc(title, "Constraint programming over reals").
:- doc(author, "Christian Holzbaur").
:- doc(author, "Daniel Cabeza").
:- doc(author, "Samir Genaim (Meta-programming predicates)").

:- doc(module, "
@begin{alert}
@bf{Warning}: This package is currently being adapted to the new
characteristics of the Ciao module system. This new version now works
right now to some extent, but it under further development at the
moment. Use with (lots of) caution.
@end{alert}
").

%% We include this file here so that the operators 
%% appear in the documentation.

:- include(library(clpqr/clpqr_ops)).


:- doc(bug, "clp(Q) and clp(R) cannot be used simultaneously in
the same program, or even within the same toplevel session.").


:- doc(appendix, "

@subsection{Some CLP(R) examples}

@noindent
(Other examples can be found in the source and library directories.)

@begin{itemize}
@item 'Reversible' Fibonacci (clpr):
@end{itemize}

@includecode{clpqr/examples/fib_r}

@begin{itemize}
@item Dirichlet problem for Laplace's equation (clpr):
@end{itemize}

@includecode{clpqr/examples/laplace}

@subsection{Meta-programming with CLP(R)}

see @ref{Meta-programming with CLP(Q)}

").
