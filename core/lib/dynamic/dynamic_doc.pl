:- use_package(assertions).
:- doc(nodoc,assertions).
:- doc(nodoc,assertions_basic).

:- doc(title,"Dynamic predicates (not source preserving)").

:- doc(author, "The Ciao Development Team").

:- doc(module,"This module implements the assert/retract family of
   predicates to manipulate dynamic predicates.

   @begin{note}
   This module does not preserve the original source definition of
   dynamic predicates. That is, @pred{clause/2} may obtain the
   asserted clauses in lower-level expanded form rather than in the
   original shape. Use @lib{dynamic_clauses} if you need consulting
   the original form.
   @end{note}

   The defined predicates (see @lib{dynamic_rt}) allow modification of the
   program as it is actually running.  Clauses can be added to the
   program (@em{asserted}) or removed from the program (@em{retracted}).
   For these predicates, the argument which corresponds to the clause
   head must be instantiated to an atom or a compound term. The argument
   corresponding to the clause must be instantiated either to a term
   @tt{Head :- Body} or, if the body part is empty, to @tt{Head}. An
   empty body part is represented as @tt{true}. 

   Note that using this library is very detrimental to global
   analysis, and that for most uses the predicates listed in
   @ref{Fast/concurrent update of facts} suffice.").

