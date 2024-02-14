:- use_package(assertions).
:- doc(nodoc,assertions).
:- doc(nodoc,assertions_basic).

:- doc(title,"Dynamic predicates (source preserving)").

:- doc(author, "Daniel Cabeza").
:- doc(author, "The Ciao Development Team").

:- doc(module,"The package @lib{dynamic_clauses} provides the
   assert/retract family of predicates to manipulate dynamic predicates.

   The defined predicates (see @lib{dynamic_clauses_rt}) allow
   modification of the program as it is actually running.  Clauses can
   be added to the program (@em{asserted}) or removed from the program
   (@em{retracted}), as well as inspected.  Note that in Ciao only the
   dynamic predicates of the current module (or accessible dynamic
   multifile predicates) can be accessed and modified.  This limits
   the bad impact to global analysis of this dynamic modification of
   the program.  Thus, if dynamic predicates are exported, to be able
   to inspect or modify them externally some accessing predicates need
   to be implemented and exported alongside.

   For the inspecting/manipulating predicates, the argument which
   corresponds to the clause head must be instantiated to an atom or a
   compound term.  The argument corresponding to the clause must be
   instantiated either to a term @tt{Head :- Body} or, if the body part
   is empty, to @tt{Head}. An empty body part is represented as
   @tt{true}.

   Note that using this library is very detrimental to global analysis,
   and that for most uses the predicates listed in
   @ref{Fast/concurrent update of facts} suffice.

   Example:
@includecode{dynamic_clauses/examples/selfmodif.pl}
").

