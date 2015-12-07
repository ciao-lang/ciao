:- use_package(assertions).
:- doc(nodoc,assertions).

:- doc(title, "Breadth-first execution").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Manuel Carro").
:- doc(author, "Manuel Hermenegildo").

:- doc(module,"This package implements breadth-first execution of
   predicates.  This may be useful in search problems when a proof
   procedure is needed that will find all solutions (even if it may
   still loop for some failures).  This is in contrast with the
   default depth-first search, which may loop in some cases even if
   there are correct answers to a given query.  This library is also
   useful when experimenting with pure programs as well as when
   teaching logic programming, for illustrating the expected
   theoretical results that should be expected from the declarative
   semantics (see for example the slides in
   @href{http://www.cliplab.org/proglog}).

   It is important to realize, however, that the improved behaviour of
   breadth first execution comes at a high (exponential!) price in
   terms of both time ad memory.  This library allows the programmer
   to control this overhead by selecting which predicates will be
   executed in breadth-first mode and which predicates in depth-first
   mode.  More concretely, predicates written with operators
   @op{'<-'/1} (facts) and @op{'<-'/2} (clauses) are executed using
   breadth-first search, while predicates using the standard syntax
   will be executed depth-first.

   The following example implements two versions of a predicate meant
   to succeed if two nodes of a directed graph are connected. The
   @tt{chain/2} predicate (which will be executed depth-first) loops
   without finding the connection between @tt{a} and @tt{d}, while the
   @tt{bfchain/2} predicate (which will be executed breadth-first)
   will find the connection correctly:

@begin{verbatim}
@includeverbatim{bf/examples/chain.pl}
@end{verbatim}

   A second package, @lib{'bf/bfall'}, allows executing @em{all} the
   predicates in a given module in breadth-first mode. In this case,
   predicates should be written using the standard syntax.  This is
   useful to be able to switch easily between depth-first and
   breadth-first execution (e.g., for testing purposes) for all
   predicates in a given module without having to modify the
   program. The following program (written in standard syntax) runs
   breadth-first:

@begin{verbatim}
@includeverbatim{bf/examples/chain_bfall.pl}
@end{verbatim}

   There is another version, package @lib{'bf/af'}, which ensures
   AND-fairness by goal shuffling.  This reduces the number of cases
   in which an execution that is a failure loops instead (infinite
   failures) at a small additional cost. For example, by using
   @lib{'bf/af'} the following code correctly answers ``@tt{no}'' when
   executing @tt{test/0}:

@begin{verbatim}
@includeverbatim{bf/examples/sublistapp.pl}
@end{verbatim}

   There is also a package @lib{'bf/bfall'} which again allows
   executing @em{all} the predicates in a given module in
   breadth-first, and-fair mode, where also all predicates should be
   written using the standard syntax. This package offers (at a cost,
   of course) very nice results for many programs, and is used
   extensively in programming courses by the Ciao developers.

   Finally, it should be noted that a separate library, @lib{id},
   implements @em{iterative-deepening} search, which can in many cases
   be a better alternative to breadth-first search, since it achieves
   the same improvement in the completeness results in many cases at a
   greatly reduced execution cost (but the enumeration order of
   solutions is not as nice, and that is why these packages are very
   attractive for prototyping and teaching).

").

:- doc(bug, "Does not correctly work in user files.").

:- include(library(bf/ops)).
