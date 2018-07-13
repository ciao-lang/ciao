:- use_package(assertions).
:- doc(nodoc,assertions).

:- doc(title,"Traits").

:- doc(author, "Jose F. Morales").

:- doc(stability, devel("Currently the subject of active development
   and research. Syntax may change without warning or deprecation
   period.")).

:- doc(module, "This package extends the Ciao module system with
   traits. This is a lightweight translation with no overhead
   w.r.t. traditional use of @tt{multifile} declarations for @em{hook}
   predicates.

   A @concept{trait} is defined in Ciao as a collection of predicates
   that can be implemented for any functor. Functors can implement
   multiple traits.

   This translation delegates on the underlying module system as much
   as possible, e.g., for dealing with undefined predicates.

   Some important notes on the translation:
   @begin{itemize}
   @item internal argument order ensures that first-argument indexing is
     preserved

   @item functors data is passed as an extra argument to implementation clauses as follows:
     @begin{itemize}
     @item constants add no extra arguments
     @item unary functors @tt{f(Datum)} are passed as @tt{Datum}
     @item any other functor is passed unaltered
     @end{itemize}
   @end{itemize}

   Example code:

   @begin{verbatim}@includeverbatim{examples/trait_test.pl}@end{verbatim}

   which should be equivalent to:

   @begin{verbatim}@includeverbatim{examples/trait_orig.pl}@end{verbatim}
").
