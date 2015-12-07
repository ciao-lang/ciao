:- module(indexer_doc, [], [assertions, regtypes]).

:- use_module(library(assertions/native_props), [nonground/1]).

:- doc(title, "Multiple Argument Indexing").

:- doc(author, "Anil Nair (original work)").
:- doc(author, "Tom Howland"||
                   " (@href{http://home.pacbell.net/tomjdnh/pd.html},"||
                   " derived the original work)").
:- doc(author, "Francisco Bueno (initial port to Ciao)").
:- doc(author, "Jose F. Morales (improvements in implementation and
   documentation)").

:- doc(module,"Indexing (in Prolog) is an optimization technique that
   reduces the search space of the predicates without altering the
   Prolog semantics.

   In the most general case, predicate clauses are tryed on
   backtracking one after the other, in sequential order. We can call
   this list of clauses a @em{try-list}. Indexing is based on removing
   clauses that are known to fail without any observable output (no
   side-effects) from the try-list. A typical implementation
   introduces tests before the actual predicate execution to
   discriminate among a collection of precomputed specialized
   try-lists. In the best case, this technique can obtain try-lists
   with 0 or 1 elements for calls.

   Currently, the Ciao engine implements a limited but fast
   1st-argument-1st-level indexing. The @lib{indexer} package provides
   more powerful indexing schemes. It lets you pick different
   combinations of arguments to index on.  E.g., it will let you index
   on the first and third argument or the second and the third
   argument of a predicate.

   The selection of the try-list is based on computing a hash value
   for the terms (or part of them) to be indexed upon. Given this, the
   optimization pays off only when the amount of clashing that your
   original predicate causes without indexing superseeds the cost of
   the hashing function. Such amount of course depends on the number
   and form of the facts in your predicate, and the calling modes.

   @begin{alert}
     @bf{Important Note about Performance}

     @begin{itemize}
     @item The current implementation of the package is done at the
     source level, so it may sometimes not be as fast as expected.

     @item The complexity of the hashing function currently used is
     @math{O(n)} with @math{n} the number of characters in the textual
     representation of the term. Thus, even if the search tree is
     reduced, performance can be much slower in some cases that the
     cheaper internal (1st argument, 1st level) indexing used in Ciao.
     @end{itemize}

     Despite this, the package implements some indexing schemes with
     @bf{low overhead}.
     @begin{itemize}
     @item A single @code{:- index p(+,?,...?)} indexer (1st argument,
       1st level). Reuses the internal indexing.

     @item A single @code{:- index p(?,...,+,...?)} indexer (one argument,
       1st level). Reuses the internal indexing by reordering the
       predicate arguments.
     @end{itemize}
   @end{alert}
").

% [original comment]
% "library(indexer)" generates index tables of the form
% Nth_arg-1st_arg to index on the Nth argument. This means that you
% could get redundant solutions. This package adds an extra unique
% "clause number" argument to each fact to get around this.

% [original comment]
% "library(indexer)" can be used only for dynamic facts. This
% directory contains files for static rules (indexer.pl), dynamic
% facts (dynamic_indexer.pl), and module-partitioned dynamic facts
% (module_indexer.pl).

:- doc(usage,"This facility is used as a package, thus either
   including @lib{indexer} in the package list of the module, or by
   using the @decl{use_package/1} declaration. The facility predicate
   @pred{hash_term/2}, documented here, is defined in library module
   @lib{indexer(hash)}.").

:- doc(doinclude,index/1).
:- decl index(IndexSpecs) => indexspecs
# "Declares an indexing scheme for a predicate. Each spec declares an indexing on
   a combination of the arguments. Indexing will be performed using any of
   the specs in @var{IndexSpecs} (being thus interpreted as an or).

   You should use a @tt{*} in an argument position if you wish to hash on 
   the entire term in that argument. If a @tt{+} is used only one level of 
   the term in the argument is used for hashing. An @tt{i} is used to
   indicate that argument is already an integer, and therefore its own value
   will be used for hashing. The argspec @tt{?} simply indicates not to use
   the argument for indexing.

   For example, the index specification:
@begin{verbatim}
:- index foo(+,?,*,i), foo(?,?,?,i).
@end{verbatim}
   declares indexing for @tt{foo/4} either on a combination of the first,
   third, and fourht arguments, or only on the last argument, which is an
   integer. In the first case, only the principal functor of the first 
   argument will be used for hashing; the third argument will be used in
   its entirety.

   The argspec @tt{n} is a pragmatic extension and can not be used in 
   conjunction with the other specifiers aside from @tt{?}. It stands for 
   ""nonvar"" and implies that the argument will not be used for hashing,
   since only ground terms can effectively be used in hashing. Thus, it
   can not be used in combination with other specifiers within a particular 
   index specification. It is often the fastest thing to use.
".

:- doc(bug, "The semantics of cut (!) are not preserved with the
   'general' indexing scheme (see implementation). Translations should
   happen after choice idiom / cut idiom are introduced. That is not
   possible with the current expansion mechanism. Communication with
   the internal indexing tables could be the easier solution").

:- doc(bug, "Indexing specs must appear before the clauses of the
   predicate they specify.").

:- regtype indexspecs(IndexSpecs)
   # "@var{IndexSpecs} is an index specification.".
:- doc(indexspecs/1,
  "An index specification is defined as follows:
   @includedef{indexspecs/1} @includedef{indexspec/1}").
:- doc(doinclude,indexspecs/1).

indexspecs(Spec) :- indexspec(Spec).
indexspecs((Spec,Specs)) :- indexspec(Spec), indexspecs(Specs).

indexspec(Spec) :- Spec=..[_F|Args], list(Args,argspec).

:- regtype argspec(Spec)
   # "@var{Spec} is an argument hash specification.".
:- doc(argspec/1,
  "An argument hash specification is defined as follows:
   @includedef{argspec/1}").
:- doc(doinclude,argspec/1).

argspec(+).
argspec(*).
argspec(i).
argspec(n).
argspec(?).

:- pred hash_term(T,N) : ground * var => int(N)
	# "@var{N} is a hashing index for @var{T}.".
:- pred hash_term(T,N) : nonground * var => var(N).
:- doc(hash_term(Term,HashValue),
  "Provides an efficient way to calculate an integer @var{HashValue} for a
   ground @var{Term}.").
:- doc(doinclude,hash_term/2).
% hash_term/2 documented, but not implemented here: --EMM
:- reexport(library(indexer/hash), [hash_term/2]).
