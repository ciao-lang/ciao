:- use_package(assertions).
:- doc(nodoc,assertions).
:- doc(nodoc,assertions_basic).

:- doc(title, "Iterative-deepening execution").

:- doc(author, "R@'{e}my Haemmerl@'{e}").
:- doc(author, "Manuel Carro").
:- doc(author, "Claudio Vaucheret").
:- doc(author, "Manuel Hermenegildo").

:- doc(module,"This package applies a @em{compiling control} technique
to implement @index{depth-first iterative deepening} execution
@cite{iterative-deepening}. It changes the usual @em{depth-first}
search rule by @index{iterative-deepening} on those predicates
specifically marked. This is very useful in search problems as an
alternative to, e.g., breadth-first search, when a
@concept{complete proof procedure} is needed.

When this search rule is used, first all goals are expanded only up to
a given depth.  If no solution is found or more solutions are needed by
backtracking, the depth limit is incremented and the whole goal is
repeated.  Although it might seem that this approach is very inefficient
because all higher levels are repeated for the deeper ones, it has been
shown that it performs only about b/(b - 1) times as many operations than
the corresponding breadth-first search (where b is the branching factor of
the proof tree), while the memory consumption is the same as depth first.

   The usage is by means of the following directive:

@tt{:- iterative(Name, FirstCut, Formula).}

which states that the predicate 'Name' given in functor/arity form
will be executed using the iterative deepening search rule, starting
at depth @var{FirstCut}, and with depth being incremented by predicate
@var{Formula}. This predicate computes the new depth using the
previous one. It must implement a dilating function i.e., the new
depth must be greater than the old one. For example, to start with
depth 5 and increment by 10 you can write:

@tt{:- iterative(p/1,5,f).}

@tt{f(X,Y) :- Y is X + 10.}

or, alternatively:

@tt{:- iterative(p/1,5,(_(X,Y):- Y is X + 10)).}

@cindex{depth limit}
You can also use a fourth parameter to set a limiting depth. Goals
that reach the given depth limit simply fail. Thus, with the following
directive:

@tt{:- iterative(p/1,5,(_(X,Y):- Y is X + 10),100).}

all goals deeper than 100 will fail. 

This is a simple example using this package:

@includecode{id/examples/example_id.pl}

The order of solutions is as follows: first, the shallower ones and
then the deeper ones in the resolution tree. Solutions between two
cutoffs are given in the usual left-to-right order (i.e., the leftmost
branch corresponds to the first unifying clause in the program, as in
depth-first search). For example:

@includecode{id/examples/example2.pl}

It is possible to preserve the iterative-deepening behavior for calls to
predicates defined in other modules. These modules should obviously also
use this package. In addition, @em{all} predicates from such modules should
be imported, i.e., the directive @tt{:- use_module(module)}, should be used in
this case instead of @tt{:- use_module(module,[...])}.  Otherwise calls to
predicates outside the module will be treated in the usual way, i.e., by
depth-first execution.

Another complete proof procedure is implemented by the @lib{bf} package(s)
(@concept{breadth first execution}).").
