:- use_package(assertions).
:- doc(nodoc,assertions).

:- doc(title, "Iterative-deepening execution").

:- doc(author, "R@'{e}my Haemmerl@'{e}").
:- doc(author, "Manuel Carro").
:- doc(author, "Claudio Vaucheret").
:- doc(author, "Manuel Hermenegildo").

:- doc(module,"This package applies a @em{compiling control} technique
to implement @index{depth first iterative deepening} execution
@cite{iterative-deepening}. It changes the usual @em{depth-first}
computation rule by @index{iterative-deepening} on those predicates
specifically marked. This is very useful in search problems when a
@concept{complete proof procedure} is needed.

When this computation rule is used, first all goals are expanded only up to
a given depth.  If no solution is found or more solutions are needed by
backtracking, the depth limit is incremented and the whole goal is
repeated.  Although it might seem that this approach is very inefficient
because all higher levels are repeated for the deeper ones, it has been
shown that is performs only about b/(b - 1) times as many operations than
the corresponding breadth-first search, (where b is the branching factor of
the proof tree) while the waste of memory is the same as depth first.

   The usage is by means of the following directive:

@tt{:- iterative(Name, FirstCut, Formula).}

which states than the predicate 'Name' given in functor/arity form will be
executed using iterative deepening rule starting at the depth 'FirstCut'
with depth being incremented by the predicate 'Formula'. This predicate
compute the new depth using the previous one. It must implement a dilating
function i.e. the new depth must be greater. For example, to start with
depth 5 and increment by 10 you can write:

@tt{:- iterative(p/1,5,f).}

@tt{f(X,Y) :- Y is X + 10.}

or if you prefer,

@tt{:- iterative(p/1,5,(_(X,Y):- Y is X + 10)).}

@cindex{depth limit}
You can also use a fourth parameter to set a limiting depth. All goals
below the given depth limit simply fail. Thus, with the following
directive:

@tt{:- iterative(p/1,5,(_(X,Y):- Y is X + 10),100).}

all goals deeper than 100 will fail. 

An example of code using this package would be:

@begin{verbatim}
@includeverbatim{id/examples/example_id.pl}
@end{verbatim}

The order of solutions are first the shallower and then the
deeper. Solutions which are between two cutoff are given in the usual left
to right order. For example,

@begin{verbatim}
@includeverbatim{id/examples/example2.pl}
@end{verbatim}

Another complete proof procedure implemented is the @lib{bf} package
(@concept{breadth first execution}).").
