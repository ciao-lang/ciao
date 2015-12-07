:- use_package(assertions).
:- doc(nodoc,assertions).

:- doc(title,"Terms with named arguments -records/feature terms").

:- doc(bug,"It would be nice to add a mechanism to portray terms
    with named arguments in a special (user definable) way.").
:- doc(bug, "The predicate @tt{$~} still does not support runtime
   argnames.").

:- doc(author,"Daniel Cabeza").
:- doc(author,"Manuel Hermenegildo").
:- doc(author,"Jose F. Morales").

:- doc(module, "@cindex{Naming term arguments} This library package
   provides syntax which allows accessing term arguments by name
   (these terms are sometimes also referred to as @index{records}, and
   are also similar to @index{feature terms}
   @cite{Ait-KaciPodelskiSmolka92}).").

:- use_package(library(argnames)).

:- decl argnames(ArgNamedPredSpec) 

# "An @decl{argnames/1} declaration assigns names to the argument
   positions of terms (or literal/goals) which use a certain
   functor/arity. This allows referring to these arguments by their
   name rather than by their argument position. Sometimes, argument
   names may be clearer and easier to remember than argument
   positions, specially for predicates with many arguments. Also, in
   some cases this may allow adding arguments to certain predicates
   without having to change the code that uses them.  These terms with
   named arguments are sometimes also referred to as
   @concept{records}, and are also similar to @concept{feature terms}
   @cite{Ait-KaciPodelskiSmolka92}. For example, in order to write a
   program for the @em{zebra} puzzle we might declare:

@noindent
@begin{verbatim}
:- use_package([argnames]).
:- argnames house(color, nation, pet, drink, car).
@end{verbatim}

   @noindent which first includes the package and then assigns a name
   to each of the arguments of any term (or literal/goal) with
   @tt{house/5} as the main functor.

   For convenience the package extends the built-in @decl{data/1}
   declaration so that names to arguments can be asigned as with the
   @decl{argnames/1} declaration, as for example:

@noindent
@begin{verbatim}
:- data product(id, description, brand, quantity).
@end{verbatim}

   Once an @decl{argnames/1} is given, it is possible to use the names
   to refer to the arguments of any term (or literal/goal) which has
   the same main functor as that of the term which appears in the
   @decl{argnames/1} declaration. This is done by first writing the
   functor name, then the infix operator @tt{$}, and then, between
   curly brackets, zero, one, or more pairs
   @em{argument-name}@tt{=>}@em{argument-value}, separated by commas
   (i.e., the infix operator @tt{=>} is used between the name and the
   value). Again, argument names must be atomic. Argument values can
   be any term.  Arguments which are not specified are assumed to have
   a value of ``@tt{_}'' (i.e., they are left unconstrained).

   Thus, after the declaration for @tt{house/5} in the example above,
   any ocurrence in that code of, for example,
   @tt{house$@{nation=>Owns_zebra,pet=>zebra@}} is exactly equivalent
   to @tt{house(_,Owns_zebra,zebra,_,_)}. Also, @tt{house$@{@}} is
   equivalent to @tt{house(_,_,_,_,_)}. The actual zebra puzzle
   specification might include a clause such as:

@noindent
@begin{verbatim}
zebra(Owns_zebra, Drinks_water, Street) :-
   Street = [house$@{@},house$@{@},house$@{@},house$@{@},house$@{@}],
   member(house$@{nation=>Owns_zebra,pet=>zebra@}, Street),
   member(house$@{nation=>Drinks_water,drink=>water@}, Street),
   member(house$@{drink=>coffee,color=>green@}, Street),
   left_right(house$@{color=>ivory@}, house$@{color=>green@}, Street),
   member(house$@{car=>porsche,pet=>snails@}, Street),
        ...
@end{verbatim}

   Another syntax supported, useful mainly in declarations to avoid
   specifying the arity, is @tt{house$@{/@}}, which is equivalent in our
   example to @tt{house/5} (but for data declarations there is a special
   syntax as we have seen).

   Any number of @decl{argnames/1} declarations can appear in a file,
   one for each functor whose arguments are to be accessed by name.
   As with other packages, argument name declarations are @em{local to
   the file} in which they appear. The @decl{argnames/1} declarations
   affect only program text which appears after the
   declaration. It is easy to make a set of declarations affect
   several files for example by putting such declarations in a
   sepatate file which is included by all such files.

   An @decl{argnames/1} declaration does not change in any way the
   internal representation of the associated terms and does not affect
   run-time efficiency. It is simply syntactic sugar.

   @p
   @noindent
   @bf{Runtime support}

   It is possible to write pairs with unbound argument names. In that
   case, runtime information is emitted to resolve the argument name
   at execution time.  
".

:- pred $~(Term, Replacement, NewTerm) # "@var{NewTerm} is as @var{Term}
   but with the arguments specified in @var{Replacement} changed (they
   need to be in argnames syntax). The predicate is in fact virtual,
   since it is translated by the package to a pair of unifications.  For
   example, given the declaration @tt{:- argnames house(color, nation,
   pet, drink, car)}, the goal
@begin{verbatim}
   $~(House, house$@{car => seat, pet => mouse@}, NewHouse)
@end{verbatim}
 would be compiled to the unifications
@begin{verbatim}
   House = house(C,N,_,D,_), NewHouse = house(C,N,mouse,D,seat).
@end{verbatim}".

$~(_,_,_). % todo: change by impl_defined? (jfmc)

:- doc(appendix,"

   Two simple examples of the use of the argnames library package
   follow.  

    @subsection{Using argument names in a toy database}

@noindent
@begin{verbatim}
@includeverbatim{argnames/examples/simple_db}
@end{verbatim}

    @subsection{Complete code for the zebra example}

@noindent
@begin{verbatim}
@includeverbatim{argnames/examples/zebra_argnames}
@end{verbatim}

   ").
