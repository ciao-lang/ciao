:- use_package(assertions).
:- doc(nodoc,assertions).
:- doc(nodoc,assertions_basic).

:- doc(title, "Lazy evaluation").

:- doc(author, "Amadeo Casas").
:- doc(author, "Jose F. Morales (minor modifications)").

:- doc(module, "This library package allows the use of lazy evaluation
   in a Ciao module/program.

   Lazy Evaluation is a program evaluation technique used particularly in
   functional languages. When using lazy evaluation, an expression is not
   evaluated as soon as it is assigned, but rather when the evaluator is
   forced to produce the value of the expression.  Although the @tt{when}
   or @tt{freeze} control primitives present in many modern logic
   programming systems are more powerful than lazy evaluation, they lack
   the simplicity of use and cleaner semantics of functional lazy
   evaluation.

   The objective of this package is to allow evaluating the functions
   lazily. Functions are the subset of relations (predicates) which have a
   designated argument through which a single output is obtained for any
   set of inputs (the other arguments). In logic programming systems which
   have syntactic support for functions (including Ciao), functions are
   typically translated to predicates whose @tt{last} argument is
   designated as a (single value) output and the rest as inputs.

   In our proposal, a function can be declared as lazy via the following
   declaration:


@begin{verbatim}
:- lazy fun_eval f/N.
@end{verbatim}


   This function could be represented as:

@begin{verbatim}
:- lazy fun_eval f(~_,_,_,_).
@end{verbatim}

   where ~ indicates the argument through which the single output will be
   obtained. Another possible representation may be:

@begin{verbatim}
:- lazy fun_return f(~_,_,_,_).
@end{verbatim}

@noindent
   In order to achieve the intended behavior, the execution of each
   function declared as lazy is suspended until the return value of the
   function is needed.

   A simple example of the use of lazy evaluation would be the definition
   of a function which returns the (potentially) infinite list of integers
   starting with a given one:

@begin{verbatim}
:- lazy fun_eval nums_from/1.
nums_from(X) := [X | nums_from(X+1)].
@end{verbatim}


@noindent
   While lazy functions certainly increase the overhead in the execution,
   they also allow the user to develop in an easy way predicates which can
   handle infinite terms, and this is the main advantage of the proposed
   functionality.

   Lazy evaluation can be also a better option than eager evaluation when a
   function in a different module is used and it returns a big amount of
   data. As an example, we have the following module @tt{module1}:

@includecode{lazy/examples/module1.pl}

@noindent
   and another module @tt{module2}:

@includecode{lazy/examples/module1.pl}

@noindent
   Function @tt{test/0} in module @tt{m1} needs to execute function
   @tt{squares/1}, in module @tt{m2}, which will return a very long
   list (in the case of this example this list will be infinite, but the
   conclusions also apply with finite but long lists). If
   @tt{squares/1} were executed eagerly then the entire list would be
   returned, to immediately execute the @tt{take/2} function with the
   entire list, but creating this intermediate result is wasteful in terms
   of memory requirements.  In order to solve this problem, the
   @tt{squares/1} function could be moved to module @tt{m1} and
   merged with @tt{take/2} (or, also, they could exchange a size
   parameter). But rearranging the program is not always possible and may
   perhaps complicate other aspects of the overall program design.

   If instead the @tt{squares/1} function is evaluated lazily, it is
   possible to keep the definitions unchanged and in different modules and
   there will be a smaller memory penalty for storing the intermediate
   result. As more values are needed by the @tt{take/2} function, more
   values in the list returned by @tt{squares/1} are built (in this
   example, only 10 values). These values that have been consumed and
   passed over will be recovered by the garbage collector and the
   corresponding memory freed. The query:


@begin{verbatim}
?- test(X).
@end{verbatim}


@noindent
   will compute @var{X} = [1,4,9,16,25,36,49,64,81,100].

   Some common lazy predicates are provided in @lib{lazy_lib}.
").


:- doc(appendix, "The translation of the code in order to execute it
   lazily is explained below.

   A sentence translation is provided to handle the @tt{lazy} directives.
   The translation of a lazy function into a predicate is done in two
   steps.  First, the function is converted into a predicate (using the
   fsyntax package). Then, the resulting predicate is transformed to
   suspend its execution until the value of the last variable (i.e., the
   output variable) is needed.  This suspension is achieved by the use of
   the @tt{freeze/1} control primitive that many modern logic programming
   systems implement quite efficiently (@tt{block} or @tt{when}
   declarations can obviously also be used, but we explain the
   transformation in terms of @tt{freeze} because it is more widespread).
   The translation will rename the original predicate to an internal name
   and add a bridge predicate with the original name which invokes the
   internal predicate through a call to @tt{freeze/1}. This will delay the
   execution of the internal predicate until its result is required, which
   will be detected as a binding (i.e., demand) of its output variable.

   We show now an example of the use of lazy evaluation, and how a lazy
   function is translated by this package. The following code returns an
   (infinite) list of fibonacci numbers:


@begin{verbatim}
:- lazy fun_eval fiblist/0.
fiblist := [0, 1 | ~zipWith(add, FibL, ~tail(FibL))]
    :- FibL = fiblist.
@end{verbatim}


@noindent
   which is translated into:


@begin{verbatim}
fiblist(X) :- 
      freeze(X, 'fiblist_$$lazy$$'(X)).

'fiblist_$$lazy$$'([0, 1 | Rest]) :- 
      fiblist(FibL), 
      tail(FibL, T), 
      zipWith(add, FibL, T, Rest).
@end{verbatim}


@noindent

   In the @tt{fiblist} function defined, any element in the resulting
   infinite list of fibonacci numbers can be referenced, as for example,
   @tt{nth(X, ~fiblist, Value).}. The other functions used in the
   definition are @tt{tail/2} and @tt{zipWith/3}. These two functions can
   be found in the @bf{lazy_lib.pl} runtime file.

").

:- use_package(lazy).
