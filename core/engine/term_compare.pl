:- module(term_compare, [
        (==)/2, (\==)/2, (@<)/2, (@=<)/2, (@>)/2, (@>=)/2, compare/3],
        [assertions, nortchecks, nativeprops, isomodes]).

:- doc(title,"Comparing terms").

:- doc(author,"Daniel Cabeza").
:- doc(author,"Manuel Hermenegildo").

:- doc(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- doc(module, "These built-in predicates are extra-logical. They
treat uninstantiated variables as objects with values which may be
compared, and they never instantiate those variables. They should
@em{not} be used when what you really want is arithmetic comparison or
unification.

The predicates make reference to a @index{standard total ordering} of terms,
which is as follows:

@begin{itemize}

@item Variables, by age (roughly, oldest first -- the order is @em{not}
 related to the names of variables).

@item Floats, in numeric order (e.g. -1.0 is put before 1.0). 

@item Integers, in numeric order (e.g. -1 is put before 1). 

@item Atoms, in alphabetical (i.e. character code) order. 

@item Compound terms, ordered first by arity, then by the name of the
    principal functor, then by the arguments in left-to-right
    order. Recall that lists are equivalent to compound terms with
    principal functor @tt{'.'/2}.

@end{itemize}

For example, here is a list of terms in standard order: 

@begin{verbatim}
[ X, -1.0, -9, 1, bar, foo, [1], X = Y, foo(0,2), bar(1,1,1) ]
@end{verbatim}
").

% Compiled inline -- these are hooks for the interpreter.


:- prop (Term1 == Term2)
	# "The terms @var{Term1} and @var{Term2} are strictly identical.".
:- true comp (@Term1 == @Term2) + ( sideff(free), native ).
:- true comp (Term1 == Term2) : (ground(Term1), ground(Term2)) + eval.
:- trust comp (Term1 == Term2) + (is_det, test_type(unification)).

X==Y :- X==Y.

:- trust pred (@Term1 \== @Term2) + ( sideff(free), native )
	# "The terms @var{Term1} and @var{Term2} are not strictly identical.".
:- true comp (Term1 \== Term2) : (ground(Term1), ground(Term2)) + eval.
:- trust comp (Term1 \== Term2) + (is_det, test_type(unification)).

X\==Y :- X\==Y.

:- trust pred (@Term1 @< @Term2) + ( sideff(free), native )
	# "The term @var{Term1} precedes the term @var{Term2} in the 
           standard order.".
:- true comp (Term1 @< Term2) : (ground(Term1), ground(Term2)) + eval.

X@<Y :- X@<Y.


:- trust pred (@Term1 @=< @Term2) + ( sideff(free), native )
	# "The term @var{Term1} precedes or is identical to the term
           @var{Term2} in the standard order.".
:- true comp (Term1 @=< Term2) : (ground(Term1), ground(Term2)) + eval.

X@=<Y :- X@=<Y.


:- trust pred (@Term1 @> @Term2) + ( sideff(free), native )
	# "The term @var{Term1} follows the term @var{Term2} in the 
           standard order.".
:- true comp (Term1 @> Term2) : (ground(Term1), ground(Term2)) + eval.

X@>Y :- X@>Y.


:- trust pred (@Term1 @>= @Term2) + ( sideff(free), native )
	# "The term @var{Term1} follows or is identical to the term
           @var{Term2} in the standard order.".
:- true comp (Term1 @>= Term2) : (ground(Term1), ground(Term2)) + eval.

X@>=Y :- X@>=Y.

:- doc(compare(Op,Term1,Term2) , "@var{Op} is the result of
           comparing the terms @var{Term1} and @var{Term2}.").

%:- trust pred compare(?atm,@term,@term)
%	=> member([(=),(>),(<)]) * term * term + ( sideff(free), native ).


:- trust pred compare(?atm,@term,@term)
	=> comparator * term * term + ( sideff(free), native ).

:- true comp compare(_,Term1,Term2) : (ground(Term1), ground(Term2)) + eval.

compare(X, Y, Z) :- compare(X, Y, Z).

:- export(comparator/1).
:- prop comparator/1 + regtype.
comparator((=)).
comparator((>)).
comparator((<)).
