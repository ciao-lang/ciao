:- use_package([assertions,metaprops]).
:- doc(nodoc,assertions).
:- doc(nodoc,assertions_basic).
:- doc(nodoc,metaprops).
:- doc(hide,callme/2). % TODO: needed since nodoc above does not hide this predicate

:- doc(title,"Classical Prolog modes").
 
:- doc(author,"Manuel Hermenegildo").

:- doc(module,"This package defines a number of @concept{modes} which
   are frequently useful in programs when describing predicates, e.g.,
   via @decl{pred} assertions (or @em{doccomments}, see below).
   They correspond to the modes used in
   many classical Prolog texts and code as documentation, with some
   additions. In Ciao these modes are actually syntactic sugar for
   assertions, so that they can be checked either statically or
   dynamically. Note that some of these modes use the same symbol as
   one of the @lib{basicmodes} and @lib{isomodes} packages (see
   @ref{Some basic Prolog modes} and @ref{ISO-Prolog modes}) but have
   in some cases subtly different meaning.

   As an example, the following declaration:

@begin{verbatim}
:- pred is(-,+).
@end{verbatim}

   Expresses that @pred{is/2} should be called with the second argument
   bound and it will bind the first argument. Also: 

@begin{verbatim}
:- pred is(-num,+arithexpression).
@end{verbatim}

   (more precise than the above), expresses that @pred{is/2} should be
   called with the second argument @em{instantiated} to an arithmetic
   expression and that on success it will bind the first argument to a
   number. The argument of a mode as above can be any property,
   including, e.g., regular types. 

   The first declaration is equivalent to (and is in fact translated
   to) the assertion:

@begin{verbatim}
:- pred is(X,Y) : nonvar(Y) => nonvar(X). 
@end{verbatim}

   and the second one to:

@begin{verbatim}
:- pred is(X,Y) : arithexpression(Y) => num(X). 
@end{verbatim}

   Modes can also be included inside comments in markdown format (see
   the @lib{doccomments} and @lib{markdown} libraries). For example:

@begin{verbatim}
%! pred is(-,+):
%  Evaluates the expression in the second argument
%  and binds the first argument with the result. 
@end{verbatim}

   which are also translated to the corresponding assertions.").

:- use_package(modes).

:- doc( + /1,"The argument should be bound (nonvar) when the predicate is
   called. For example:

@begin{verbatim}
:- pred + > +.
@end{verbatim}

   expresses that both arguments of @pred{>/2} should be bound when
   the predicate is called.").

:- doc( + /2,"This argument should be @em{instantiated} when the
   predicate is called. For example:

@begin{verbatim}
:- pred +arithexpression > +arithexpression.
@end{verbatim}

   expresses that both arguments of @pred{>/2} should be bound to
   arithmetic expressions (have the @prop{arithexpression} property)
   when the predicate is called.  ").

:- doc( - /1,"The argument is an output argument. It may be bound or
   not at call time. It will be bound (nonvar) if the predicate
   succeeds.").

:- doc( - /2,"The argument is an output argument. It may be bound or
   not at call time. It will be @em{instantiated} to a term that has
   the indicated type or property if the predicate succeeds. For
   example, this assertion:

@begin{verbatim}
:- pred length(-list,-int).
@end{verbatim}

   expresses that @pred{length/2} can be called in any mode, but on
   output the second argument will be @em{instantiated} to a number
   and the first one will be @em{instantiated} to a list. Note that
   this does not mean that the list will be ground, but rather that it
   will be a complete list but whose elements can be any term,
   including variables (see the discussion of instantitation and
   compatibility types in @ref{Declaring regular types}.").

:- doc(-- /1,"The argument should be a free variable (i.e., unbound)
   when the predicate is called.").

:- doc(-- /2,"The argument should be a free variable (i.e., unbound)
   when the predicate is called and will be bound to a term that has
   the indicated type or property in general, if the predicate
   succeeds.").

:- doc( ? /1,"No information is given on this argument.").

:- doc( ? /2,"The argument can be a variable or, if it is
   instantiated, it is to a term that is @em{compatible} with the
   indicated type or property.").

:- doc( @ /1,"The argument will not be further instantiated, i.e.,
   will not be more instantiated than when the predicate is called.").

:- doc( @ /2,"The argument will not be further instantiated, i.e.,
   will not be more instantiated than when the predicate is called,
   and the term is @em{compatible} with the indicated type or property.").

:- doc( in/1,"The argument is ground at call time.").

:- doc( in/2,"The argument is ground at call time and is @em{compatible}
   with the indicated type or property.").

:- doc(++ /1,"Same as @tt{in}: the argument is ground at call time.").

:- doc(++ /2,"Same as @tt{in}: the argument is ground at call time and
   is @em{compatible} with the indicated type or property.").

:- doc(out/1,"The argument is a variable when the predicate is called
   and will be ground if the predicate succeeds.").

:- doc(out/2,"The argument is a variable when the predicate is called
   and will be bound to a ground term that is @em{compatible} with the
   indicated type or property, if the predicate succeeds.").

:- doc( go/1,"The argument is ground by the predicate, if it
   succeeds.").

:- doc( go/2,"The argument is ground by the predicate to a ground term
   that is @em{compatible} with the indicated type or property, if the
   predicate succeeds.").

