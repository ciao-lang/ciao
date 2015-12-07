
:- use_package([assertions,pure]).
:- doc(nodoc,assertions).
:- doc(nodoc,pure).

:- use_module(library(assertions/assertions_props)).

:- doc(filetype,package).

:- doc(title,"Declaring regular types").
 
:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Pedro L@'{o}pez").
:- doc(author, "Francisco Bueno").

:- doc(module,"This library package adds declarations and new
   operator definitions which provide simple syntactic sugar to write
   @concept{regular type definitions} in source code.  Regular types
   are just properties which have the additional characteristic of
   being @concept{regular types} (@pred{basic_props:regtype/1}),
   defined below.

   For example, this library package allows writing:
   @begin{verbatim}
   :- regtype tree(X) # ""@var{X} is a tree."".
   @end{verbatim}
   instead of the more cumbersome:
   @begin{verbatim}
   :- prop tree(X) + regtype # ""@var{X} is a tree."".
   @end{verbatim}

   Regular types can be used as properties to describe
   predicates and play an essential role in program debugging (see the 
   Ciao Prolog preprocessor (@tt{ciaopp}) manual).

   In this chapter we explain some general considerations worth taking
   into account when writing properties in general, not just regular
   types. 

   @include{regtypes/writing_props.lpdoc}
").


:- use_package(library(regtypes)).

:- doc(regtype/1, "@cindex{regtype assertion} This assertion is
    similar to a prop assertion but it flags that the property being
    documented is also a ``@concept{regular type}.''  Regular types
    are properties whose definitions are @em{regular programs} (see
    lelow).  This allows for example checking whether it is in the
    class of types supported by the regular type checking and
    inference modules.

    @include{regtypes/regular_type_syntax.lpdoc}

    The set of regular types is thus a well defined subset of the set
    of properties. Note that types can be used to describe
    characteristics of arguments in assertions and they can also be
    executed (called) as any other predicates.  "

).

:- decl regtype(AssertionBody) : assrt_body.

%% :- decl type/1
%%    # "Same as @pred{regtype/1}. Deprecated. Included only for
%%       backwards compatibility.".

%:- new_declaration(regtype/2).
:- doc(regtype/2,
    "@cindex{regtype assertion} This assertion is similar to a
     @pred{regtype/1} assertion but it is explicitely qualified.
     Non-qualified @pred{regtype/1} assertions are assumed the qualifier
     @tt{check}.
     Note that checking regular type definitions should be done with the
     @tt{ciaopp} preprocessor.
").
:- decl regtype(AssertionStatus,AssertionBody) : assrt_status * assrt_body.

%% :- decl type/2
%%    # "Same as @pred{regtype/2}. Deprecated. Included only for
%%       backwards compatibility.".
