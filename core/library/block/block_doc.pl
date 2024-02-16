:- use_package(assertions).
:- doc(nodoc,assertions). 
:- doc(nodoc,assertions_basic). 

:- doc(title, "Block declarations").
:- doc(author, "R@'{e}my Haemmerl@'{e}").
:- doc(author, "Jose F. Morales (documentation improvements)").

:- doc(version(0*1+0,2008/25/05), "First naive implementation").

:- doc(module, "This package provides compatibility with SICStus'
   block declarations.

   @bf{Convention:} The recommended style is to write the block
   declarations in front of the source code of the predicate they
   refer to. Indeed, they are part of the source code of the predicate
   and must precede the first clause. Moreover it is suggested to use
   @tt{?} for specifying non conditioned arguments.

   @bf{Example:} The following definition calls to @pred{merge/3}
   having uninstantiated arguments in the first and third position or
   in the second and third position will suspend.

```
:- block merge(-,?,-), merge(?,-,-).

merge([], Y, Y).
merge(X, [], X).
merge([H|X], [E|Y], [H|Z]) :- H @< E,  merge(X, [E|Y], Z).
merge([H|X], [E|Y], [E|Z]) :- H @>= E, merge([H|X], Y, Z).
```

   @bf{Simulating @tt{block} with @pred{when/2} predicate}: In the
   predicate above, execution of @tt{merge(X,Y,Z)} is suspended while
   @tt{(var(X),var(Z);var(Y),var(Z))} holds. A similar effect can be
   obtained with @pred{when/2} using the negated condition
   @tt{((nonvar(X);nonvar(Z)),(nonvar(Y);nonvar(Z)))}. 

```
:- use_module(library(when)).

merge(X,Y,Z) :-
    when(((nonvar(X);nonvar(Z)),
          (nonvar(Y);nonvar(Z))), merge_(X,Y,Z)).

merge_([], Y, Y).
merge_(X, [], X).
merge_([H|X], [E|Y], [H|Z]) :- H @< E,  merge(X, [E|Y], Z).
merge_([H|X], [E|Y], [E|Z]) :- H @>= E, merge([H|X], Y, Z).
```
").

:- decl block(BlockSpecs) : sequence_or_list(cgoal) #
   "@var{BlockSpecs} specifies a disjunction of conditions.  Each
   condition is of the form @tt{predname(C1, ..., CN)} where each
   @var{CI} is either a @tt{-} if the call must suspend until the
   corresponding argument is bound, or anything else otherwise.".

:- op(1150, fx, block).
