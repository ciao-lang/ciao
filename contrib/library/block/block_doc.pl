:- use_package(assertions).
:- doc(nodoc,assertions). 

:- doc( title, "Block Declarations").
:- doc( author, "R@'{e}my Haemmerl@'{e}").
:- doc( version(0*1+0,2008/25/05), "first naive implementation").

:- doc( module, "This package provides compatibility with SICStus'
block declarations").

:- true decl block(BlockSpecs) : sequence_or_list(callable) # 

"In this declaration @var{BlockSpecs} specifies a disjunction of conditions.
 Each condition is of the form @var{predname(C1, ..., CN)} where each
 @var{CI} is either a `@var{-}' if the call must suspend until the
 corresponding argument is bound, or anything else otherwise.

@item @em{Convention:} The recommended style is to write the block
declarations in front of the source code of the predicate they refer
to. Indeed, they are part of the source code of the predicate and must
precede the first clause. Moreover it is suggested to use `@var{?}'
for specifying non conditioned arguments.

@item @em{Example :} The following definition calls to @var{merge/3}
having uninstantiated arguments in the first and third position or in
the second and third position will suspend.

@begin{verbatim}

:- block merge(-,?,-), merge(?,-,-).

merge([], Y, Y).
merge(X, [], X).
merge([H|X], [E|Y], [H|Z]) :- H @@< E,  merge(X, [E|Y], Z).
merge([H|X], [E|Y], [E|Z]) :- H @@>= E, merge([H|X], Y, Z).

@end{verbatim}
".


:- op(1150, fx, block).

