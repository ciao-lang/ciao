:- module(_, _, [assertions, nativeprops, expander]).

:- doc(author, "Edison Mera").

:- doc(module, "Example of runtime checking of an assertion about
	an imported (or even builtin) predicate.").

%:- pred functor(F, A, N) : nonvar(F) => (atom(A), nnegint(N)).

:- pred functor/3 : (nonvar * var * var) => (nonvar * atom * nnegint) + fails.

:- test functor(A,B,C) : (A=0) => (nonvar * atom * nnegint) + fails.

test :-
	functor(0, A, N),
	display(A/N),
	nl.
