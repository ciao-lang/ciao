:- module(meta_pred, [p/2, q/2, q/3], []).

:- doc(author, "Edison Mera").

:- doc(module, "This module show a call to p/2 from q/2 inside the
	module itself and should compile successfully.").

:- meta_predicate p(?, addmodule(?)).

p(A,B,C) :-
	display(p(A,B,C)),
	nl.

q(A,B) :-
	p(A,B).

q(A,B,C) :-
	p(A,B,C).
