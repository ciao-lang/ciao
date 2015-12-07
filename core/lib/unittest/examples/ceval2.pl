:- module(ceval2, [ceval/2], [assertions, regtypes, nativeprops]).

:- doc(author, "Edison Mera").

:- doc(module, "This example illustrates how the unified approach for
   static verification, run-time checking, and unit testing works. It
   captures the situation in which the programmer changes the main
   functor of a data structure. In this case, the regular type complex
   c(A, B) is renamed to cm(A, B).  This file contains the renamed
   program (with a bug).").

:- entry ceval/2 : gnd * var.

:- regtype complex/1.
:- export(complex/1).

complex(cm(A, B)) :-
	num(A),
	num(B).

:- false test ceval(A, B) : (A = cm(3, 4) + cm(1, 2) - cm(2, 3))
	=> (B = cm(2, 3)) + (not_fails, is_det).

:- test ceval(A, B) : (A = cm(3, 4) * cm(1, 2) / cm(1, 2))
	=> (B = cm(3.0, 4.0)).

:- check pred ceval/2 : gnd * term => gnd * complex.

ceval(A,   A) :- complex(A), !.
ceval(A+B, C) :- ceval(A, CA), ceval(B, CB), add(CA, CB, C).
ceval(A-B, C) :- ceval(A, CA), ceval(B, CB), sub(CA, CB, C).
ceval(A*B, C) :- ceval(A, CA), ceval(B, CB), mul(CA, CB, C).
ceval(A/B, C) :- ceval(A, CA), ceval(B, CB), div(CA, CB, C).

:- pred add/3 : complex * complex * var => complex * complex * complex.
add(cm(A1, B1), cm(A2, B2), cm(A, B)) :-
	A is A1 + A2,
	B is B1 + B2.

:- pred sub/3 : complex * complex * var => complex * complex * complex.
sub(cm(A1, B1), cm(A2, B2), c(A, B)) :- % You forgot to rename c to cm!
	A is A1 - A2,
	B is B1 - B2.

:- pred mul/3 : complex * complex * var => complex * complex * complex.
mul(cm(A1, B1), cm(A2, B2), cm(A, B)) :-
	A is A1*A2 - B1*B2,
	B is A1*B2 + B1*A2.

:- pred div/3 : complex * complex * var => complex * complex * complex.
div(cm(A1, B1), cm(A2, B2), cm(A, B)) :-
	D is A2*A2 + B2*B2,
	A is (A1*A2 + B1*B2) / D,
	B is (A2*B1 - A1*B2) / D.
