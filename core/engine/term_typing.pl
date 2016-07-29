:- module(term_typing, [
		var/1, nonvar/1,
		atom/1, integer/1, float/1, number/1, atomic/1,
		ground/1, type/2],
	    [assertions, nortchecks, nativeprops, isomodes]).

:- doc(title, "Extra-logical properties for typing").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Manuel Hermenegildo").

:- doc(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- doc(module, "This library contains traditional Prolog predicates
        for testing types.  They depend on the state of instantiation of
        their arguments, thus being of extra-logical nature.").

:- true prop ground(X) + native
# "@var{X} is currently ground (it contains no variables).".
:- trust success ground(X) => gnd(X).
:- trust comp ground/1 + (is_det, test_type(meta)).
:- true comp ground(@X) + (sideff(free), native).
:- true comp ground(X) : ground(X) + eval.
:- true comp ground(X) : var(X) + equiv(fail).
:- true comp ground(X) : ground(X) + equiv(true).

/* % jfran: Now implemented in C 
ground(Term):-
	nonvar(Term),
	functor(Term,_,N),
	ground_(N,Term).

ground_(0,_) :- !.
ground_(N,Term):-
	N > 0,
	arg(N,Term,Arg),
	ground(Arg),
	N1 is N-1,
	ground_(N1,Term).
*/
:- impl_defined(ground/1).

% Compiled inline -- these are hooks for the interpreter.

:- true prop atom(X) + native
# "@var{X} is currently instantiated to an atom.".
:- trust success atom(X) => atm(X).
:- trust comp atom/1 + (is_det, test_type(arithmetic)).
:- true comp atom(@X) + (sideff(free), native).
:- true comp atom(X) : nonvar(X) + eval.
:- true comp atom(T) : var(T) + equiv(fail).

atom(X) :- atom(X).

:- true prop atomic(X) + native
# "@var{X} is currently instantiated to an atom or a number.".
:- trust success atomic(T) => constant(T).
:- trust comp atomic/1 + (is_det, test_type(meta)).
:- true comp atomic(@X) + (sideff(free), native).
:- true comp atomic(X) : nonvar(X) + eval.
:- true comp atomic(T) : var(T) + equiv(fail).

atomic(X) :- atomic(X).

:- true prop float(X) + native
# "@var{X} is currently instantiated to a float.".
:- trust success float(X) => flt(X).
:- trust comp float/1 + (is_det, test_type(meta)).
:- true comp float(@X) + (sideff(free), native).
:- true comp float(X) : nonvar(X) + eval.
:- true comp float(T) : var(T) + equiv(fail).

float(X) :- float(X).

:- true prop integer(X) + native
# "@var{X} is currently instantiated to an integer.".
:- trust success integer(X) => int(X).
:- trust comp integer/1 + (is_det, test_type(arithmetic)).
:- true comp integer(@X) + (sideff(free), native).
:- true comp integer(X) : nonvar(X) + eval.
:- true comp integer(T) : var(T) + equiv(fail).

integer(X) :- integer(X).

:- true prop nonvar(X) + native(not_free(X))
# "@var{X} is currently a term which is not a free variable.".
:- trust comp nonvar/1 + is_det.
:- true comp nonvar(@X) + (sideff(free), native).
:- true comp nonvar(X) : nonvar(X) + eval.
:- true comp nonvar(T) : var(T) + equiv(fail).
:- true comp nonvar(T) : nonvar(T) + equiv(true).

nonvar(X) :- nonvar(X).

% :- true prop nv(T) + regtype.

% nv(T) :- atm(T).
% nv(T) :- num(T).
% nv(T) :- struct(T).


:- true prop number(X) + native
# "@var{X} is currently instantiated to a number.".
:- trust success number(X) => num(X).
:- trust comp number/1 + (is_det, test_type(arithmetic)).
:- true comp number(@X) + (sideff(free), native).
:- true comp number(X) : nonvar(X) + eval.
:- true comp number(T) : var(T) + equiv(fail).

number(X) :- number(X).

:- true prop var(X) + native(free(X))
# "@var{X} is a free variable.".
:- trust comp var/1 + (is_det, test_type(meta)).
:- true comp var(@X) + (native, sideff(free)).
:- true comp var(X) : nonvar(X) + eval.
:- true comp var(X) : nonvar(X) + equiv(fail).
:- true comp var(X) : var(X) + equiv(true).

var(X) :- var(X).

:- true prop type(X, Y) + native
# "@var{X} is internally of type @var{Y} (@tt{var}, @tt{attv}, @tt{float},
      @tt{integer}, @tt{structure}, @tt{atom} or @tt{list}).".
:- trust success type(X, Y) => atm(Y).
:- trust comp type/2 + is_det.
:- true comp type/2 + (sideff(free), native).
:- true comp type(X, Y) : nonvar(X) + eval.

type(X, Y) :- type(X, Y).
