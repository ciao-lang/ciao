:- module(arithmetic, [
		(is) /2, (<) /2, (=<) /2, (>) /2, (>=) /2, (=:=) /2, (=\=) /2,
		arithexpression/1, intexpression/1],
	    [assertions, nortchecks, nativeprops, isomodes]).

:- doc(title, "Arithmetic").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Manuel Hermenegildo").

:- doc(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- doc(module, "Arithmetic is performed by built-in predicates which
   take as arguments arithmetic expressions (see
   @pred{arithexpression/1}) and evaluate them.  Terms representing
   arithmetic expressions can be created dynamically, but at the time of
   evaluation, each variable in an arithmetic expression must be bound
   to a non-variable expression (the term must be ground).  For example,
   given the code in the first line a possible shell interaction follows:
@begin{verbatim}
evaluate(Expression, Answer) :- Answer is Expression.

?- _X=24*9, evaluate(_X+6, Ans).

Ans = 222 ?

yes
@end{verbatim}
").

% Compiled inline -- these are hooks for the interpreter.

:- doc(is(Val, Exp), "The arithmetic expression @var{Exp} is
   evaluated and the result is unified with @var{Val}").
%:- trust pred is(?term,+arithexpression) + iso.
%:- trust pred is(X,Y) : arithexpression(Y) => num(X) + iso.

:- doc(bug, "We could improve the precision if we had
	(arithexpression,\+intexpression) but we need a relational
	domain. -- EMM, JFMC").

:- test (X is Y) : (Y = sqrt(4)) => (X = 2.0) # "is/2, sqrt
   test".

:- trust pred is(-A, +B) : var * arithexpression
	=> (num(A), arithexpression(B), size(A, int(B))) + (not_fails, eval).
:- trust pred is(-A, +B) : var * intexpression
	=> (int(A), intexpression(B), size(A, int(B))) + (not_fails, eval).
:- trust pred is(+A, +B) : num * arithexpression + (test_type(arithmetic)).
:- trust pred is(+A, +B) : int * intexpression + (test_type(arithmetic)).
:- trust comp is(A, B) + (size_metric(A, int), size_metric(B, int)).
:- true comp is/2 + ( iso, native, sideff(free), bind_ins, is_det,
	    relations(inf) ).

X is Y :- X is +Y.

:- doc((Exp1 < Exp2), "The numeric value of @var{Exp1} is less than
   the numeric value of @var{Exp2} when both are evaluated as arithmetic
   expressions.").


:- trust pred <(+A, +B) : arithexpression * arithexpression
	+ (eval, size_metric(A, int), size_metric(B, int)).
:- true comp (<) /2 + ( iso, native, sideff(free), bind_ins, is_det,
	    relations(inf), test_type(arithmetic) ).

X<Y :- X<Y.

:- doc((Exp1 =< Exp2), "The numeric value of @var{Exp1} is less than
   or equal to the numeric value of @var{Exp2} when both are evaluated as
   arithmetic expressions.").


:- trust pred =<(+A, +B) : arithexpression * arithexpression
	+ (eval, size_metric(A, int), size_metric(B, int)).
:- true comp (=<) /2 + ( iso, native, sideff(free), bind_ins, is_det,
	    relations(inf), test_type(arithmetic) ).

X=<Y :- X=<Y.

:- doc((Exp1 > Exp2), "The numeric value of @var{Exp1} is greater than
   the numeric value of @var{Exp2} when both are evaluated as arithmetic
   expressions.").


:- trust pred >(+A, +B) : arithexpression * arithexpression
	+ (eval, size_metric(A, int), size_metric(B, int)).
:- true comp (>) /2 + ( iso, native, sideff(free), bind_ins, is_det,
	    relations(inf), test_type(arithmetic) ).

X>Y :- X>Y.

:- doc((Exp1 >= Exp2), "The numeric value of @var{Exp1} is greater than
   or equal to the numeric value of @var{Exp2} when both are evaluated as
   arithmetic expressions.").


:- trust pred >=(+A, +B) : arithexpression * arithexpression
	+ (eval, size_metric(A, int), size_metric(B, int)).
:- true comp (>=) /2 + ( iso, native, sideff(free), bind_ins, is_det,
	    relations(inf), test_type(arithmetic) ).

X>=Y :- X>=Y.

:- doc((Exp1 =:= Exp2), "The numeric values of @var{Exp1} and
   @var{Exp2} are equal when both are evaluated as arithmetic expressions.").


:- trust pred =:=(+A, +B) : arithexpression * arithexpression
	+ (eval, size_metric(A, int), size_metric(B, int)).
:- true comp (=:=) /2 + ( iso, native, sideff(free), bind_ins, is_det,
	    relations(inf), test_type(arithmetic) ).

X=:=Y :- X=:=Y.

:- doc((Exp1 =\= Exp2), "The numeric values of @var{Exp1} and
   @var{Exp2} are not equal when both are evaluated as arithmetic
   expressions.").


:- trust pred =\=(+A, +B) : arithexpression * arithexpression
	+ (eval, size_metric(A, int), size_metric(B, int)).
:- true comp (=\=) /2 + ( iso, native, sideff(free), bind_ins, is_det,
	    relations(inf), test_type(arithmetic) ).

X=\=Y :- X=\=Y.

% :- doc(doinclude, arithexpression/1).

:- prop arithexpression(E) + regtype # "@var{E} is an arithmetic expression.".
:- trust comp arithexpression/1 + sideff(free).
:- trust comp arithexpression(+) + (is_det, test_type(arithmetic)).

:- doc(arithexpression/1, "An arithmetic expression is a term built
   from numbers and @concept{evaluable functors} that represent
   arithmetic functions.  An arithmetic expression evaluates to a
   number, which may be an integer (@pred{int/1}) or a float
   (@pred{flt/1}).  The evaluable functors allowed in an arithmetic
   expression are listed below, together with an indication of the
   functions they represent.  All evaluable functors defined in
   @concept{ISO-Prolog} are implemented, as well as some other useful or
   traditional.  Unless stated otherwise, an expression evaluates to a
   float if any of its arguments is a float, otherwise to an integer.

   @begin{itemize}

   @item @pred{- /1}: sign reversal. @hfill @iso

   @item @pred{+ /1}: identity.

   @item @pred{-- /1}: decrement by one.

   @item @pred{++ /1}: increment by one.

   @item @pred{+ /2}: addition. @hfill @iso

   @item @pred{- /2}: subtraction. @hfill @iso

   @item @pred{* /2}: multiplication. @hfill @iso

   @item @pred{// /2}: integer division.  Float arguments are truncated
   to integers, result always integer. @hfill @iso

   @item @pred{/ /2}: division.  Result always float. @hfill @iso

   @item @pred{rem/2}: integer remainder.  The result is always an
   integer, its sign is the sign of the first argument. @hfill @iso

   @item @pred{mod/2}: modulo. The result is always a positive integer.
   @hfill @iso

   @item @pred{abs/1}: absolute value. @hfill @iso

   @item @pred{sign/1}: sign of. @hfill @iso

   @item @pred{float_integer_part/1}: float integer part. Result always
   float. @hfill @iso

   @item @pred{float_fractional_part/1}: float fractional part. Result always
   float. @hfill @iso

   @item @pred{truncate/1}: The result is the integer equal to the
   integer part of the argument. @hfill @iso

   @item @pred{integer/1}: same as @pred{truncate/1}.

   @item @pred{float/1}: conversion to float. @hfill @iso

   @item @pred{floor/1}: largest integer not greater than. @hfill @iso

   @item @pred{round/1}: integer nearest to. @hfill @iso

   @item @pred{ceiling/1}: smallest integer not smaller than. @hfill @iso

   @item @pred{** /2}: exponentiation.  Result always float. @hfill @iso

   @item @pred{>> /2}: integer bitwise right shift. @hfill @iso

   @item @pred{<< /2}: integer bitwise left shift. @hfill @iso

   @item @pred{/\\ /2}: integer bitwise and. @hfill @iso

   @item @pred{\\/ /2}: integer bitwise or. @hfill @iso

   @item @pred{\\ /1}: integer bitwise complement. @hfill @iso

   @item @pred{# /2}: integer bitwise exclusive or (xor).

   @item @pred{exp/1}: exponential (@em{e} to the power of). Result always
   float. @hfill @iso

   @item @pred{log/1}: natural logarithm (base @em{e}). Result always
   float. @hfill @iso

   @item @pred{sqrt/1}: square root. Result always float. @hfill @iso

   @item @pred{sin/1}: sine. Result always float. @hfill @iso

   @item @pred{cos/1}: cosine. Result always float. @hfill @iso

   @item @pred{atan/1}: arc tangent. Result always float. @hfill @iso 

   @item @pred{gcd/2}: Greatest common divisor.  Arguments must evaluate
   to integers, result always integer.

   @end{itemize}

   In addition to these functors, a list of just a number evaluates to
   this number.  Since a @concept{quoted string} is just a list of
   integers, this allows a quoted character to be used in place of its
   @concept{ASCII code}; e.g. @tt{""A""} behaves within arithmetic
   expressions as the integer 65.  Note that this is not
   @cindex{ISO-Prolog}ISO-compliant, and that can be achieved by using
   the ISO notation @tt{0'A}.

   Arithmetic expressions, as described above, are just data structures.
   If you want one evaluated you must pass it as an argument to one of the
   arithmetic predicates defined in this library.
  ").

arithexpression(X) :- num(X).
arithexpression(-X) :- arithexpression(X).
arithexpression(+X) :- arithexpression(X).
arithexpression(--X) :- arithexpression(X).
arithexpression(++X) :- arithexpression(X).
arithexpression(X+Y) :- arithexpression(X), arithexpression(Y).
arithexpression(X-Y) :- arithexpression(X), arithexpression(Y).
arithexpression(X*Y) :- arithexpression(X), arithexpression(Y).
arithexpression(X//Y) :- arithexpression(X), arithexpression(Y).
arithexpression(X/Y) :- arithexpression(X), arithexpression(Y).
%arithexpression(X rem Y) :- arithexpression(X), arithexpression(Y).
%arithexpression(X mod Y) :- arithexpression(X), arithexpression(Y).
arithexpression(abs(X)) :- arithexpression(X).
arithexpression(sign(X)) :- arithexpression(X).
arithexpression(float_integer_part(X)) :- arithexpression(X).
arithexpression(float_fractional_part(X)) :- arithexpression(X).
%arithexpression(integer(X)) :- arithexpression(X).
%arithexpression(truncate(X)) :- arithexpression(X).
arithexpression(float(X)) :- arithexpression(X).
%arithexpression(floor(X)) :- arithexpression(X).
%arithexpression(round(X)) :- arithexpression(X).
%arithexpression(ceiling(X)) :- arithexpression(X).
arithexpression(X**Y) :- arithexpression(X), arithexpression(Y).
%arithexpression(X>>Y) :- arithexpression(X), arithexpression(Y).
%arithexpression(X<<Y) :- arithexpression(X), arithexpression(Y).
%arithexpression(X/\Y) :- arithexpression(X), arithexpression(Y).
%arithexpression(X\/Y) :- arithexpression(X), arithexpression(Y).
%arithexpression(\(X)) :- arithexpression(X).
%arithexpression(X#Y ) :- arithexpression(X), arithexpression(Y).
arithexpression(exp(X)) :- arithexpression(X).
arithexpression(log(X)) :- arithexpression(X).
arithexpression(sqrt(X)) :- arithexpression(X).
arithexpression(sin(X)) :- arithexpression(X).
arithexpression(cos(X)) :- arithexpression(X).
arithexpression(atan(X)) :- arithexpression(X).
%arithexpression(gcd(X,Y)) :- arithexpression(X), arithexpression(Y).
arithexpression([X]) :- arithexpression(X).
arithexpression(X) :- intexpression(X).

:- prop intexpression(E) + regtype # "@var{E} is an integer expression.".
:- true comp intexpression/1 + sideff(free).

intexpression(X) :- int(X).
intexpression(-X) :- intexpression(X).
intexpression(+X) :- intexpression(X).
intexpression(--X) :- intexpression(X).
intexpression(++X) :- intexpression(X).
intexpression(X+Y) :- intexpression(X), intexpression(Y).
intexpression(X-Y) :- intexpression(X), intexpression(Y).
intexpression(X*Y) :- intexpression(X), intexpression(Y).
intexpression(X//Y) :- arithexpression(X), arithexpression(Y).
intexpression(X rem Y) :- arithexpression(X), arithexpression(Y).
intexpression(X mod Y) :- arithexpression(X), arithexpression(Y).
intexpression(integer(X)) :- arithexpression(X).
intexpression(truncate(X)) :- arithexpression(X).
intexpression(floor(X)) :- arithexpression(X).
intexpression(round(X)) :- arithexpression(X).
intexpression(ceiling(X)) :- arithexpression(X).
intexpression(X**Y) :- intexpression(X), intexpression(Y).
intexpression(X>>Y) :- arithexpression(X), arithexpression(Y).
intexpression(X<<Y) :- arithexpression(X), arithexpression(Y).
intexpression(X/\Y) :- arithexpression(X), arithexpression(Y).
intexpression(X\/Y) :- arithexpression(X), arithexpression(Y).
intexpression(\(X)) :- arithexpression(X).
intexpression(X#Y) :- arithexpression(X), arithexpression(Y).
intexpression(gcd(X, Y)) :- intexpression(X), intexpression(Y).
intexpression([X]) :- intexpression(X).

:- multifile('$internal_error_where_term'/4).

'$internal_error_where_term'('arithmetic:$-', 2, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$+', 2, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$integer', 2, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$float', 2, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$++', 2, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$--', 2, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$+', 3, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$-', 3, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$*', 3, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$/', 3, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$//', 3, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$rem', 3, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$mod', 3, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$abs', 2, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$sign', 2, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$\\', 2, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$#', 3, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$/\\', 3, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$\\/', 3, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$<<', 3, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$>>', 3, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$gcd', 3, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$float_integer_part', 2, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$float_fractional_part', 2, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$floor', 2, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$round', 2, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$ceiling', 2, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$**', 3, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$exp', 2, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$log', 2, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$sqrt', 2, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$sin', 2, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$cos', 2, _, 'arithmetic:is'/2-2).
'$internal_error_where_term'('arithmetic:$atan', 2, _, 'arithmetic:is'/2-2).
