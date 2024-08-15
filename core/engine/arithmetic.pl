:- module(arithmetic, [], [noprelude, assertions, nortchecks, nativeprops, isomodes]).

:- doc(title, "Arithmetic").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Manuel Hermenegildo").

:- doc(usage, "@include{InPrelude.lpdoc}").

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

:- use_module(engine(basiccontrol)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_typing)).

:- if(defined(optim_comp)).
:- '$native_include_c_source'(.(arithmetic)).
:- endif.

:- export((is)/2).
:- doc(is(Val, Exp), "The arithmetic expression @var{Exp} is
   evaluated and the result is unified with @var{Val}").
%:- trust pred is(?term,+arithexpression) + iso.
%:- trust pred is(X,Y) : arithexpression(Y) => num(X) + iso.
:- if(defined(optim_comp)).
:- trust pred is(X,+arithexpression) => num(X) + (iso, native).
:- trust comp is/2 + sideff(free).
:- else.
:- trust pred is(-A, +B) : var * arithexpression
    => (num(A), arithexpression(B), size(A, int(B))) + (not_fails, eval).
:- trust pred is(-A, +B) : var * intexpression
    => (int(A), intexpression(B), size(A, int(B))) + (not_fails, eval).
:- trust pred is(+A, +B) : num * arithexpression + (test_type(arithmetic)).
:- trust pred is(+A, +B) : int * intexpression + (test_type(arithmetic)).
:- trust comp is(A, B) + (size_metric(A, int), size_metric(B, int)).
:- trust comp is/2 + ( iso, native, sideff(free), bind_ins, is_det,
        relations(inf) ).
:- endif.

X is Y :- X is +Y. % (compiled inline, hook for interpreter)

:- export((<)/2).
:- doc((Exp1 < Exp2), "The numeric value of @var{Exp1} is less than
   the numeric value of @var{Exp2} when both are evaluated as arithmetic
   expressions.").
:- if(defined(optim_comp)).
:- trust pred <(+arithexpression,+arithexpression) + (iso, native).
:- trust comp (<)/2 + sideff(free).
:- '$props'((<)/2, [impnat=cblt(bu2_numlt,m(0,0))]).
:- else.
:- trust pred <(+A, +B) : arithexpression * arithexpression
    + (eval, size_metric(A, int), size_metric(B, int)).
:- trust comp (<) /2 + ( iso, native, sideff(free), bind_ins, is_det,
        relations(inf), test_type(arithmetic) ).
X<Y :- X<Y. % (compiled inline, hook for interpreter)
:- endif.

:- export((=<)/2).
:- doc((Exp1 =< Exp2), "The numeric value of @var{Exp1} is less than
   or equal to the numeric value of @var{Exp2} when both are evaluated as
   arithmetic expressions.").
:- if(defined(optim_comp)).
:- trust pred =<(+arithexpression,+arithexpression) + (iso, native).
:- trust comp (=<)/2 + sideff(free).
:- '$props'((=<)/2, [impnat=cblt(bu2_numle,m(0,0))]).
:- else.
:- trust pred =<(+A, +B) : arithexpression * arithexpression
    + (eval, size_metric(A, int), size_metric(B, int)).
:- trust comp (=<) /2 + ( iso, native, sideff(free), bind_ins, is_det,
        relations(inf), test_type(arithmetic) ).
X=<Y :- X=<Y. % (compiled inline, hook for interpreter)
:- endif.

:- export((>)/2).
:- doc((Exp1 > Exp2), "The numeric value of @var{Exp1} is greater than
   the numeric value of @var{Exp2} when both are evaluated as arithmetic
   expressions.").
:- if(defined(optim_comp)).
:- trust pred >(+arithexpression,+arithexpression) + (iso, native).
:- trust comp (>)/2 + sideff(free).
:- '$props'((>)/2, [impnat=cblt(bu2_numgt,m(0,0))]).
:- else.
:- trust pred >(+A, +B) : arithexpression * arithexpression
    + (eval, size_metric(A, int), size_metric(B, int)).
:- trust comp (>) /2 + ( iso, native, sideff(free), bind_ins, is_det,
        relations(inf), test_type(arithmetic) ).
X>Y :- X>Y. % (compiled inline, hook for interpreter)
:- endif.

:- export((>=)/2).
:- doc((Exp1 >= Exp2), "The numeric value of @var{Exp1} is greater than
   or equal to the numeric value of @var{Exp2} when both are evaluated as
   arithmetic expressions.").
:- if(defined(optim_comp)).
:- trust pred >=(+arithexpression,+arithexpression) + (iso, native).
:- trust comp (>=)/2 + sideff(free).
:- '$props'((>=)/2, [impnat=cblt(bu2_numge,m(0,0))]).
:- else.
:- trust pred >=(+A, +B) : arithexpression * arithexpression
    + (eval, size_metric(A, int), size_metric(B, int)).
:- trust comp (>=) /2 + ( iso, native, sideff(free), bind_ins, is_det,
        relations(inf), test_type(arithmetic) ).
X>=Y :- X>=Y. % (compiled inline, hook for interpreter)
:- endif.

:- export((=:=)/2).
:- doc((Exp1 =:= Exp2), "The numeric values of @var{Exp1} and
   @var{Exp2} are equal when both are evaluated as arithmetic expressions.").
:- if(defined(optim_comp)).
:- trust pred =:=(+arithexpression,+arithexpression) + (iso, native).
:- trust comp (=:=)/2 + sideff(free).
:- '$props'((=:=)/2, [impnat=cblt(bu2_numeq,m(0,0))]).
:- else.
:- trust pred =:=(+A, +B) : arithexpression * arithexpression
    + (eval, size_metric(A, int), size_metric(B, int)).
:- trust comp (=:=) /2 + ( iso, native, sideff(free), bind_ins, is_det,
        relations(inf), test_type(arithmetic) ).
X=:=Y :- X=:=Y. % (compiled inline, hook for interpreter)
:- endif.

:- export((=\=)/2).
:- doc((Exp1 =\= Exp2), "The numeric values of @var{Exp1} and
   @var{Exp2} are not equal when both are evaluated as arithmetic
   expressions.").
:- if(defined(optim_comp)).
:- trust pred =\=(+arithexpression,+arithexpression) + (iso, native).
:- trust comp (=\=)/2 + sideff(free).
:- '$props'((=\=)/2, [impnat=cblt(bu2_numne,m(0,0))]).
:- else.
:- trust pred =\=(+A, +B) : arithexpression * arithexpression
    + (eval, size_metric(A, int), size_metric(B, int)).
:- trust comp (=\=) /2 + ( iso, native, sideff(free), bind_ins, is_det,
        relations(inf), test_type(arithmetic) ).
X=\=Y :- X=\=Y. % (compiled inline, hook for interpreter)
:- endif.

:- if(defined(optim_comp)).
% Internal arithmetic predicates (the compiler unfolds is/2 when possible)
:- export('$-'/2).
:- '$props'('$-'/2, [impnat=cfun(fu1_minus,yes)]).
:- export('$+'/2).
:- '$props'('$+'/2, [impnat=cfun(fu1_plus,yes)]).
:- export('$--'/2).
:- '$props'('$--'/2, [impnat=cfun(fu1_sub1,yes)]).
:- export('$++'/2).
:- '$props'('$++'/2, [impnat=cfun(fu1_add1,yes)]).
:- export('$integer'/2).
:- '$props'('$integer'/2, [impnat=cfun(fu1_integer,yes)]).
:- export('$truncate'/2).
:- '$props'('$truncate'/2, [impnat=cfun(fu1_integer,yes)]).
:- export('$float'/2).
:- '$props'('$float'/2, [impnat=cfun(fu1_float,yes)]).
:- export('$\\'/2).
:- '$props'('$\\'/2, [impnat=cfun(fu1_not,yes)]).
:- export('$abs'/2).
:- '$props'('$abs'/2, [impnat=cfun(fu1_abs,yes)]).
:- export('$sign'/2).
:- '$props'('$sign'/2, [impnat=cfun(fu1_sign,yes)]).
:- export('$float_integer_part'/2).
:- '$props'('$float_integer_part'/2, [impnat=cfun(fu1_intpart,yes)]).
:- export('$float_fractional_part'/2).
:- '$props'('$float_fractional_part'/2, [impnat=cfun(fu1_fractpart,yes)]).
:- export('$floor'/2).
:- '$props'('$floor'/2, [impnat=cfun(fu1_floor,yes)]).
:- export('$round'/2).
:- '$props'('$round'/2, [impnat=cfun(fu1_round,yes)]).
:- export('$ceiling'/2).
:- '$props'('$ceiling'/2, [impnat=cfun(fu1_ceil,yes)]).
:- export('$+'/3).
:- '$props'('$+'/3, [impnat=cfun(fu2_plus,yes)]).
:- export('$-'/3).
:- '$props'('$-'/3, [impnat=cfun(fu2_minus,yes)]).
:- export('$*'/3).
:- '$props'('$*'/3, [impnat=cfun(fu2_times,yes)]).
:- export('$/'/3).
:- '$props'('$/'/3, [impnat=cfun(fu2_fdivide,yes)]).
:- export('$//'/3).
:- '$props'('$//'/3, [impnat=cfun(fu2_idivide,yes)]).
:- export('$rem'/3).
:- '$props'('$rem'/3, [impnat=cfun(fu2_rem,yes)]).
:- export('$#'/3).
:- '$props'('$#'/3, [impnat=cfun(fu2_xor,yes)]).
:- export('$/\\'/3).
:- '$props'('$/\\'/3, [impnat=cfun(fu2_and,yes)]).
:- export('$\\/'/3).
:- '$props'('$\\/'/3, [impnat=cfun(fu2_or,yes)]).
:- export('$<<'/3).
:- '$props'('$<<'/3, [impnat=cfun(fu2_lsh,yes)]).
:- export('$>>'/3).
:- '$props'('$>>'/3, [impnat=cfun(fu2_rsh,yes)]).
:- export('$mod'/3).
:- '$props'('$mod'/3, [impnat=cfun(fu2_mod,yes)]).
:- export('$**'/3).
:- '$props'('$**'/3, [impnat=cfun(fu2_pow,yes)]).

:- export('$gcd'/3).
:- '$props'('$gcd'/3, [impnat=cfun(fu2_gcd,yes)]).
:- export('$exp'/2).
:- '$props'('$exp'/2, [impnat=cfun(fu1_exp,yes)]). % (ISO) 
:- export('$log'/2).
:- '$props'('$log'/2, [impnat=cfun(fu1_log,yes)]). % (ISO) 
:- export('$sqrt'/2).
:- '$props'('$sqrt'/2, [impnat=cfun(fu1_sqrt,yes)]). % (ISO) 
:- export('$sin'/2).
:- '$props'('$sin'/2, [impnat=cfun(fu1_sin,yes)]). % (ISO) 
:- export('$cos'/2).
:- '$props'('$cos'/2, [impnat=cfun(fu1_cos,yes)]). % (ISO) 
:- export('$atan'/2).
:- '$props'('$atan'/2, [impnat=cfun(fu1_atan,yes)]). % (ISO) 

% TODO: write in Prolog, obtain c funtion name using pred props
:- export('$eval_arith'/2). % TODO: NOT A EXPORT BUT A INTERNAL ENTRY
:- '$props'('$eval_arith'/2, [impnat=cswitchcfun(
    ('-'/1->fu1_minus
    ;'+'/1->fu1_plus
    ;'--'/1->fu1_sub1
    ;'++'/1->fu1_add1
    ;'integer'/1->fu1_integer
    ;'truncate'/1->fu1_integer
    ;'float'/1->fu1_float
    ;'\\'/1->fu1_not
    ;'abs'/1->fu1_abs
    ;'sign'/1->fu1_sign
    ;'float_integer_part'/1->fu1_intpart
    ;'float_fractional_part'/1->fu1_fractpart
    ;'floor'/1->fu1_floor
    ;'round'/1->fu1_round
    ;'ceiling'/1->fu1_ceil
    ;'+'/2->fu2_plus
    ;'-'/2->fu2_minus
    ;'*'/2->fu2_times
    ;'/'/2->fu2_fdivide
    ;'//'/2->fu2_idivide
    ;'rem'/2->fu2_rem
    ;'#'/2->fu2_xor
    ;'/\\'/2->fu2_and
    ;'\\/'/2->fu2_or
    ;'<<'/2->fu2_lsh
    ;'>>'/2->fu2_rsh
    ;'mod'/2->fu2_mod
    ;'**'/2->fu2_pow
    ;'gcd'/2->fu2_gcd
    ;'exp'/1->fu1_exp
    ;'log'/1->fu1_log
    ;'sqrt'/1->fu1_sqrt
    ;'sin'/1->fu1_sin
    ;'cos'/1->fu1_cos
    ;'atan'/1->fu1_atan
    ))]).
:- endif.

% :- doc(doinclude, arithexpression/1).

:- export(arithexpression/1).
:- prop arithexpression(E) + regtype # "@var{E} is an arithmetic expression.".
:- trust comp arithexpression/1 + sideff(free).
:- if(defined(optim_comp)).
:- else.
:- trust comp arithexpression(+) + (is_det, test_type(arithmetic)).
:- endif.

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

   @item @pred{/\\\\ /2}: integer bitwise and. @hfill @iso

   @item @pred{\\\\/ /2}: integer bitwise or. @hfill @iso

   @item @pred{\\\\ /1}: integer bitwise complement. @hfill @iso

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

:- export(intexpression/1).
:- prop intexpression(E) + regtype # "@var{E} is an integer expression.".
:- trust comp intexpression/1 + sideff(free).

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

% ---------------------------------------------------------------------------
% TODO:[JF] New arithmetic functions, integrated into is/2 !

% NOTE: lsb makes sense only for positive integer
:- export('$lsb'/2).
:- if(defined(optim_comp)).
:- '$props'('$lsb'/2, [impnat=cbool(prolog_lsb)]).
:- else.
:- impl_defined('$lsb'/2).
:- endif.

% NOTE: msb makes sense only for positive integer
:- export('$msb'/2).
:- if(defined(optim_comp)).
:- '$props'('$msb'/2, [impnat=cbool(prolog_msb)]).
:- else.
:- impl_defined('$msb'/2).
:- endif.

% NOTE: popcount makes sense only for positive integer
:- export('$popcount'/2).
:- if(defined(optim_comp)).
:- '$props'('$popcount'/2, [impnat=cbool(prolog_popcount)]).
:- else.
:- impl_defined('$popcount'/2).
:- endif.

