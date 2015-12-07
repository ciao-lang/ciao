
:- op(700, xfx, [(.=.),(.<>.),(.<.),(.=<.),(.>.),(.>=.)]).

:- true comp ( A .=. B ) + native(A.=.B).
:- impl_defined(.=. /2).
:- true comp ( A .>. B ) + native(A.>.B).
:- impl_defined(.>. /2).
:- true comp ( A .<. B ) + native(A.<.B).
:- impl_defined(.<. /2).
:- true comp ( A .>=. B ) + native(A.>=.B).
:- impl_defined(.>=. /2).
:- true comp ( A .=<. B ) + native(A.=<.B).
:- impl_defined(.=<. /2).
% Nop! :- true comp ( A .<>. B ) + native(A.<>.B).
:- impl_defined(.<>. /2).
