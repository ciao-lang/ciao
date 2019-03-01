
:- op(700, xfx, [(.=.),(.<>.),(.<.),(.=<.),(.>.),(.>=.)]).

:- trust comp ( A .=. B ) + native(A.=.B).
:- impl_defined(.=. /2).
:- trust comp ( A .>. B ) + native(A.>.B).
:- impl_defined(.>. /2).
:- trust comp ( A .<. B ) + native(A.<.B).
:- impl_defined(.<. /2).
:- trust comp ( A .>=. B ) + native(A.>=.B).
:- impl_defined(.>=. /2).
:- trust comp ( A .=<. B ) + native(A.=<.B).
:- impl_defined(.=<. /2).
% Nop! :- trust comp ( A .<>. B ) + native(A.<>.B).
:- impl_defined(.<>. /2).
