:- module(_,_,[fsyntax]).

:- fun_eval arith(true).
:- fun_eval defined(true).

fact(0) := 1.  
fact(N) := N * fact(--N) :- N > 0.

%% Or,alternatively:
%
% fact(N) := N=0 ? 1
%          | N>0 ? N * fact(--N).


