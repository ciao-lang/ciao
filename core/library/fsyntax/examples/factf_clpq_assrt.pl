:- module(_,_,[assertions,nativeprops,fsyntax,clpqf]).

:- fun_eval .=. /1.
:- op(700,fx,[.=.]).
:- fun_eval fact/1.

:- pred fact(+int,-int) + is_det.
:- pred fact(-int,-int) + non_det.

fact( .=. 0) := .=. 1.
fact(N) := .=. N*fact( .=. N-1 ) :- N .>. 0. 
