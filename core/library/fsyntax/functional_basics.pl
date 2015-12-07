:- module(_,[(++)/3],[fsyntax]).

:- fun_eval arith(true).
:- fun_eval defined(true).
:- include(library(fsyntax/functional_defs)).

[] ++ L := L.
X.Xs ++ L := X.(Xs ++ L).
