:- package(statevars).

% State variable notation

:- use_package(xsyntax).
:- use_package(xsyntax/'_xcontrol'). % (needed for statevars and loops)

:- fun_eval(statevars(true)). % enable expansion of !X syntax
:- op(60, fx, (!)).

% Reassignment and 'let' notation
:- op(980, xfx, [(<-)]). % priority between (::) and (,)
:- op(985, fx, [(let)]). % TODO: check, only meaningful for blocks
%
:- fun_eval(notation((let Var<-Val), '\6\letvar'(Var,Val))).
:- fun_eval(notation((Var<-Val), '\6\assign'(Var,Val))).
