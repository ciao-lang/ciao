:- package(statevars).

% State variable notation

:- use_package(xsyntax/'_xsyntax').
:- use_package(xsyntax/'_xcontrol'). % (needed for statevars and loops)

:- fun_eval(statevars(true)). % enable expansion of !X syntax
:- op(60, fx, (!)).

% Scoped {...} goals
:- fun_eval(blkgoal(true)).

% Reassignment and 'let' notation
:- op(980, xfx, [(:=)]). % priority between (::) and (,) (must be the same as in fsyntax/ops.pl)
:- op(985, fx, [(let)]). % TODO: check, only meaningful for blocks
%
:- notation((let Var:=Val), '\6\letvar'(Var,Val)).
:- notation((Var:=Val), '\6\assign'(Var,Val)).
