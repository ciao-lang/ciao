:- package('_xcontrol'). % (internal, do not use directly)

% Low-level expansion support for statevars, loops, predicate
% abstractions with shared-by-default variables 
% (`-[X1...Xn] -> Head :- Body`).

:- load_compilation_module(library(xsyntax/xcontrol_tr)).
:- add_clause_trans(xcontrol_tr:tr_clause/3, 108900). % (after_mexp phase)
:- use_module(library(xsyntax/xcontrol_rt)).
