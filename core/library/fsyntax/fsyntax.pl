:- package(fsyntax).
:- include(library(fsyntax/ops)).
:- use_package(xsyntax/'_xsyntax').
%
:- if(defined(optim_comp)).
% TODO: no fine grained control!
:- '$pragma'(functional_expand).
:- else.
:- fun_eval(funhead(true)). % Head := R notation
:- fun_eval(funexp(true)). % ~X
:- fun_eval(arithfunexp(true)). % allow arith in ~X
:- fun_eval(quote(true)). % ^X quoting (avoid main functor expansion)
:- fun_eval(condexp(true)). % (C ? T | E)
:- endif.
