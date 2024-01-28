:- use_package(functional).

% Postfix blocks enable X[I1,...In] syntax
:- push_prolog_flag(read_postfix_blocks, on).
:- op(40, yf, ['[]']). % (inside a list, otherwise the list is empty!)
:- fun_eval abbrev('\6\postfix_block'(X,Idx), ~get_elem(X,Idx)). % (X[I1,...,In])

:- op(500,yfx,<+>).
:- fun_eval '<+>'/2.

:- op(400,yfx,<*>).
:- fun_eval '<*>'/2.

