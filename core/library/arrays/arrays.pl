:- package(arrays).

:- use_package(fsyntax). % TODO: :- use_package(fsyntaxplus). ?
:- use_package(statevars).

% TODO: This notation could also be used for assocs as if they were Python dictionaries?

% Postfix blocks enable X[I1,...In] syntax
:- push_prolog_flag(read_postfix_blocks, on).
:- op(40, yf, ['[]']). % (inside a list, otherwise the list is empty!)
:- notation('\6\postfix_block'(X,[I]), ~get_elem(X,I)).
% TODO: make it optional?
:- notation((Var[I]:=Val), '\6\assign'(Var,~replace_elem(Var,I,Val))).

% (Interface definitions)
%   get_elem/3
%   replace_elem/3
%   array_length/3
:- include(library(arrays/arrays_itf)).
