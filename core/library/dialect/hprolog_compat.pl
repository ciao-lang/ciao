:- package(hprolog_compat).
%:- include(library(dialect/hprolog_compat_ops)).

% TODO: uncertain priority: duplicates the compiler logic, not really compatible with other extensions
%:- load_compilation_module(library(dialect/hprolog_compat_tr)).
%:- add_sentence_trans(hprolog_compat_tr:hprolog_compat_sentence/3, 8010).

% :- use_package(library(hiord)). % for call/N
:- set_prolog_flag(multi_arity_warnings, off).
% :- set_prolog_flag(single_var_warnings, off).
