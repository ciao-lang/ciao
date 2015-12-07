:- package(yap_compat).
:- include(library(dialect/yap_compat_ops)).

% TODO: uncertain priority: duplicates the compiler logic, not really compatible with other extensions
:- load_compilation_module(library(dialect/yap_compat_tr)).
:- add_sentence_trans(yap_compat_tr:yap_compat_sentence/3, 8010).
%:- add_goal_trans(yap_compat_tr:yap_compat_goal/3).

:- use_package(library(hiord)). % for call/N
:- set_prolog_flag(multi_arity_warnings, off).
% :- set_prolog_flag(single_var_warnings, off).
