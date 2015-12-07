:- package(commons_compat).

% We turn on assertions and doccomments by default
:- use_package([assertions,doccomments]).

:- include(library(dialect/commons_compat_ops)).

:- load_compilation_module(library(dialect/commons_compat_tr)).
% :- add_sentence_trans(commons_compat_tr:commons_sentence/3).

:- include(library(dialect/commons_modes)).
