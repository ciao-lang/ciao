:- package(fd).

:- use_module(library(fd/fd_rt)).

:- push_prolog_flag(unused_pred_warnings, no).
:- include(library(fd/fd_syntax)).
:- include(library(fd/fd_translation)).
:- pop_prolog_flag(unused_pred_warnings).

:- load_compilation_module(library(fd/fd_tr)).
:- add_goal_trans(fd_tr:fd_tr/2, 750). % TODO: Right priority?

