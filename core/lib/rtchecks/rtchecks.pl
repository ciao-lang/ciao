:- package(rtchecks).

:- use_package(assertions).
:- use_package(hiord).
:- use_package(library(inliner/inliner_ops)).

:- new_declaration(rtchecked/0).

:- rtchecked.

:- load_compilation_module(library(rtchecks/rtchecks_tr)).
:- add_sentence_trans(rtchecks_tr:rtchecks_sentence_tr/4, 8310).
:- add_goal_trans(rtchecks_tr:rtchecks_goal_tr/3, 8310).

:- set_prolog_flag(runtime_checks, yes).
