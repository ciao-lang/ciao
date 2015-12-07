:- package(dcg).

:- load_compilation_module(library(dcg/dcg_tr)).
:- add_sentence_trans(dcg_tr:dcg_translation/3, 310).
:- add_goal_trans(dcg_tr:dcg_translation_goal/3, 310).

:- include(library(dcg/dcg_ops)).

