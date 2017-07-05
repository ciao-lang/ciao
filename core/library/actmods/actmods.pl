:- package(actmods).

:- use_module(library(actmods/actmod_rt)).

:- load_compilation_module(library(actmods/actmod_tr)).
:- add_sentence_trans(actmod_tr:actmodtr/2, 750).
