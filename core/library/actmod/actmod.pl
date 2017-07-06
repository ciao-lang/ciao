:- package(actmod).

:- use_module(library(actmod/actmod_rt)).

:- load_compilation_module(library(actmod/actmod_tr)).
:- add_sentence_trans(actmod_tr:actmodtr/2, 750).
