:- package(actmod).

:- use_package(fibers).

:- include(library(actmod/actmod_hooks)).

:- use_module(library(actmod/actmod_rt)).

:- load_compilation_module(library(actmod/actmod_tr)).
:- add_sentence_trans(actmod_tr:sentence_tr/3, 750).

