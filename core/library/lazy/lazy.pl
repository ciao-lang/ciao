:- package(lazy).

:- include(library(lazy/ops)).
:- use_module(library(freeze)).

:- load_compilation_module(library(lazy/lazytr)).
:- add_sentence_trans(lazytr:lazy_sentence_translation/3, 620).
