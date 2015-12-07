:- package(emugen).

:- load_compilation_module(library(compiler/emugen/emugen_tr)).
:- add_sentence_trans(emugen_tr:emugen_sent/3, 750).

:- include(library(compiler/emugen/emugen_ops)).

