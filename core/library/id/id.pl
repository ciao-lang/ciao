:- package(id).

:- load_compilation_module(library(id/id_tr)).
:- include(library(id/id_rt)).
:- add_sentence_trans(id_tr:idclause/3, 750). % TODO: Right priority?
:- add_clause_trans(id_tr:trbody/3, 750). % TODO: Right priority?
