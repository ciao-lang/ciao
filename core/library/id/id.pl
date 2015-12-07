:- package(id).

:- use_package(hiord).

:- new_declaration(iterative/2, on).

:- load_compilation_module(library(id/id_tr)).
:- add_sentence_trans(id_tr:id_sentence/3, 750). % TODO: Right priority?
:- add_clause_trans(id_tr:id_clause/3, 750). % TODO: Right priority?
