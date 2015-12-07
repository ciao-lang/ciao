:- package(regrtestdecls).

:- new_declaration(regr_texec/1).
:- op(1160,  fx, [regr_texec]).

:- load_compilation_module(library(regrtestdecls/regrtestdecls_tr)).
:- add_sentence_trans(regrtestdecls_tr:sentence_tr/4,620).
