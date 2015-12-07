:- package(expander).

% TODO: probably the priority should be a parameter of this extension
:- load_compilation_module(library(expander/expander_tr)).
:- add_sentence_trans(expander_tr:expand_sentence/4, 9910).
:- add_clause_trans(expander_tr:expand_clause/4, 9910).
