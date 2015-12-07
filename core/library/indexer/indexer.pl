:- package(indexer).

:- op(1150, fx, index).
:- new_declaration(index/1,off).

:- load_compilation_module(library(indexer/indexer_tr)).
:- add_sentence_trans(indexer_tr:expand_index_sent/3, 750). % TODO: Right priority?
:- add_goal_trans(indexer_tr:expand_index_goal/3, 750). % TODO: Right priority?

% Optionally included by the translation
% :- use_module(library(indexer/hash)).
