:- package(rfuzzy).

:- use_module(library(rfuzzy/rfuzzy_rt)). 
:- reexport(library(rfuzzy/rfuzzy_rt)).

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(terms),[copy_args/3]).
:- include(library(rfuzzy/rfuzzy_ops)).

:- load_compilation_module(library(rfuzzy/rfuzzy_tr)).
:- add_sentence_trans(rfuzzy_tr:rfuzzy_trans_sentence/3, 730). % TODO: Right priority?
% :- add_clause_trans(rfuzzy_tr:rfuzzy_trans_clause/3, 730). % TODO: Right priority?

% :- new_declaration(is_fuzzy/3,on).

