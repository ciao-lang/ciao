:- package(block).

:- use_module(library(block/block_rt)).
:- use_module(library(freeze)).

:- op(1150, fx, block).

:- load_compilation_module(library(block/block_tr)).
:- add_sentence_trans(block_tr:sentence_tr/3, 750).  
:- add_clause_trans(block_tr:clause_tr/3, 750). 
