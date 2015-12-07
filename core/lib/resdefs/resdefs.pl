:- package(resdefs).

:- use_package(library(resdefs/resources_decl)).

:- use_module(library(resdefs/resources_props)).

:- load_compilation_module(library(resdefs/resdefs_tr)).
:- add_sentence_trans(resdefs_tr:resdefs_sentence_tr/3, 830).
:- add_goal_trans(resdefs_tr:resdefs_goal_tr/3, 830).

:- use_module(library(resdefs/resdefs_rt)).
:- use_module(library(lists), [length/2]).
:- use_module(library(terms), [term_size/2]).
