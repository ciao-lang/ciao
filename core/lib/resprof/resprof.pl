:- package(resprof).

:- use_package(library(resprof/resprof_decl)).
:- use_package(library(resdefs/resources_decl)).
:- use_module(library(resprof/resprof_rt)).

% note: priority before rescostcenter, resources, and resdefs
:- load_compilation_module(library(resprof/resprof_tr)).
:- add_sentence_trans(resprof_tr:resprof_sentence_tr/3, 805).
% :- add_goal_trans(resprof_tr:resprof_goal_tr/3).

:- initialization((init_resource_usage, fail;true)).

