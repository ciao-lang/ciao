:- package(profiler).
:- use_package(library(profiler/profiler_decl)).

:- use_module(library(profiler/profiler_rt)).

% TODO: uncertain priority: just disables some decls and goals
:- load_compilation_module(library(profiler/profiler_tr)).
:- add_sentence_trans(profiler_tr:profiler_def/3, 8210).
:- add_goal_trans(profiler_tr:profiler_goal_trans/3, 8210).

:- new_declaration(cost_center/1).
:- new_declaration(cost_center/2).
:- new_declaration(no_cost_center/1).
:- new_declaration(all_cost_center/0).
:- new_declaration(all_cost_center/1).
:- new_declaration(all_no_cost_center/0).

:- op(1150, fx, [cost_center, no_cost_center]).

% This include will be expanded by this package -- EMM
:- include(this_module_cc_auto).
