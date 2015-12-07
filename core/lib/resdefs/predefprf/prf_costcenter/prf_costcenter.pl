:- package(prf_costcenter).

:- use_module(library(profiler/profiler_rt),           []).
:- use_module(library(profiler/profiler_utils_native), []).

:- new_declaration(check_point/1).
:- op(1150, fx, [check_point]).

:- load_compilation_module(prf_costcenter(prf_costcenter_tr)).
:- add_sentence_trans(prf_costcenter_tr:cost_center_sentence_tr/3, 810).
:- add_goal_trans(prf_costcenter_tr:cost_center_goal_tr/3, 810).

:- use_package(resdefs).
:- use_module(prf_costcenter(prf_costcenter_rt)).

% This is expanded to define all the cost center based resources.
:- resource_cost_center.
