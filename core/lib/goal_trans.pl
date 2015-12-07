:- module(goal_trans, [add_goal_trans/3, del_goal_trans/1], []).

:- use_module(engine(internals), [goal_trans/3, term_to_meta/2]).

:- include(library(compiler/add_goal_trans)).
