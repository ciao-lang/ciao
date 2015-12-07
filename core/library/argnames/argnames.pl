:- package(argnames).

:- load_compilation_module(library(argnames/argnames_trans)).
:- add_sentence_trans(argnames_trans:argnames_def/3, 630).
:- add_term_trans(argnames_trans:argnames_use/3, 630).
:- add_goal_trans(argnames_trans:argnames_goal/3, 630).

:- op(150, xfx, [$]).
:- op(950, xfx, (=>)).
:- op(1150, fx, [argnames]).

