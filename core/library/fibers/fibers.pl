:- package(fibers).

:- include(library(fibers/fibers_hooks)).

:- use_module(library(fibers/fibers_rt)).

:- load_compilation_module(library(fibers/fibers_tr)).
:- add_sentence_trans(fibers_tr:sentence_tr/3, 2100).
:- add_goal_trans(fibers_tr:goal_tr/3, 500). % TODO:T253 before fsyntax; same for sentence_trans?

% Compile-time metadata (extended module interface)
% (second pass translation)
:- new_declaration('$export_functor'/2, on). % TODO:T253 move to fnct_hooks?

% 'fnct' static_trait
:- static_trait_spec(fnct, prop, 1).

