:- package(traits).

:- load_compilation_module(library(traits/traits_tr)).
:- add_sentence_trans(traits_tr:traits_sent/3, 8110).
:- add_goal_trans(traits_tr:traits_goal/3, 8110).

% ---------------------------------------------------------------------------

:- include(library(traits/traits_ops)).

