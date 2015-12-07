:- package(inliner).

% TODO: uncertain priority: duplicates the compiler logic, not really compatible with other extensions
%       Priorities ensure this order: profiler < rtchecks < inliner
:- load_compilation_module(library(inliner/inliner_tr)).
:- add_sentence_trans(inliner_tr:inliner_sentence_tr/3, 8410).
:- add_goal_trans(inliner_tr:inliner_goal_tr/3, 8410).

:- use_package(library(inliner/inliner_ops)).
