:- package(debugpred).
% A package to define special debug predicates what can be enabled
% (executed) or disabled (replaced by 'true') based on a single
% directive.

:- load_compilation_module(library(debugpred/debugpred_tr)).
:- add_sentence_trans(debugpred_tr:debugpred_sentence_tr/3, 9010).
:- add_goal_trans(debugpred_tr:debugpred_goal_tr/3, 9010).

% TODO: Import only the necessary predicates
:- use_module(library(lists)).

:- op(1150, fx, [debugpred]).

:- debugpredstatus(on).
