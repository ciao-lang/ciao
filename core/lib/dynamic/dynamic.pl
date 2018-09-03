:- package(dynamic).
:- use_package(datafacts). % (for convenience)
:- use_module(library(dynamic/dynamic_rt)).

% TODO: just the operator, set a compiler flag (i.e. declare predicate kind?)
:- op(1150, fx, [(dynamic)]).

