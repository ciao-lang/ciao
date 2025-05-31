:- module(_,_,[]).

:- meta_predicate('\6\block_goal'(primitive(goal))).
:- meta_predicate('\6\block_expr'(primitive(goal),?)).
:- meta_predicate('\6\stpa_def'(?,?,?,primitive(goal),?)).
:- meta_predicate('\6\loop'(?,primitive(goal),primitive(goal),primitive(goal),primitive(goal))).
:- impl_defined(['\6\letvar'/2,'\6\assign'/2,'\6\block_goal'/1,'\6\block_expr'/2,'\6\stpa_def'/5,'\6\loop'/5,'\6\return'/1]).

% TODO: experimental for pure loops
:- meta_predicate('\6\posneg'(primitive(goal),primitive(goal))).
:- impl_defined(['\6\posneg'/2]).
