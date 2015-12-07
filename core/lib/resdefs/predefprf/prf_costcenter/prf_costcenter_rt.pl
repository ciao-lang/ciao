:- module(prf_costcenter_rt, [cost_center/7, cost_center/8, cost_center/9],
	    [assertions, nativeprops]).

:- use_package(predefprf(prf_costcenter)).

:- use_module(library(resdefs/resources_props)).
:- use_module(library(profiler/profiler_rt)).
:- use_module(library(profiler/profiler_utils_native)).

resource_usage(cost_center(Name0, Name, Res), Value) :-
	get_profile_active(1),
	cost_center_edge_value(Name0, Name, Res, Value).
resource_usage(cost_center(Name, Res), Value) :-
	get_profile_active(1),
	cost_center_node_value(Name, Res, Value).
resource_usage(cost_center(Res), Value) :-
	get_profile_active(1),
	cost_center_global_value(Res, Value).

global_resource_usage(cost_center(_, _, Res), Value) :-
	get_profile_active(1),
	cost_center_global_value(Res, Value).
global_resource_usage(cost_center(_, Res), Value) :-
	get_profile_active(1),
	cost_center_global_value(Res, Value).
global_resource_usage(cost_center(Res), Value) :-
	get_profile_active(1),
	cost_center_global_value(Res, Value).

:- doc(bug, "We have to do this transformation because Ciao does
	not support meta-term transformations. --EMM").

:- meta_predicate cost_center(goal, ?, ?, ?, spec, spec, ?,
	    addterm(list(goal)), ?).
cost_center(Goal, Rel, Ap, Type, '$:'(Name0), '$:'(Name), Res, IF, IFT, CE) :-
	cost(Goal, Rel, Ap, Type, cost_center(Name0, Name, Res), IF, IFT, CE).

:- meta_predicate cost_center(goal, ?, ?, ?, spec, ?, addterm(list(goal)), ?).

cost_center(Goal, Rel, Ap, Type, '$:'(Name), Res, IF, IFT, CE) :-
	cost(Goal, Rel, Ap, Type, cost_center(Name, Res), IF, IFT, CE).

:- meta_predicate cost_center(goal, ?, ?, ?, ?, addterm(list(goal)), ?).

cost_center(Goal, Rel, Ap, Type, Res, IF, IFT, CE) :-
	cost(Goal, Rel, Ap, Type, cost_center(Res), IF, IFT, CE).
