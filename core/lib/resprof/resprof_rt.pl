:- module(_, [init_resource_usage/2, inc_cost/2, check_cost/3, try_finally/2,
		apply_trust_literal/3, dump_profiler/0, restore_resources/1],
	    [assertions, dcg, hiord]).

:- use_package(library(resprof/resprof_decl)).
:- use_package(library(resdefs/resources_decl)).
:- use_module(library(resdefs/resources_props), [cost/8, calls/1]).
:- use_module(library(resdefs/resdefs_rt)).
:- use_module(library(resdefs/res_litinfo)).
:- use_module(library(rtchecks/rtchecks_rt), [checkc/2]).
:- use_module(library(aggregates)).
:- use_module(library(format)).
:- use_module(library(hiordlib)).
:- use_module(library(sort)).

resource_usage(Resource, Cost) :-
	resource_usage_db(Resource, ub, Cost).

'$def$ru'(Resource) :-
	resource_usage_db(Resource, ub, _).

dump_profiler :-
	format("Res\tApprox\tCost~n", []),
	resource_usage_db(Resource, Approx, Cost),
	format("~w\t~w\t~w\n", [Resource, Approx, Cost]),
	fail
    ;
	true.

init_resource_usage(Resource, Approx) :-
	resource_usage_db(Resource, Approx, _) -> true
    ;
	assertz_fact(resource_usage_db(Resource, Approx, 0)).

:- meta_predicate inc_cost(pred(3), ?).
inc_cost(CostCall, Head) :-
	retract_fact(resource_usage_db(Resource, Approx, Cost0)),
	inc_value(CostCall, Head, Approx, Resource, Cost0, Cost),
	assertz_fact(resource_usage_db(Resource, Approx, Cost)),
	fail
    ;
	true.

:- meta_predicate inc_value(pred(3), ?, ?, ?, ?, ?).
inc_value(CostCall, Head, Approx, Resource, Cost0, Cost) :-
	CostCall(Approx, Resource, Func) ->
	(
	    num(Func) -> Cost1 = Func
	;
	    get_litinfo(Head, Approx, [], LitInfo),
	    Func(LitInfo, Cost1)
	),
	Cost is Cost0 + Cost1
    ;
	Cost = Cost0.

:- meta_predicate cost_pattern(goal).
cost_pattern(cost(_, _, _, _, _, _, _, _)).

:- meta_predicate apply_trust_literal(goal, ?, ?).
apply_trust_literal(Disj, Cost, Updates) :-
	cost_pattern(Cost),
	findall(Cost, Disj, Costs0),
	sort(Costs0, Costs),
	map(Costs, update_resource, Updates, []),
	% warning_required_trusts,
	!.

:- meta_predicate try_finally(goal, goal).

try_finally(Goal, Finally) :-
	catch(if(Goal, Finally, Finally), Ex, (Finally, throw(Ex))).

restore_resources(Updates) :-
	list(Updates, restore_resource).

:- meta_predicate check_cost(list(goal), list(goal), ?).
check_cost(CheckProp, Comp, Cost) :-
	checkc(CheckProp, true),
	member(Cost, Comp).

restore_resource(cost(Approx, Resource, Value)) :-
	assertz_fact(resource_usage_db(Resource, Approx, Value)).

:- meta_predicate update_resource(goal, ?, ?).
update_resource(cost(_Goal, abs, Ap, _, Res, IF, _, CF)) -->
	!,
	(
	    {retract_fact(resource_usage_db(Res, Ap, Value1))} ->
	    {calls(IF)},
	    {eval_cf(CF, Value0)},
	    {Value is Value0 + Value1},
	    [cost(Ap, Res, Value)]
	;
	    []
	).
update_resource(_) --> [].

/*
warning_required_trusts :-
	resource_usage_db(Resource, Approx, _),
	warning(['No trust for resource ', Resource, ' Approx ', Approx]),
	fail
    ;
	true.
*/
