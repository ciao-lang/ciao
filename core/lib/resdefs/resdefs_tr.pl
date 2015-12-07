:- module(resdefs_tr, [resdefs_sentence_tr/3, resdefs_goal_tr/3],
	    [assertions]).

:- doc(author, "Edison Mera").
:- doc(author, "Teresa Trigo").

:- use_module(library(aggregates)).
:- use_module(library(messages)).
:- use_module(library(compiler/c_itf_internal), [location/1]).
:- use_module(library(resdefs/rescostfunc)).

:- data resource_db/2, def_ru_db/2, def_gru_db/2.
:- export(resource_db/2).

assert_resource(R, M) :- assertz_fact(resource_db(R, M)).
cleanup_resource_db(M) :- retractall_fact(resource_db(_, M)).

resdefs_sentence_tr(0, 0, M) :-
	retractall_fact(def_ru_db(_,  M)),
	retractall_fact(def_gru_db(_, M)),
	cleanup_resource_db(M).
resdefs_sentence_tr((:- resource(Resource)), (:- resource(Resource)), M) :-
	assert_resource(Resource, M).
resdefs_sentence_tr(
	    (resource_usage(Res, Value) :- Body),
	    (resource_usage(Res, Value) :- Body), M) :-
	check_resource(Res, M),
	assertz_fact(def_ru_db(Res, M)).
resdefs_sentence_tr(
	    (global_resource_usage(Res, Value) :- Body),
	    (global_resource_usage(Res, Value) :- Body), M) :-
	check_resource(Res, M),
	assertz_fact(def_gru_db(Res, M)).
resdefs_sentence_tr(end_of_file, RUL, M) :-
	findall('$def$ru'(Res), retract_fact(def_ru_db(Res, M)), RUL, GRUL),
	findall('$def$gru'(Res), retract_fact(def_gru_db(Res, M)), GRUL,
	    [end_of_file]).

check_resource(Res, M) :-
	\+ \+ resource_db(Res, M) ->
	true
    ;
	location(Loc),
	error_message(Loc, "Resource ~w not defined.", [Res]).

expand_cost(Goal, Rel, Ap, Type, Res, CostFunction,
	    cost(Goal, Rel, Ap, Type, Res, InputDataFuncs, CostFunctionN),
	    M) :-
	check_resource(Res, M),
	expand_cf(CostFunction, InputDataFuncs, CostFunctionN).

resdefs_goal_tr(end_of_file, _, M) :-
	!,
	cleanup_resource_db(M).
%
resdefs_goal_tr(cost(Goal, Ap, Res, CostFunction), Cost, M) :-
	expand_cost(Goal, abs, Ap, call, Res, CostFunction, Cost, M).
resdefs_goal_tr(cost(Goal, Ap, Type, Res, CostFunction), Cost, M) :-
	expand_cost(Goal, abs, Ap, Type, Res, CostFunction, Cost, M).
%
resdefs_goal_tr(rel_cost(Goal, Ap, Res, CostFunction), Cost, M) :-
	expand_cost(Goal, rel, Ap, call, Res, CostFunction, Cost, M).
resdefs_goal_tr(rel_cost(Goal, Ap, Type, Res, CostFunction), Cost, M) :-
	expand_cost(Goal, rel, Ap, Type, Res, CostFunction, Cost, M).
%
resdefs_goal_tr(intervals(Size, Intervals),
	    (intervals(Goal, V, Intervals)), _) :-
	compound_size(Size, V, Goal).
