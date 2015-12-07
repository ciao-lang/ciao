:- module(_, [cost_center_sentence_tr/3, cost_center_goal_tr/3], [assertions]).

:- use_module(library(aggregates)).
:- use_module(library(messages)).
% :- use_module(library(resdefs/resources_types)).
:- use_module(library(profiler/profiler_tr),    [cost_center/4]).
:- use_module(library(compiler/c_itf_internal), [location/1]).
:- use_module(library(assertions/assrt_lib), [normalize_assertion/9,
		assertion_body/7]).
:- use_module(library(resdefs/rescostfunc)).

:- doc(author, "Edison Mera").
:- doc(author, "Teresa Trigo").

:- doc(module, "This program expands the cost and rel_cost global
	properties to cost_center properties, in order to allow module
	expansions of node names.").

resc(counts).
resc(ticks).

cost_typec(sol(_)).
cost_typec(allsols).
cost_typec(call).
cost_typec(call_exit).
cost_typec(call_fail).
cost_typec(redo).
cost_typec(redo_exit).
cost_typec(redo_fail).

:- data check_point_db/2.

:- doc(bug, "But Note that in resource/1 we are not checking that
	Name0 and Name correponds to valid cost centers. --EMM").

cc_resource(cost_center(_/_, _/_, res(Res, Type))) :-
%	predname(Name0),
%	predname(Name),
	resc(Res),
	cost_typec(Type).
cc_resource(cost_center(_/_, res(Res, Type))) :-
%	predname(Name),
	resc(Res),
	cost_typec(Type).
cc_resource(cost_center(res(Res, Type))) :-
	resc(Res),
	cost_typec(Type).

% cost center declaration:
cost_center_sentence_tr((:- resource_cost_center), Clauses, _) :-
	!,
	findall((:- resource(CC)), cc_resource(CC), Clauses).
cost_center_sentence_tr((:- check_point(F/A)), (:- check_point(F/A)), M) :-
	!,
	asserta_fact(check_point_db(F/A, M)).
cost_center_sentence_tr((:- Assr0), (:- Assr), M) :-
	location(loc(S, LB, LE)),
	normalize_assertion(M, Assr0, _Pred, Status, Type, Body, S, LB, LE) ->
	transform_cc_assertion(M, Status, Type, Body, S, LB, LE, Assr).

transform_cc_assertion(M, Status, Type, Body0, S, LB, LE, Assr) :-
	assertion_body(PD, [], [], [], Cp, Cm, Body0),
	Cp = [rel_cost(_Goal, Ap, ResCC, CF)],
	!,
	(
	    check_point_db(F/A, M) ->
	    functor(PD, F0, A0),
	    expand_cf(CF, IDF, CFN),
	    Assr =.. [Type, Status, F/A+cost(rel, Ap, call,
		    cost_center(F0/A0, res(ResCC, call)), IDF, CFN) # Cm]
	;
	    warning_message(loc(S, LB, LE),
		"check_point needs to be declarared before.", []),
	    fail
	).

check_cost_center(F/N, M) :-
	( cost_center(F, N, _, M) -> true
	;
	    location(Loc),
	    error_message(Loc, "Undefined cost center ~w/~w", [F, N])
	).

% cost properties:

cost_center_goal_tr(cost(Goal, Rel, Ap, Type, cost_center(Name0, Name, Res),
		IF, CF),
	    cost_center(Goal, Rel, Ap, Type, Name0, Name, Res, IF, CF), M) :-
	check_cost_center(Name0, M),
	check_cost_center(Name,  M).
cost_center_goal_tr(cost(Goal, Rel, Ap, Type, cost_center(Name, Res), IF, CF),
	    cost_center(Goal, Rel, Ap, Type, Name, Res, IF, CF), M) :-
	check_cost_center(Name, M).
cost_center_goal_tr(cost(Goal, Rel, Ap, Type, cost_center(Res), IF, CF),
	    cost_center(Goal, Rel, Ap, Type, Res, IF, CF), _M).
cost_center_goal_tr(end_of_file, end_of_file, M) :-
	retractall_fact(check_point_db(_, M)).
