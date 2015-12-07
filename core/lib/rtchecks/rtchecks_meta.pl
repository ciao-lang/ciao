:- module(rtchecks_meta, [
		body_check_pos/9,
		body_check_pre/9,
		collapse_redundants/3,
		collect_checks/3,
		compound_rtchecks/6,
		compound_rtchecks_end/5
	    ], [assertions, dcg, hiord]).

:- use_module(library(lists)).
:- use_module(library(sort)).
:- use_module(library(hiordlib), [map/3]).
:- use_module(library(rtchecks/rtchecks_basic)).

:- doc(author, "Edison Mera").

:- doc(module, "Meta predicates used in rtcheck expansion.").

:- meta_predicate collapse_redundants2(?, ?, pred(4), ?).
collapse_redundants2([], T, Goal, Es0) :-
	Goal(_, T, Es0, []).
collapse_redundants2([T0|Ts], T, Goal, Es0) :-
	Goal(T0, T, Es0, Es),
	collapse_redundants2(Ts, T0, Goal, Es).

:- meta_predicate collapse_redundants(?, pred(4), ?).
collapse_redundants([],     _,    []).
collapse_redundants([T|Ts], Goal, Es) :-
	collapse_redundants2(Ts, T, Goal, Es).

:- meta_predicate compound_rtchecks2(pred(2), pred(4), ?, ?, ?, ?).
compound_rtchecks2(CheckToProps, Collapser, CheckProps, CheckedPropsCUI0,
	    PropsCUI, PropsL) :-
	map(CheckProps, CheckToProps, PropsCUI0),
	sort(PropsCUI0, PropsCUI1),
	diff_props(PropsCUI1, CheckedPropsCUI0, PropsCUI),
	collapse_redundants(PropsCUI, Collapser, PropsL).

:- meta_predicate compound_rtchecks(pred(2), pred(4), ?, ?, ?, ?).
compound_rtchecks(CheckToProps, Collapser, CheckProps, CheckedPropsCUI0,
	    CheckedPropsCUI, PropsL) :-
	compound_rtchecks2(CheckToProps, Collapser, CheckProps,
	    CheckedPropsCUI0, PropsCUI0, PropsL),
	sort(PropsCUI0, PropsCUI),
	append(PropsCUI, CheckedPropsCUI0, CheckedPropsCUI).

:- meta_predicate compound_rtchecks_end(pred(2), pred(4), ?, ?, ?).
compound_rtchecks_end(CheckToProps, Collapser, CheckProps, CheckedPropsL,
	    PropsL) :-
	compound_rtchecks2(CheckToProps, Collapser, CheckProps,
	    CheckedPropsL, _, PropsL).

:- meta_predicate body_check_pos(?, pred(2), pred(2), pred(4), ?, ?, ?, ?, ?).
body_check_pos([], _, _, _, _, CheckedL, CheckedL, Body, Body) :- !.
body_check_pos(CheckPos, CheckToProps, CheckToPropsPos, Collapser, Params,
	    CheckedL0, CheckedL, Body0, Body) :-
	compound_rtchecks(CheckToProps, Collapser, CheckPos, CheckedL0,
	    CheckedL, Pre),
	compound_rtchecks_end(CheckToPropsPos, Collapser, CheckPos, [], PosL),
	map(PosL, checkif_to_lit(Params), Pos),
	Body0 = [Pre, Body, Pos].

:- meta_predicate body_check_pre(?, pred(2), pred(2), pred(2), pred(4), ?, ?,
	    ?, ?).
body_check_pre([], _, _, _, _, Checked, Checked, Body, Body) :- !.
body_check_pre(ChkProp, CheckToProps, CheckToFails, CheckToError, Collapser,
	    CheckedL0, CheckedL, Body0, Body) :-
	compound_rtchecks(CheckToProps, Collapser, ChkProp, CheckedL0,
	    CheckedL, Prop),
	compound_rtchecks_end(CheckToFails, Collapser, ChkProp, [],
	    PropFails0),
	lists_to_lits(PropFails0, PropFails),
	(
	    PropFails == true ->
	    Body0 = Body
	;
	    compound_rtchecks_end(CheckToError, Collapser, ChkProp, [],
		PropError0),
	    lists_to_lits(PropError0, PropError),
	    Body0 = [
		Prop,
		(
		    PropFails ->
		    PropError
		;
		    true
		),
		Body]
	).

:- meta_predicate collect_checks(?, pred(2), ?).
collect_checks([],                     _,         []).
collect_checks([Assertion|Assertions], Collector, Checks0) :-
	(
	    Collector(Assertion, Check) ->
	    Checks0 = [Check|Checks]
	;
	    Checks0 = Checks
	),
	collect_checks(Assertions, Collector, Checks).
