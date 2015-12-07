:- package(rtchecks_rt_inline).

:- use_package(inliner).
:- use_package(isomodes).

:- use_module(library(terms_vars), [varset/2]).
:- use_module(library(freeze),     [freeze/2]).
:- use_module(engine(internals),   ['$setarg'/4]).
:- use_module(library(terms_check)).

:- unfold checkif(yes, no, no, no, yes, yes, yes, yes, yes).
:- unfold rtcheck(no, no, yes, no, yes, yes, yes, yes).
:- unfold checkif_comp(yes, yes, yes, yes).
:- unfold add_info_rtsignal(yes, yes, yes, yes).
:- unfold rtcheck(yes, yes, yes).
:- unfold attach_cut_fail(yes, yes).
:- unfold map(no, yes, no).

:- use_module(engine(internals), ['$setarg'/4]).
:- use_module(library(terms_check)).
:- use_module(library(terms_vars)).
:- use_module(library(freeze)).

:- use_module(library(hiordlib)).
:- use_module(library(lists)).
:- use_module(library(sort)).
:- use_module(library(rtchecks/rtchecks_basic)).

% :- nortchecks(intercept/3).
% :- unfold intercept(no, no, no).
% :- unfold asserta_catching(yes, yes, yes).
% :- unfold retract_catching(yes, yes, yes).

% :- meta_predicate intercept(goal, ?, goal).
% :- use_module(engine(exceptions_db)).
% :- inline_module(engine(exceptions), [intercept/3]).

%:- use_module(library(hiordlib), [map/3]).
:- use_module(library(varnames/apply_dict)).
:- use_module(library(varnames/complete_dict)).
:- use_package(dcg).
:- use_package(fsyntax).
:- use_module(library(rtchecks/rtchecks_rt),
	    [attach_cut_fail/2, select_defined/3]).
:- inline_module(library(rtchecks/rtchecks_rt), [
		condition/1,
		checkc/6,
		checkc/4,
		checkif/9,
		checkif_comp/4,
		rtcheck/8,
		disj_prop/5,
		disj_prop/3,
		add_info_rtsignal/4,
		call_stack/2,
		non_inst/2,
		non_compat/2,
		rtcheck/3,
		rtcheck_/3,
		send_rtcheck/5,
		pretty_prop/2
	    ]).
:- inline_module(library(assertions/native_props), [
		mshare/1,
		not_fails/1,
		fails/1,
		is_det/1,
		non_det/1,
		sideff_pure/1,
		sideff_soft/1,
		sideff_hard/1,
		finite_solutions/1,
		terminates/1,
		possibly_fails/1,
		possibly_nondet/1,
		mut_exclusive/1,
		not_mut_exclusive/1,
		steps_lb/2,
		steps_ub/2,
		steps_o/2,
		steps/2
	    ]).
:- inline_module(library(resdefs/resources_props),
	    [
		cost/7,
		head_cost/4,
		literal_cost/4,
		compare_ap/3,
		cost_t/8
	    ]).
:- inline_module(library(resdefs/rescostfunc), [expand_cf/3]).
:- inline_module(engine(basic_props), [
		iso/1,
		not_further_inst/2,
		sideff/2,
		regtype/1,
		native/1,
		eval/1,
		equiv/2,
		bind_ins/1,
		error_free/1,
		memo/1,
		pe_type/1
	    ]).
