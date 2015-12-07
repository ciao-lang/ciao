:- package(clpr).
:- include(library(clpqr/clpqr_ops)).
:- use_module(library(clpr/clpr_rt)).
:- use_module(library(clpr/clpr_attr), [attach_attribute/2]).
:- use_module(library(clpr/solver_r),
	    [solve_sign/2, solve_abs/2, solve_mix/4, solve_mult/3, solve_pow/3,
		solve_trig/3, inf/2, sup/2, minimize/1, maximize/1]).

%% :- use_module(library(clpr/clpr_dump)).
:- multifile dump_constraints/3.

:- use_module(library(clpr/clpr_meta)).

:- load_compilation_module(library(clpr/clpr_tr)).
% :- add_term_trans(clpr_tr:translate_hash/2).
:- add_goal_trans(clpr_tr:translate_clpqr/2, 750). % TODO: Right priority?
