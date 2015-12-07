:- module(clpq_rt, 
	[ nonzero/1,
% RH: why it is clpr_freeze and not clpq_freeze ?
          clpr_freeze/2,
	  solve_generic_0/2,
	  solve_generic_1/4,
	  solve_generic_2/6,
	  solve_generic_3/8,
	  solve_generic_4/10,
	  solve_generic_5/12,
	  solve_generic_n/4],
	[ assertions, 
	  library(clpq/clpq_src),
	  library(clpqr/clpqr_options)
	]).

:- use_module(library(clpq/solver_q)).
:- use_module(library(clpq/clpq_dump), [clpqr_dump_constraints/3]).  

:- use_module(library(clpq/clpq_attr)).

:- include(library(clpqr/clpqr_rt)).
:- include(library(clpqr/clpqr_attr_hooks)).
