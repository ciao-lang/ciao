:- module(clpr_rt, 
	[ nonzero/1,
 	  clpr_freeze/2,
 	  solve_generic_0/2,
 	  solve_generic_1/4,
 	  solve_generic_2/6,
 	  solve_generic_3/8,
 	  solve_generic_4/10,
 	  solve_generic_5/12,
 	  solve_generic_n/4 ], 
	[ assertions, 
	  library(clpr/clpr_src), 
	  library(clpqr/clpqr_options)
	]).


:- use_module(library(clpr/solver_r)).
:- use_module(library(clpr/clpr_dump), [clpqr_dump_constraints/3]).

:- use_module(library(clpr/clpr_attr)).

:- include(library(clpqr/clpqr_rt)).
:- include(library(clpqr/clpqr_attr_hooks)).
