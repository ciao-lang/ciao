:- module(clpr_dump, 
	[dump_internal/3, clpqr_dump_constraints/3], 
	[dcg, library(clpr/clpr_src)]).

:- include(library(clpqr/clpqr_ops)).

:- use_module(library(clpr/clpr_attr)).
:- use_module(library(clpr/solver_r)).

:- include(library(clpqr/clpqr_dump)).
:- include(library(clpqr/fourier_motzkin)).
