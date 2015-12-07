:- module(clpq_dump, 
	[clpqr_dump_constraints/3, dump_internal/3], 
	[dcg, library(clpq/clpq_src)]).

:- include(library(clpqr/clpqr_ops)).

:- use_module(library(clpq/clpq_attr)).
:- use_module(library(clpq/solver_q)).

:- include(library(clpqr/clpqr_dump)).
:- include(library(clpqr/fourier_motzkin)).
