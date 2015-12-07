:- module(clpq_compiler, 
	[compile_constr/4],
	[dcg,library(clpq/clpq_src)]).

:- use_module(library(clpq/solver_q)).
:- use_module(library(clpq/clpq_dump), [dump_internal/3]).

:- include(library(clpqr/clpqr_compiler)).
