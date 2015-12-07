:- module(clpq_meta,
	[clpq_meta/1,
	 clpq_entailed/1]).

:- use_module(library(clpq/solver_q), 
        [normalize/4, 
	 var_with_def/5, 
	 solve_lin/2, 
	 solve_ineq_lt/2, 
	 solve_ineq_le/2]).

clpq_meta(X):- 
	clpqr_meta(X).
clpq_entailed(X):- 
	clpqr_entailed(X).

:- include(library(clpqr/clpqr_meta)).
