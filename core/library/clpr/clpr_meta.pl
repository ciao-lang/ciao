:- module(clpr_meta, [clpr_meta/1,clpr_entailed/1]).

% RH: deactiavte unittests
%:- use_package(assertions)
%:- use_package(nativeprops).
%:- use_packgae(unittestdecls).

:- use_module(library(clpr/solver_r), 
        [normalize/4, 
	 var_with_def/5, 
	 solve_lin/2, 
	 solve_ineq_lt/2,
	 solve_ineq_le/2]).

clpr_meta(X):- 
	clpqr_meta(X).
clpr_entailed(X):- 
	clpqr_entailed(X).


:- include(library(clpqr/clpqr_meta)).

%:- load_test_package(clpr).
%:- test clpr_meta(Constraint) : (Constraint = (A .>=. B, A .<.B)) + fails.
