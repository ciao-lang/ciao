:- module(clpr_compiler, [compile_constr/4],[
        dcg,
        library(clpr/clpr_src)]).

:- use_module(library(clpr/solver_r)).
:- use_module(library(clpr/clpr_dump), [dump_internal/3]).

:- include(library(clpqr/clpqr_compiler)).
