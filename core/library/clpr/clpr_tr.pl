:- module(clpr_tr, [translate_clpqr/2
          % , translate_hash/2
             ],[]).

:- include(library(clpqr/clpqr_ops)).

:- use_module(library(clpr/clpr_compiler), [compile_constr/4]).
:- use_module(library(clpr/clpr_rt), []). % To define attribute hooks

:- include(library(clpqr/clpqr_tr)).
