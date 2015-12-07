:- module(clpq_tr, [translate_clpqr/2
          % , translate_hash/2
             ],[]).

:- include(library(clpqr/clpqr_ops)).

:- use_module(library(clpq/clpq_compiler), [compile_constr/4]).
:- use_module(library(clpq/clpq_rt), []). % To define attribute hooks

:- include(library(clpqr/clpqr_tr)).
