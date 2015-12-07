:- module(_, _, [assertions]).

:- use_package(predefres(res_exectime)).
:- use_module(library(prolog_sys), [statistics/2]).

resource_usage(exectime, T) :- statistics(walltime, [T|_]).
