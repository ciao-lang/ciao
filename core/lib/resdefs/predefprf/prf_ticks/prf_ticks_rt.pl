:- module(_, _, [assertions]).

:- use_package(predefres(res_ticks)).
:- use_module(library(hrtime), [hrtime/1]).

resource_usage(ticks, T) :- hrtime(T).
