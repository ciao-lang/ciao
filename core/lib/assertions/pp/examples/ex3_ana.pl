:- module(ex3_ana, [foo/1], [assertions, nativeprops, regtypes]).

% Analyze, write output, and add rtchecks
:- use_package(library(assertions/pp)).
:- pp_opt(output).
:- pp_opt(rtchecks).
:- pp_cmd(analyze(shfr)).

:- pred foo(X) => number(X).

foo(X) :-
    X = bar.
