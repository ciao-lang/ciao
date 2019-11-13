:- module(ex1_check, [foo/1], [assertions, nativeprops, regtypes]).

% An example for compile-time check assertions
% (gives errors)

:- use_package(library(assertions/pp)).
:- pp_opt(auto_check_assert).

:- pred foo(X) => number(X).

foo(X) :-
    X = bar.
