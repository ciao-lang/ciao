:- module(ex2_check, [foo/1], [assertions, nativeprops, regtypes]).

% An example for compile-time check assertions
% (should be correct)

:- use_package(library(assertions/pp)).
:- pp_opt(auto_check_assert).

:- pred foo(X) => atm(X).

foo(X) :-
    X = bar.
