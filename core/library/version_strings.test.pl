:- module(_, [], [assertions]).

:- doc(title, "Tests for version_string.pl").

:- use_module(library(version_strings)).

:- export(test_norm/0). % export needed for test
:- test test_norm # "Comparison of normal versions".
test_norm :- strict_sorted_vers(['1.0.0', '2.0.0', '2.1.0', '2.1.1']).

:- export(test_pre/0).
:- test test_pre # "Comparison of prerelease and normal versions (simple)".
test_pre :- strict_sorted_vers(['1.0.0-alpha', '1.0.0']).

:- export(test_presub/0).
:- test test_presub # "Comparison of prerelease versions (with subcomponents)".
test_presub :- strict_sorted_vers([
    '1.0.0-alpha', '1.0.0-alpha.1', '1.0.0-alpha.beta',
    '1.0.0-beta', '1.0.0-beta.2', '1.0.0-beta.11',
    '1.0.0-rc.1', '1.0.0']).

% TODO: only for tests! disable when not testing
strict_sorted_vers([]).
strict_sorted_vers([_]) :- !.
strict_sorted_vers([A,B|Xs]) :- version_compare(<, A, B), strict_sorted_vers([B|Xs]).

