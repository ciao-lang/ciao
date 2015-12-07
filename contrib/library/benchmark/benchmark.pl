:- package(benchmark).
% A package that define benchmark programs.
% (interface for benchmarks)
% 
% See: 
%
% Author: Jose F. Morales

% TODO: Add documentation
% TODO: Define as an interface

:- export(name/1).
:- export(repeat_count/1).
:- export(solve/1).

:- use_package(fsyntax).
:- if((defined(optim_comp), backend(js_backend))).
:- source_as_resource. % for testsuite_ui
:- use_package([oo_syntax, hiord, assertions, mutables, string_type]).
:- endif.

:- if((defined(optim_comp), backend(js_backend))).
:- else.
:- use_module(library(strings), [write_string/1]).
:- use_module(library(prolog_sys), [statistics/2]).
% Dummy code (just benchmark)
:- export(main/0).
main :-
	repeat_count(N),
	statistics(runtime, [X0|_]),
	solve_(N),
	statistics(runtime, [X|_]),
%	display(L),
%	nl,
	name(Name),
	display('name: '),
	write_string(Name),
	nl,
	display('count: '),
	display(N),
	nl,
	display('time: '),
	Y is X - X0,
	display(Y),
	nl.

solve_(N) :-
	( repeat(N),
	  solve(_L),
%	  display(_L), nl,
	  fail
	; true
	).

repeat(_N).
repeat(N) :- N > 1, N1 is N - 1, repeat(N1).
:- endif.
