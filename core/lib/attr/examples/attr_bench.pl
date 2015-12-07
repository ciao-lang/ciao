:- module(attr_bench, _).

:- use_module(library(debugger), [call_in_module/2]).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(compiler/c_itf_internal), [base_name/2]).
:- use_module(library(terms), [atom_concat/2]).


%%% Freeze %%%

test(1, t(nreverse_freeze, Size, Time)):-
	default_parameter(Size, 5000), 
	call_in_module_with_time(library(freeze/examples/nreverse_freeze), 
	                         test(Size, Time), _).

test(2, t(queens_freeze, Size, Time)):-
	default_parameter(Size, 12),
	call_in_module_with_time(library(freeze/examples/queens_freeze),
	                         test(Size, Time), _).

test(3, t(sendmoney_freeze, na, Time)):-
	call_in_module_with_time(library(freeze/examples/sendmoney_freeze),
	                         go_(_, Time), _).

test(4, t(sort_freeze, Size, Time)):-
	default_parameter(Size, 24),
	call_in_module_with_time(library(freeze/examples/sort_freeze), 
	                         test(Size, Time), _).

%%% CLPFD %%% 

test(5, t(queens_clpfd, Size, Time)):-
	default_parameter(Size, 105),
	call_in_module_with_time(library(clpfd/examples/queens), 
	                         main(Size, [ff], kernel), Time). 

test(6, t(bridge_clpfd, na, Time)):-
	call_in_module_with_time(library(clpfd/examples/bridge),
	                         bridge(_, _), Time).

%%% CHR %%%

test(7, t(regexp_chr, na, Time)):-
	call_in_module_with_time(library(chr/'Examples'/regexp), test(Time), _).

test(8, t(counter_chr, Size, Time)):-
	default_parameter(Size, 40000),
	call_in_module_with_time(library(chr/'Examples'/test), t(Size), Time).

%%% TOOLS %%%

call_in_module_with_time(File, Call, Time):-
	ensure_loaded(File), 
	base_name(File, BaseName), 
	file_name_to_module(BaseName, Module),
	statistics(usertime, [Begin, _]),
	call_in_module(Module, Call),
	statistics(usertime, [End, _]), 
	Time is End - Begin.

file_name_to_module(BaseName, Module):-
	atom_concat([_, '/', Module], BaseName), 
	\+ atom_concat([_, '/', _], Module),!.
file_name_to_module(Module, Module). 


default_parameter(X, Default):-
	var(X), !, X = Default.
default_parameter(_, _).