:- module(module_test, _, []).

:- use_module(.(module3), [main3/1]).

:- use_module(library(write), [write/1]).
:- use_module(library(profiler/profiler_auto_conf)).
:- use_module(library(compiler/global_module_options)).
:- use_module(library(profiler/profiler_utils)).

t0 :-
	cc_auto_conf(ticks, [module1, module2, module3], main3(_A), 2, Goals,
	    Tree),
	write(Goals),
	nl,
	write(Tree),
	nl.

t1 :-
	glbmod_add_package(module2, profiler),
	glbmod_add_package(module3, profiler),
	profile_reset,
	(\+ profile(main3(_A)) -> true ; true),
	profile_dump.

