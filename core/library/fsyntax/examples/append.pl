:- module(_,_,[functional]).

:- use_module(library(write), [write/1]).

testapp :- 
	set_prolog_flag(write_strings,on),
	X = " my ",
	write("Hello" ++ X ++ "world!").
