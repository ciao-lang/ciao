:- module(_,_,[functional]).

:- use_module(library(stream_utils), [write_string/1]).

testapp :- 
	X = " my ",
	write_string("Hello" ++ X ++ "world!").
