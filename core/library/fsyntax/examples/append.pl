:- module(_,_,[functional]).

:- use_module(engine(io_aux), [display_string/1]).

testapp :- 
	X = " my ",
	display_string("Hello" ++ X ++ "world!").
