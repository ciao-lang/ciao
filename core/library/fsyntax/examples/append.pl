:- module(_,_,[functional]).

:- use_module(engine(messages_basic), [display_string/1]).

testapp :- 
	X = " my ",
	display_string("Hello" ++ X ++ "world!").
