:- module(_,_,[classic,functional]).

testapp :- 
	set_prolog_flag(write_strings,on),
	X = " my ",
	write("Hello" ++ X ++ "world!").
