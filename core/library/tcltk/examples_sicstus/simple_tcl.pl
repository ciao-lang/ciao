:- use_module(library(tcltk)).

go :-

	tcl_new(Interp), 
	tcl_eval(Interp, 'set x 1', _),
	tcl_eval(Interp, 'incr x', R).

go2 :-
	tcl_new(X), 
	tcl_eval(X, 'wilbert', R).


go3 :-
	tcl_new(T), 
	tcl_eval(T, 'prolog wilbert', R).
