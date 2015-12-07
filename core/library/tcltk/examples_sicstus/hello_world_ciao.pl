:- use_module(library(tcltk)).

go :-
	tk_new([name('Example 1')], Interp),
	tcl_eval(Interp, 'button .fred -text "hello world" -command { display "hello world"}', _),
	tcl_eval(Interp, 'pack .fred', _),
	tk_main_loop.
