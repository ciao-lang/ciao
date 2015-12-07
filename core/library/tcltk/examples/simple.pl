:- module(simple, [main/0]).

:- use_module(library(tcltk)).
:- use_package(classic).
     
hello('Hello, world!').

main :- go.
     
go :-
	tk_new([name('Simple')], Tcl),
	tcl_eval(Tcl, 'source simple.tcl', _),
	tk_main_loop(Tcl),
	tcl_delete(Tcl).
