:- use_module(library(tcltk)).

:- use_package(classic).


p(yes).
q(yes).
r(yes, no).

main :- go.

go :-
	tk_new([name('test')], X),
	tcl_eval(X, 'source test.tcl',_),
	tcl_eval(X, [foo], _),
	tcl_delete(X).
