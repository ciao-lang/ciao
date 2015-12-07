:- module(objects, [object/2, show_object/1], [foreign_interface]).

:- true pred object(in(N),go(Object)) ::
	int * address + (foreign,returns(Object)).

:- true pred show_object(in(Object)) ::
	address + foreign.

:- use_foreign_source(objects_c).
:- extra_compiler_opts('-O2').
