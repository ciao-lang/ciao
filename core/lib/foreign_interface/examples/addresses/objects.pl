:- module(objects, [object/2, show_object/1], [foreign_interface]).

:- trust pred object(in(N),go(Object)) ::
	c_int * address + (foreign,returns(Object)).

:- trust pred show_object(in(Object)) ::
	address + foreign.

:- use_foreign_source(objects_c).
:- extra_compiler_opts(['-O2']).
