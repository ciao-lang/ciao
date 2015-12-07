:- module(strings_and_atoms,
	[ lookup_string/2,
	  lookup_atom/2,
	  a_string/1,
	  show_string/1,
	  show_atom/1
	],
	[foreign_interface]).

:- true pred a_string(go(S)) ::
	string + (foreign(get_static_str),returns(S),do_not_free(S)).
	 
:- true pred lookup_string(in(N),go(S)) ::
	int * string + (foreign(get_str),returns(S)).
:- true pred lookup_atom(in(N),go(S)) ::
	int * atm + (foreign(get_str),returns(S)).

:- true pred show_string(in(S)) :: string + foreign(put_str).
:- true pred show_atom(in(S)) :: atm + foreign(put_str).

:- use_foreign_source(str_op).
