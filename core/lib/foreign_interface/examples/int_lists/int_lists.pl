:- module(int_lists, [obtain_list/3, show_list/2], [foreign_interface]).
	 
:- true pred obtain_list(in(N),go(Length),go(List)) :: int * int * int_list
	+ (foreign,size_of(List,Length)).
:- true pred show_list(in(Length),in(List)) :: int * int_list
	+ (foreign,size_of(List,Length)).

:- use_foreign_source(ints_op).
