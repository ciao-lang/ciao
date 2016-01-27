:- module(byte_lists, [obtain_list/3, show_list/2], [foreign_interface]).
	 
:- true pred obtain_list(in(N),go(Length),go(List)) :: c_int * c_size * c_uint8_list
	+ (foreign,size_of(List,Length)).
:- true pred show_list(in(Length),in(List)) :: c_size * c_uint8_list
	+ (foreign,size_of(List,Length)).

:- use_foreign_source(bytes_op).
