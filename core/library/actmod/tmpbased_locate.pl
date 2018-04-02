:- module(tmpbased_locate, ['actmod.address'/2], []).

:- use_module(library(actmod/tmpbased_common)).
:- use_module(library(read)).

:- data address_db/2.

/* look address in db, else access file and extend db */
'actmod.address'(Module, Address) :-
	current_fact(address_db(Module,Address)), !.
'actmod.address'(Module, Address) :-
	module_to_addressfile(Module,AddressFile),
	open(AddressFile, read, ST),
	read(ST, Address), 
	close(ST),
	asserta_fact(address_db(Module,Address)).
