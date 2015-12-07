:- module(tmpbased_locate, [module_address/2], []).

:- use_module(library(actmods/tmpbased_common)).
:- use_module(library(read)).

:- data address_db/2.

/* look address in db, else access file and extend db */
module_address(Module, Address) :-
	current_fact(address_db(Module,Address)), !.
module_address(Module, Address) :-
	module_to_addressfile(Module,AddressFile),
	open(AddressFile, read, ST),
	read(ST, Address), 
	close(ST),
	asserta_fact(address_db(Module,Address)).
