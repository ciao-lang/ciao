:- module(filebased_locate, [module_address/2], []).

:- use_module(library(read)).
:- use_module(library(system)).

:- data address_db/2.
:- data wd/1.

% look for address in address_db, else accesses .addr file and extends db 

module_address(M, Address) :-
	current_fact(address_db(M, Address)), !.
module_address(M, Address) :-
	current_fact(wd(CurrentDir)),
	atom_concat(CurrentDir, '.addr', Loc),
	(file_exists(Loc)->
	 open(Loc, read, S),
	 read(S, Location_Dir),
	 close(S)
	;
	 Location_Dir = CurrentDir),
        atom_concat(M, '.addr', M_addr),
	atom_concat(Location_Dir, M_addr, Mod_Addr), 
	open(Mod_Addr, read, ST),
	read(ST, Address), 
	close(ST),
	asserta_fact(address_db(M, Address)).

:- initialization(startup).
startup:-
	working_directory(CurrentDir, CurrentDir),
	atom_concat(CurrentDir, '/', Dir),
	asserta_fact(wd(Dir)).
