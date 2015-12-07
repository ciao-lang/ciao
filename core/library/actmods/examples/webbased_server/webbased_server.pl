
:- module(webbased_server, [module_address/2,
	                    my_module_address/3],
			   [persdb]).

persistent_dir(actmods_db,'./webbased_db').

:- persistent(module_address_db/3,actmods_db).

module_address(M,Address):-
	current_fact(module_address_db(M,Address,_Pid)).

my_module_address(M,Address,Pid):-
	retractall(M),
	assertz_fact(module_address_db(M,Address,Pid)).

% retractall_fact/1 does not fit!
% if server dies and is restarted, it keeps the old module addresses,
% if no new server for a module is registered, every call will bakctrack
% over all the old addresses for that module until one that works is found ...
retractall(M):-
	retract_fact(module_address_db(M,_,_)),
	fail.
retractall(_).
