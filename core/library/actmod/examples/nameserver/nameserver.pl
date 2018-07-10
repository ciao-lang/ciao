:- module(_, [], [persdb, actmod]).

% A simple nameserver actmod for platformbased or webbased registry.

% :- actmod_reg_protocol(webserver).
:- actmod_reg_protocol(platformbased).
:- dist_node.

persistent_dir(actmod_db,'./nameserver_db').
:- persistent(module_address_db/4, actmod_db).

:- export(ask_address/3).
ask_address(ActRef,DMod,Address):-
	current_fact(module_address_db(ActRef,DMod,Address,_Pid)).

:- export(tell_address/4).
tell_address(ActRef,DMod,Address,Pid):-
	retractall_addresses(ActRef),
	assertz_fact(module_address_db(ActRef,DMod,Address,Pid)).

% retractall_fact/1 does not fit!
% if server dies and is restarted, it keeps the old module addresses,
% if no new server for a module is registered, every call will backtrack
% over all the old addresses for that module until one that works is found ...
retractall_addresses(ActRef):-
	retract_fact(module_address_db(ActRef,_,_,_)),
	fail.
retractall_addresses(_).
