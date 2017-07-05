:- module(platformbased_publish, [], []).

:- use_module(library(actmods/actmod_rt), ['actmod.call'/2]).
:- use_module(library(pathnames)).
:- use_module(library(system)).

:- include(library(actmods/actmod_hooks)).

:- multifile '$platform$addr'/1.
:- data '$platform$addr'/1.
:- multifile '$actmod$name'/1.
:- data '$actmod$name'/1.

'actmod.save_addr'([Address,Host,PORT]) :-
        atom_codes(PORT, Chs),
        number_codes(Port, Chs), !,
	current_executable(ExePath),
        path_basename(ExePath, ExeFile),
        ( path_splitext(ExeFile, Mod, _), ! ; Mod = ExeFile ),
	connect_platform(Host,Port,Mod,Address).
'actmod.save_addr'([Address,Mod,Host,PORT]) :-
        atom_codes(PORT, Chs),
        number_codes(Port, Chs), !,
	connect_platform(Host,Port,Mod,Address).
'actmod.save_addr'(_) :-
        inform_user(['Bad number of arguments: [port] [name] platform_host platform_port']),
 	halt(1).

connect_platform(Host,Port,Mod,Address):-
        get_pid(Pid),
        Platform=a(Host,Port),
        ( 'actmod.call'(Platform,'actmod.tell_address'(Mod,Address,Pid)) -> true
	; throw(unable_to_connect_platform(Host:Port)) ),
	asserta_fact('$actmod$name'(Mod)),
	displayq(asserta_fact('$actmod$name'(Mod))), nl,
	asserta_fact('$platform$addr'(Platform)).
