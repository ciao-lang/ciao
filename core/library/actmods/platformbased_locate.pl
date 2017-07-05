:- module(platformbased_locate, ['actmod.address'/2], []).

:- use_module(library(actmods/actmod_rt), ['actmod.call'/2]).

:- multifile '$platform$addr'/1.
:- data '$platform$addr'/1.
:- data address_db/2.

'actmod.address'(M, Address) :-
	current_fact(address_db(M, Address)), !.
'actmod.address'(M, Address) :-
	current_fact('$platform$addr'(Platform)), !,
        ( 'actmod.call'(Platform,'actmod.address'(M,Address)) -> true
	; throw(no_module_address_for(M)) ),
	asserta_fact(address_db(M, Address)).
'actmod.address'(M, _Address) :-
        throw(no_platform_for(M)).
