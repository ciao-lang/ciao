:- module(platformbased_locate, [module_address/2], []).

:- use_module(library(actmods/actmodrt), [remote_call/2]).

:- multifile '$platform$addr'/1.
:- data '$platform$addr'/1.
:- data address_db/2.

module_address(M, Address) :-
	current_fact(address_db(M, Address)), !.
module_address(M, Address) :-
	current_fact('$platform$addr'(Platform)), !,
        ( remote_call(Platform,module_address(M,Address)) -> true
	; throw(no_module_address_for(M)) ),
	asserta_fact(address_db(M, Address)).
module_address(M, _Address) :-
        throw(no_platform_for(M)).
