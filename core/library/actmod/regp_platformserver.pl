:- module(regp_platformserver,[],[]).

% The 'platformserver' registry protocol (auxiliary for platformbased)

:- include(library(actmod/actmod_hooks)).

:- impl(actmod_locate, platformserver).

(platformserver as actmod_locate).remote_address(_ActRef, DMod, Address) :-
	( current_fact(platform_addr(Address0)) -> true
	; throw(no_platformserver)
	),
	DMod = platformserver, % TODO: check
	Address = Address0.

(platformserver as actmod_locate).cleanup_actI(_ActRef).

% ---------------------------------------------------------------------------

:- data platform_addr/1.

:- export(set_platform_addr/1).
% Set the address of the platformserver
set_platform_addr(PlatformAddr) :-
	retractall_fact(platform_addr(_)),
	asserta_fact(platform_addr(PlatformAddr)).
