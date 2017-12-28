:- module(actmod_dyn, [], [assertions, fsyntax, hiord]).

:- doc(title, "Dynamic call to active modules").

:- doc(module, "Implement dynamic call to active modules (without
   @tt{use_active_module}).").

% TODO: Document 'actmod.in_process'/1 (we can call active modules
%   that has been loaded dynamically in the same process)

% :- use_package(actmod).
% :- use_module(library('actmod/tmpbased_locate')). 
% :- use_active_module(..., [...]).

:- include(library(actmod_async/actmod_async_hooks)).

% NOTE: active modules should be compiled with tmpbased_publish method
:- use_module(library(actmod/tmpbased_locate),
	['actmod.address'/2, 'actmod.reset_address'/1]). 
:- use_module(library(actmod/actmod_rt), ['actmod.call'/2]).

% Dynamic call on active modules
%
% Exceptions:
%   error(connection_error(actmod, Mod),actmod_call/2):
%     Could not connect to active module Mod
%   error(existence_error(actmod, Mod),actmod_call/2):
%     Could not find address to active module Mod

:- export(actmod_call/2).
actmod_call(Mod, _Goal) :-
	var(Mod),
	!,
	throw(error(unbound_mod, actmod_call/2)).
actmod_call(Mod, Goal) :-
	'actmod.in_process'(Mod),
	!,
	'actmod.in_process_call'(Mod, Goal).
actmod_call(Mod, Goal) :-
	catch('actmod.address'(Mod,Addr), E, address_error(E, Mod)),
	catch('actmod.call'(Addr, Goal), E, call_error(E, Mod)).

address_error(error(existence_error(source_sink,_),_), Mod) :- !,
	throw(error(existence_error(actmod, Mod), actmod_call/2)).
address_error(E, _) :- throw(E).

call_error(error(system_error, 'sockets:connect_to_socket_type'/4-1), Mod) :- !,
	'actmod.reset_address'(Mod), % Reset memoized address
	throw(error(connection_error(actmod, Mod), actmod_call/2)).
call_error(E, _) :- throw(E).

