:- module(actmod_ctl, [], [assertions, fsyntax, hiord, actmod]).

:- doc(title, "Controller for (daemon) active modules").

:- doc(module, "Active module that can control active modules started
   as daemons in this machine.").

:- dist_node. % TODO: (main/1 is not used here)
% :- use_module(library(actmod_http), []). % TODO: remove if not needed

:- use_module(library(actmod/actmod_process)).
:- use_module(library(service/service_registry), [service_load_mode/2]).

% Kill @var{ActRef}
:- suspendable(kill(atm)).
kill(ActRef) :-
	ServName = ActRef, % TODO:T253 see [new-actref]
	service_load_mode(ServName, Mode),
	( Mode = daemon(_) ->
	    % TODO: show message somewhere?
	    actmod_kill(ActRef, _Msg)
	; true % nothing
	).

% Ping (just to see if we are alive)
:- suspendable(ping).
ping.


