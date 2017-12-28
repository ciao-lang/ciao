:- module(actmod_ctl, [], [assertions, fsyntax, hiord]).

:- doc(title, "Controller for (daemon) active modules").

:- doc(module, "Active module that can control active modules started
   as daemons in this machine.").

% Async commands
:- include(library(actmod_async/actmod_async_hooks)).
:- use_module(library(actmod_async/actmod_async_rt), [async_call/2]).

:- use_module(library(service/actmod_process)).

'actmod.in_process'(actmod_ctl).

'actmod.in_process_call'('actmod_ctl', async_call(AsyncRequest, AsyncResponse)) :-
	actmod_ctl:async_call(AsyncRequest, AsyncResponse).

async_call(AsyncRequest, AsyncResponse) :-
	actmod_async_rt:async_call(AsyncRequest, AsyncResponse).

:- use_module(library(service/service_registry), [service_entry/3]).

% Kill @var{TargetActMod}
'async.decl'(kill(_)).
'async.ftypes'(kill(_), [atm]).
'async.run'(kill(TargetActMod), Cont) :- !,
	% TODO: only first bundle; add bundle as param?
	% lookup Bundle, make sure it is a daemon
	( service_entry(Bundle, TargetActMod, Props),
	  member(daemon, Props) ->
	    true 
	; fail
	),
	actmod_kill(Bundle, TargetActMod, Msg),
	Cont = [
          set_buf('console', Msg)
	].

% Ping (just to see if we are live)
'async.decl'(ping).
'async.ftypes'(ping, []).
'async.run'(ping, Cont) :- !,
	Cont = [].

% TODO: implemented at ciao_playground.pl; use something simpler? (generic log)
% Set value of buffer BufName
'async.ftypes'(set_buf(_, _), [const, string]).
