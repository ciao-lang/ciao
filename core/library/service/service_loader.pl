:- module(_, [], [assertions, regtypes, fsyntax, datafacts]).

:- doc(title, "Dynamic loader of Ciao services").
:- doc(author, "Jose F. Morales").

% TODO:T257 merge with active modules

:- doc(module, "Dynamic loader of Ciao services. Only services listed
   on the @lib{service_registry} are considered. Services are loaded
   dynamically depending on the options specified in their registry
   entry:

   @begin{itemize}
   @item dynamic loading of modules (same process)
   @item child processes (for active modules)
   @item daemon processes (for active modules)
   @end{itemize}
   ").


:- use_module(library(service/service_registry)).
:- use_module(library(actmod/actmod_process)).
:- use_module(library(actmod/actmod_rt), [actI_init_named/2]).

:- export(service_loaded/1).
:- pred service_loaded(ServName) :: servname # "@var{ServName} is
   loaded".
:- data service_loaded/1.

% TODO:T253 Include in actmod_spawn and actI_init_named?
:- include(library(actmod/actmod_hooks)). % (for '$static_named_actRef'/2)
'$static_named_actRef'(ActRef, DMod) :- service_loaded(ActRef), !, DMod = ActRef.

:- export(service_load/1).
:- pred service_load(ServName) :: servname # "Load @var{ServName}".
service_load(ServName) :-
	service_load_mode(ServName, Mode),
	( Mode = dynmod(Mod) ->
	    loader_message("Loading actmod `~w`... (same process)~n", [ServName]),
	    actmod_load_dynmod(Mod),
	    loader_message("Spawning actmod `~w`... (same process)~n", [ServName]),
	    % TODO: assumes static_named_actRef and "same process"
	    actI_init_named(ServName, ServName)
	; Mode = child(ExecPath) ->
	    loader_message("Spawning actmod `~w`... (child)~n", [ServName]),
	    actmod_spawn(ServName, [exec(ExecPath), child], _ActRef)
	; % Redirect, daemon, etc. do nothing
	  true
	),
	assertz_fact(service_loaded(ServName)).

:- export(service_restart/1).
:- pred service_restart(ServName) :: servname # "Try to restart
   @var{ServName} if it was loaded as a daemon".
service_restart(ServName) :-
	service_load_mode(ServName, Mode),
	( Mode = daemon(ExecPath) ->
	    loader_message("Spawning actmod `~w`... (daemon)~n", [ServName]),
	    catch(actmod_spawn(ServName, [exec(ExecPath), daemon], _ActRef),
	          _, true) % ignore errors (it may need some time to start)
	; true % nothing
	).

:- export(service_stop_all/0).
:- pred service_stop_all # "Stop all services loaded as daemons".
service_stop_all :-
	( service_entry(_, ServName, _),
	    service_stop(ServName),
	    fail
	; true
	).

service_stop(ServName) :-
	service_load_mode(ServName, Mode),
	( Mode = daemon(_ExecPath) ->
	    ActRef = ServName, % TODO:T253 see [new-actref]
	    catch(actmod_kill(ActRef, Msg), _E, fail), % ignore if we could not kill it
	    loader_message("Killing ~w [~s]~n", [ActRef, Msg])
	; true % nothing
	).

% ---------------------------------------------------------------------------

% :- use_module(library(messages), [warning_message/2]).
:- use_module(library(format), [format/3]).

loader_message(Msg, Args) :-
	format(user_error, Msg, Args).
