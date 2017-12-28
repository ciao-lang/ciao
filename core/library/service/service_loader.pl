:- module(_, [], [assertions, regtypes, fsyntax]).

:- doc(title, "Dynamic loader of Ciao services").
:- doc(author, "Jose F. Morales").

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

:- use_module(.(service_holder)).
:- use_module(library(service/service_registry)).
:- use_module(library(service/actmod_process)).

:- export(service_loaded/1).
:- pred service_loaded(ServName) :: servname # "@var{ServName} is
   loaded".
:- data service_loaded/1.

:- export(service_load/1).
:- pred service_load(ServName) :: servname # "Load @var{ServName}".
service_load(ServName) :-
	service_load_mode(ServName, Bundle, Mode),
	( Mode = dynmod(Mod) ->
	    loader_message("Loading service `~w`... (dynmod)~n", [ServName]),
	    service_holder:do_use_module(Mod)
	; Mode = child ->
	    % TODO: make sure that this works as expected
	    loader_message("Loading service `~w`... (child actmod)~n", [ServName]),
	    actmod_start(Bundle, ServName, [])
	; % Redirect, daemon, etc. do nothing
	  true
	),
	assertz_fact(service_loaded(ServName)).

:- export(service_restart/1).
:- pred service_restart(ServName) :: servname # "Try to restart
   @var{ServName} if it was loaded as a daemon".
service_restart(ServName) :-
	service_load_mode(ServName, Bundle, Mode),
	( Mode = daemon ->
	    loader_message("Starting service `~w`... (daemon actmod)~n", [ServName]),
	    actmod_start(Bundle, ServName, [daemon])
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

% (see `rundaemon`)
% Currently this is equivalent to:
%
%   $ kill `cat /tmp/Mod.pid`
service_stop(ServName) :-
	service_load_mode(ServName, Bundle, Mode),
	( Mode = daemon ->
	    actmod_kill(Bundle, ServName, Msg),
	    loader_message("Killing ~w [~s]~n", [ServName, Msg])
	; true % nothing
	).

% ---------------------------------------------------------------------------

% :- use_module(library(messages), [warning_message/2]).
:- use_module(library(format), [format/3]).

loader_message(Msg, Args) :-
	format(user_error, Msg, Args).
