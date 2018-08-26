:- module(_, [], [assertions, fsyntax]).

:- doc(title, "HTTP handler for Ciao services").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module implements the default HTTP handler for
   Ciao services that is able to start services dynamically (see
   @lib{service_loader}).

   A Ciao service is an active module (@lib{actmod}) or a module
   implementing an HTTP handler (see @lib{http/http_server_hooks}).

   The server includes by default a HTTP server exposing files from
   @tt{build/site} in the current workspace.

   This module is used from the @tt{ciao-serve.pl} command line tool.
   ").

:- include(library(http/http_server_hooks)).

:- doc(bug, "See bugs at @lib{http_server}").
:- doc(bug, "Websocket-based distribution protocol for active modules").

% ---------------------------------------------------------------------------
:- doc(section, "Serve files under site root dir").

:- use_module(ciaobld(config_common), [site_root_dir/1]).

'httpserv.file_path'('', Path) :-
	Path = ~site_root_dir.

% ---------------------------------------------------------------------------
:- doc(section, "HTTP handler with active module bridge").

% TODO: allow non-deterministic code in async requests
% TODO: missing timeout

:- use_module(library(http/http_service_rt)).
:- use_module(library(service/service_registry), [service_lookup/2]).
:- use_module(library(service/service_loader)).
:- use_module(library(system), [system_error_report/1]).
:- use_module(library(actmod/actmod_rt), [actI_send_call/3, actI_receive_response/2, get_actI/2, force_set_actref/1]).

:- use_module(library(actmod_http)).

% Handler for registered services:
%  - load services dynamically if needed ('httpserv.handle'/3 may be extended)
%  - bridge between HTTP and active modules

'httpserv.handle'(Path, Request, Response) :-
	http_decode_service(Path, ServName, Protocol),
	( service_loaded(ServName), Protocol = http ->
	    fail % delegate on other 'httpserv.handle' clauses
	; !, % cut - do not consider other handlers
	  http_handle_(ServName, Protocol, Path, Request, Response)
	).

http_handle_(ServName, Protocol, Path, Request, Response) :-
	( \+ service_loaded(ServName) ->
	    % Load and try again the HTTP handler (which may be extended)
	    service_load(ServName),
	    'httpserv.handle'(Path, Request, Response)
	; % Use bridge between HTTP and other protocols
	  http_bridge(Protocol, ServName, Request, Response, Status),
	  ( Status = need_restart ->
	      service_restart(ServName)
	  ; true
	  )
	).

http_decode_service(Path, ServName, Protocol) :-
	split_query_str(Path, URI, _),
	service_path(ServName, URI),
	service_lookup(ServName, Protocol).

% TODO: use io_sched, etc.
http_bridge(actmod, ServName, Request, Response, Status) :- !,
	force_set_actref(serve_http), % TODO: hack, use fibers in ciao-serve main
	% bridge between HTTP and active modules
	actRequest_from_http(ServName, Request, ActRequest),
	protected_send_and_receive(ActRequest, ActResponse, Status),
	actResponse_to_http(ActResponse, Response).
http_bridge(Protocol, _, _, _, _) :- !,
	throw(unknown_protocol(Protocol)).

% Route the request to the active module (in-process or remote).
% The active module is asked to be (re)started if needed.
protected_send_and_receive(ActRequest, ActResponse, Status) :-
	get_actI(ActRequest, ActRef),
	% TODO: distinguish failure from error
	catch(actI_send_call(ActRef, async_json, ActRequest), E, on_err(E)),
	% TODO: do not block here?
	catch(actI_receive_response(ActRef, ActResponse0), E, on_err(E)),
	!,
	Status = ok,
	ActResponse = ActResponse0.
protected_send_and_receive(_ActRequest, ActResponse, Status) :-
	% Error during call.
	% We send the 'not ready' message and mark Status as 'need_restart'
	% We not try the async call immediately again since the daemon
	% may need some time to start.
	Status = need_restart,
	not_ready_response(async_json, ActResponse).

:- use_module(library(actmod/actmod_rt), [dist_log/1]).
on_err(E) :-
	dist_log([~~(E)]),
	fail.

% TODO: do for any QProt
not_ready_response(async_json, ActResponse) :-
        system_error_report(ErrorMsg0),
	atom_codes(ErrorMsg0, ErrorMsg),
	ActResponse = ~async_json_error(not_ready, ErrorMsg).

