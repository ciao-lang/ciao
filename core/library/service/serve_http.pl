:- module(_, [], [assertions, fsyntax]).

:- doc(title, "HTTP handler for dynamic Ciao services").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module implements the default HTTP handler for
   Ciao services that is able to start services dynamically (see
   @lib{service_loader}).

   A Ciao service is a module or active module (@lib{actmod}) that
   implements an HTTP handler (@lib{httpserv.handle/3}) or a bridge
   between HTTP and other protocols (see @pred{http_bridge/5}).

   The server includes by default a HTTP server exposing files from
   @tt{build/site} in the current workspace.

   This module is used from the @tt{ciao-serve.pl} command line tool.
   ").

:- include(library(http/http_server_hooks)).

:- doc(bug, "see bugs at @lib{http_server}").

% ---------------------------------------------------------------------------
:- doc(section, "Serve files under site root dir").

:- use_module(ciaobld(config_common), [site_root_dir/1]).

'httpserv.file_path'('', Path) :-
	Path = ~site_root_dir.

% ---------------------------------------------------------------------------
% Bridges to other protocols

% TODO: Missing 'httpserv.handle'/3 in a child/daemon process (Add as another protocol?)
:- multifile http_bridge/5.
% http_bridge(Protocol, ServName, Request, Response, Status)
:- use_module(library(actmod_async/actmod_http), []).

% ---------------------------------------------------------------------------
:- doc(section, "HTTP handler").

:- use_module(library(http/http_service_rt)).
:- use_module(library(service/service_registry), [service_lookup/2]).
:- use_module(library(service/service_loader)).

% Handler for registered services:
%  - load services dynamically if needed ('httpserv.handle'/3 may be extended)
%  - bridge between HTTP and other protocols (see http_bridge/5)

'httpserv.handle'(Path, Request, Response) :-
	http_decode_service(Path, ServName, Protocol),
	( service_loaded(ServName), Protocol = http ->
	    fail % delegate on other 'httpserv.handle' clauses
	; !, % do not consider other handlers
	  http_handle_(ServName, Protocol, Path, Request, Response)
	).

http_handle_(ServName, Protocol, Path, Request, Response) :-
	( \+ service_loaded(ServName) ->
	    Status = load_and_retry
	; % Use bridge between HTTP and other protocols
	  http_bridge(Protocol, ServName, Request, Response, Status)
	),
	( Status = load_and_retry ->
	    % Load
	    service_load(ServName),
	    % Try again the HTTP handler (which may be extended)
	    'httpserv.handle'(Path, Request, Response)
	; Status = need_restart ->
	    service_restart(ServName)
	; true
	).

http_decode_service(Path, ServName, Protocol) :-
	split_query_str(Path, URI, _),
	service_path(ServName, URI),
	service_lookup(ServName, Protocol).

