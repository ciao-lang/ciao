:- module(_, [main/1], [assertions, fsyntax]).

:- doc(title, "Ciao as a server (using HTTP protocol)").
:- doc(author, "Jose F. Morales").

:- doc(module, "This command creates a server process (HTTP protocol)
   to access data (e.g., under @tt{build/site}) and interact with
   binaries (e.g., CGIs) in the current workspaces.

   It can be used during development or combined with HTTP reverse
   proxies (please use with care, we provide no sandboxing
   mechanism).").

:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(library(format), [format/3]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(system), [cd/1]).
:- use_module(engine(internals), [ciao_root/1]).

:- use_module(library(http/http_server), [http_bind/1, http_loop/1]).
:- include(library(http/http_server_hooks)).

:- doc(bug, "fix http_server:http_write_response/2 so that it can
   serve files without loading them in memory").

:- doc(bug, "Configure to allow serving a workspace, many binaries, etc.").
:- doc(bug, "Merge with actmod_cgi").
:- doc(bug, "Start at different ports, act as a proxy with other Ciao servers").
:- doc(bug, "Allow different protocols?").

:- use_module(ciaobld(config_common), [site_root_dir/1]).

msg :-
	format(user_error, "   Serving CIAOROOT/build/site files~n", []),
	format(user_error, "   Server reachable at http://localhost:8000~n", []).

% Open HTTP server for files at ~site_root_dir
main([builtin]) :- !,
	format(user_error, "=> starting built-in HTTP server~n", []),
	msg,
	Path = ~site_root_dir, cd(Path), % TODO: not needed?
	http_bind(8000),
	http_loop(_).
main([]) :-
	format(user_error, "=> starting external HTTP server~n", []),
	msg,
	Path = ~site_root_dir,
	cd(Path),
	external_server_start.

http_file_path('', Path) :-
	Path = ~site_root_dir.

% ---------------------------------------------------------------------------
% Dynamic load of HTTP services (http_service.pl)

:- use_module(.(ciao_service_holder)).
:- use_module(library(http/http_service_rt)).

http_handle(Path, Request, Response) :-
	service_path(Name, Path),
	\+ service_loaded(Name), % Abort rule if service is already loaded
	!,
	assertz_fact(service_loaded(Name)),
	dynload_service(Name),
	% Try again (handler may have been extended)
	http_handle(Path, Request, Response).

dynload_service(Name) :-
	( service_entry(Name, dynmod(Mod)) ->
	    format(user_error, "Loading service `~w`...~n", [Name]),
	    ciao_service_holder:do_use_module(Mod),
	    format(user_error, "Done~n", [])
	; format(user_error, "error: service `~w` is not registered~n", [Name])
	).

:- data service_loaded/1.

% TODO: configure! add entries in build/services/?

service_entry(actmod, dynmod(ciaopp_online(actmod_cgi))). % TODO: talk directly with active modules here? (need some protocol though)
%
service_entry(download, dynmod(ciao_website(download_cgi/download))).
service_entry(download_stats, dynmod(ciao_website(download_cgi/download_stats))).
service_entry(finder_cgi, dynmod(deepfind_ui(finder_cgi))).
service_entry(chat80, dynmod(chat80_ui(chat80_ui))).
service_entry(web_ui, dynmod(ciao_webide(web_ui_serve))).

% ---------------------------------------------------------------------------

%externalserver_start :- % simple server (no CGI)
%	process_call(path(python), ['-m', 'SimpleHTTPServer'], []).
external_server_start :- % simple server (with CGI under cgi-bin/)
	CgiServer = ~bundle_path(core, 'cmds/ciao-serve/cgi-server.py'),
	process_call(path(python), [CgiServer], []).
