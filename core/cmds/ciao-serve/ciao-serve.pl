:- module(_, [main/1], [assertions, fsyntax]).

:- doc(title, "Ciao server").
:- doc(author, "Jose F. Morales").

:- doc(module, "This command implements a server process for Ciao
   services (active modules, HTTP requests, etc.). See
   @lib{service_registry} and @lib{serve_http} for more details.").

:- use_module(ciaobld(config_common), [site_root_dir/1]).
:- use_module(library(http/http_server), [http_bind/1, http_loop/1]).
:- use_module(library(system), [cd/1]).
:- use_module(library(format), [format/3]).

:- use_module(library(service/service_loader), [service_stop_all/0]).
:- use_module(library(service/service_registry), [reload_service_registry/0]).
:- use_module(library(service/serve_http), []). % (implements httpserv.handle/3)

:- use_module(library(service/actmod_ctl), []). % TODO: make it optional?

:- doc(bug, "Customize available services").
:- doc(bug, "Make it work as proxy").
:- doc(bug, "Binding only to loopback (for security)").

main(Args) :- serve(Args).

help_msg("Usage: ciao-serve [-p Port] <action>

where action is one of:
  help - show this message
  stop - kill running daemons
  <default> - start the server

").

help :-
	help_msg(Msg),
	format(user_error, "~s", [Msg]).

serve_banner(Port) :-
	format(user_error, "   Serving CIAOROOT/build/site files~n", []),
	format(user_error, "   Server reachable at http://localhost:~w~n", [Port]).

serve([help]) :- !,
	help.
serve([stop]) :- !,
	format(user_error, "=> stopping daemons~n", []),
	reload_service_registry,
	service_stop_all.
serve(['-p', PortAtm]) :- !,
	atom_codes(PortAtm, Cs),
	number_codes(Port, Cs),
	serve_at_port(Port).
serve([]) :- !,
	serve_at_port(8000).

serve_at_port(Port) :-
	format(user_error, "=> starting HTTP server~n", []),
	reload_service_registry,
	serve_banner(Port),
	http_bind(Port),
	Path = ~site_root_dir, cd(Path), % TODO: not needed?
	http_loop(ExitCode),
	halt(ExitCode).

