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

:- use_module(library(pillow/http_server), [http_bind/1, http_loop/1]).
:- include(library(pillow/http_server_hooks)).

:- doc(bug, "fix http_server:http_write_response/2 so that it can
   serve files without loading them in memory").

:- doc(bug, "Configure to allow serving a workspace, many binaries, etc.").
:- doc(bug, "Merge with actmod_cgi").
:- doc(bug, "Start at different ports, act as a proxy with other Ciao servers").
:- doc(bug, "Allow different protocols?").

:- use_module(ciaobld(config_common), [site_root_dir/1]).

http_file_path('', Path) :-
	Path = ~site_root_dir.

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

%external_server_start :- % simple server (no CGI)
%	process_call(path(python), ['-m', 'SimpleHTTPServer'], []).
external_server_start :- % simple server (with CGI under cgi-bin/)
	CgiServer = ~bundle_path(core, 'cmds/ciao-serve/cgi-server.py'),
	process_call(path(python), [CgiServer], []).
