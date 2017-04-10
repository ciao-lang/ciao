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

:- doc(bug, "We cannot use http_server.pl until we support an
   extension to content/1 in http_response_string that serves files
   without reading all the contents in memory").

:- doc(bug, "Configure to allow serving a workspace, many binaries, etc.").
:- doc(bug, "Start at different ports, act as a proxy with other Ciao servers").
:- doc(bug, "Allow different protocols?").

:- use_module(ciaobld(config_common), [site_root_dir/1]).

% Open HTTP server for files in this directory
main([]) :-
	format(user_error, "=> starting http server~n", []),
	format(user_error, "   Serving CIAOROOT/build/site files~n", []),
	format(user_error, "   Server reachable at http://localhost:8000~n", []),
	Path = ~site_root_dir,
	cd(Path),
	server_start.

%server_start :- % simple server (no CGI)
%	process_call(path(python), ['-m', 'SimpleHTTPServer'], []).
server_start :- % simple server (with CGI under cgi-bin/)
	CgiServer = ~bundle_path(core, 'cmds/ciao-serve/cgi-server.py'),
	process_call(path(python), [CgiServer], []).
