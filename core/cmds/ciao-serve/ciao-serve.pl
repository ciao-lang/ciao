:- module(_, [main/1], [assertions, fsyntax]).

:- doc(title, "Ciao server").
:- doc(author, "Jose F. Morales").

:- doc(module, "This command implements a server process for Ciao
   services (active modules, HTTP requests, etc.). See
   @lib{service_registry} and @lib{serve_http} for more details.").

:- use_module(library(http/http_server), [http_bind/1, http_loop/1]).
:- use_module(library(system), [cd/1]).
:- use_module(library(format), [format/3]).

:- use_module(library(system_extra), [mkpath/1]).

:- use_module(library(service/service_loader), [service_stop_all/0]).
:- use_module(library(service/service_registry), [reload_service_registry/0]).
:- use_module(library(service/serve_http), []). % (implements httpserv.handle/3)

:- use_module(library(actmod/actmod_dist), [dist_set_reg_protocol/1]).
:- use_module(library(actmod/regp_filebased), []). % TODO: more protocols?

% :- use_module(library(service/actmod_ctl), []). % TODO: make it optional?

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

serve([X]) :- ( X = help ; X = '-h' ; X = '--help' ), !,
    help.
serve([stop]) :- !,
    format(user_error, "=> stopping daemons~n", []),
    reload_service_registry,
    service_stop_all.
serve(Args) :-
    ( Args = ['-p', PortAtm|Rest] ->
        atom_number(PortAtm, Port)
    ; % (leave Port unbound)
      Rest = Args
    ),
    Rest = [],
    % Select default port
    ( var(Port) -> Port = 8000 ; true ),
    dist_set_reg_protocol(filebased), % TODO: customize? needed for actmod_http
    serve_at_port(Port).

:- use_module(library(system_extra), [mkpath/1]).

serve_at_port(Port) :-
    format(user_error, "=> starting HTTP server~n", []),
    reload_service_registry,
    serve_banner(Port),
    http_bind(Port),
    % Ensure that site dir is created, move there % TODO: needed?
    SiteDir = ~ensure_site_dir,
    cd(SiteDir),
    %
    http_loop(ExitCode),
    format(user_error, "=> HTTP server finished with exit code ~w~n", [ExitCode]),
    halt(ExitCode).

% ---------------------------------------------------------------------------
% TODO: move as an lpdoc service, simplify catalog_ui

:- include(library(http/http_server_hooks)).
:- use_module(library(pillow/html), [html2terms/2]).

:- use_module(ciaobld(config_common), [site_root_dir/1]).
:- use_module(ciaobld(site_aux), [site_link_builddoc/0]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(system), [file_exists/1, directory_files/2]).
:- use_module(library(pathnames), [path_concat/3, path_splitext/3]).

ensure_site_dir := SiteDir :-
    SiteDir = ~site_root_dir,
    mkpath(SiteDir),
    site_link_builddoc. % TODO: always?

'httpserv.handle'(Path, _Request, Response) :- is_dir_path(Path), !,
    html2terms(Str, ~render_page),
    Response = html_string(Str).

is_dir_path("/") :- !, no_index.
is_dir_path("/index.html") :- !, no_index.
is_dir_path("/dir").

% no index file available
no_index :- \+ file_exists(~path_concat(~site_root_dir, 'index.html')).

render_page := R :-
    R = [
      start,
      title('Ciao documentation index'),
      h1('Ciao documentation index'),
      Docs,
      end
    ],
    DocDir = ~path_concat(~site_root_dir, 'ciao/build/doc'),
    directory_files(DocDir, Fs),
    findall(Doc, (member(F, Fs), docitem(F, Doc)), Docs).

docitem(F, Doc) :-
    path_splitext(F, Name, '.html'), % fail otherwise
    Ref = ~path_concat(~path_concat('/ciao/build/doc', F), 'index.html'),
    Doc = [ref(Ref, [Name]), \\].

