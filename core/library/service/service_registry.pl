:- module(service_registry, [], [regtypes, assertions, dcg, fsyntax]).

:- doc(title, "Service registry").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module provides a predicate to locate the
   services exported by bundles. These services can be loaded
   dynamically via @lib{service_loader} and commands like
   @apl{ciao-serve}.").

:- doc(bug, "Performance: loading full manifests is slow. Fill a
   registry during 'custom_run . dist' or 'build'?").

:- doc(bug, "Prune dependencies to builder (see TODO notes below)").

:- use_module(library(lists), [member/2]).
:- use_module(library(pathnames)).
:- use_module(ciaobld(site_aux), [bundle_site_url/3]). % TODO: move to other module?

:- export(servname/1).
:- regtype servname(X) # "@var{X} is a service name". % TODO: (typically an active module name)
servname(X) :- atm(X).

:- export(bundle_http_entry/3).
% bundle_http_entry(?Bundle, ?Name, -URL):
%   Entry point for HTTP (browser) on the specified Bundle.
%   `Name` is a global name (assumed to be unique) % TODO: qualify bundle if needed?
bundle_http_entry(Bundle, Name, URL) :-
	service_entry(Bundle, Name, Props),
	( member(http, Props) -> true ; fail ),
	( member(redirect(Addr0), Props) -> URL0 = Addr0
	; atom_concat('/', Name, URL0) % TODO: share
	),
	\+ member(hide, Props),
	bundle_site_url(Bundle, URL0, URL).

% ---------------------------------------------------------------------------

:- data service_entry_/3.
% service_entry_(?Bundle, ?Name, -Props)
%   Bundle: bundle
%   Name: service name
%   Props: properties of this entry
%
% Properties are:
% (protocol)
%   actmod: serves actmod requests
%   http: serves HTTP requests
% (source and loading mode)
%   main=Main: main module implementing the http_service.pl (for dynmod)
%   daemon: service is an actmod as a daemon process
%   child: service is an actmod as a child process
%   dynmod: service is a dynamically loaded module (default)
%   redirect(_): redirect to some other URI (relative to build site)
%   hide: do not list this entry in documentation, etc.
:- data service_entry_srvname/2.
% service_entry_srvname(?Name, -Protocol)

:- export(service_entry/3).
service_entry(Bundle, Name, Props) :-
	service_entry_(Bundle, Name, Props).
% service_entry(core, actmod_ctl, [actmod, main=..., hide]). % (statically loaded from ciao-serve.pl)

:- export(service_lookup/2).
service_lookup(Name, Protocol) :-
	service_entry_srvname(Name, Protocol).

clean_service_entries :-
	retractall_fact(service_entry_(_, _, _)),
	retractall_fact(service_entry_srvname(_, _)).

add_service_entry(Bundle, Name, Props) :-
	( member(actmod, Props) -> Protocol = actmod
	; member(http, Props) -> Protocol = http
	; Protocol = unknown
	),
	assertz_fact(service_entry_(Bundle, Name, Props)),
	assertz_fact(service_entry_srvname(Name, Protocol)).

:- use_module(ciaobld(manifest_compiler), [main_file_path/3]).
:- use_module(ciaobld(config_common), [libcmd_path/4]).

:- export(service_load_mode/2).
% Obtain load mode of the given service Name.
% Mode is one of:
%  - child(ExecPath): child process with executable at ExecPath
%  - daemon(ExecPath): daemon process with executable at ExecPath
%  - dynmod(AbsPath): module with code at AbsPath
service_load_mode(Name, Mode) :-
	( service_entry(Bundle, Name, Props0) -> % TODO: only first bundle; add bundle as param? follow workspaces instead?
	    Props = Props0
	; throw(service_not_found(Name))
	),
	( member(child, Props) ->
	    ExecPath = ~libcmd_path(Bundle, plexe, Name),
	    Mode = child(ExecPath)
	; member(daemon, Props) ->
	    ExecPath = ~libcmd_path(Bundle, plexe, Name),
	    Mode = daemon(ExecPath)
	; AbsPath = ~main_file_path(Bundle, Props) ->
	    Mode = dynmod(AbsPath)
	; Mode = unknown
	).

% ---------------------------------------------------------------------------

% TODO: change dynmod by main='...'? and dynmod?

:- use_module(engine(io_basic)). % TODO: use message/2?
:- use_module(engine(internals), ['$bundle_id'/1]).
:- use_module(ciaobld(manifest_compiler), [
    ensure_load_manifest/1,
    manifest_call/2
]).

:- export(reload_service_registry/0).
reload_service_registry :-
	display(user_error, '   Loading service registry...'), nl(user_error),
	% TODO: slow! (see notes above)
	clean_service_entries,
	% TODO: make it optional?
	% add_service_entry(core, actmod_ctl, [actmod]),
	%
	( % (failure-driven loop)
	  '$bundle_id'(Bundle),
	  ensure_load_manifest(Bundle), % TODO: slow?
	  manifest_call(Bundle, service(Name, Props)),
	    add_service_entry(Bundle, Name, Props),
	    fail
	; true
	),
	display(user_error, '   Service registry loaded'), nl(user_error).

