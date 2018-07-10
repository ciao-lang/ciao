:- module(regp_webserver, [], []).

:- include(library(actmod/actmod_hooks)).

% The 'webserver' registry protocol (auxiliary for webbased)

:- use_module(library(actmod/webbased_common)).
:- use_module(library(system), [umask/2]).

% ---------------------------------------------------------------------------

:- impl(actmod_publish, webserver).

(webserver as actmod_publish).save_addr(_ActRef, _DMod, Address, Pid, _Opts) :-
	common_path(Path),
        umask(OldUmask,0o022),
        open(Path, write, ST),
        current_output(OldOut),
        set_output(ST),
        display(server(Address,Pid)), nl,
        set_output(OldOut),
        close(ST),
        umask(_, OldUmask).

% ---------------------------------------------------------------------------

:- impl(actmod_locate, webserver).

(webserver as actmod_locate).remote_address(_ActRef, DMod, Address) :-
	common_url(URL),
	DMod = webserver, % TODO: check
	fetch_server_addr(URL, Address).

(webserver as actmod_locate).cleanup_actI(_ActRef).
