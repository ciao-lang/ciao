:- module(webserver_publish, [], []).

:- use_module(library(actmod/webbased_common)).
:- use_module(library(system)).

:- include(library(actmod/actmod_hooks)).

'actmod.save_addr'([Address]) :-
        get_pid(Pid),
	common_path(Path),
        umask(OldUmask,0o022),
        open(Path, write, ST),
        current_output(OldOut),
        set_output(ST),
        display(server(Address,Pid)), nl,
        set_output(OldOut),
        close(ST),
        umask(_, OldUmask).
'actmod.save_addr'(_) :-
        inform_user(['Bad number of arguments: either none or port number']),
 	halt(1).
