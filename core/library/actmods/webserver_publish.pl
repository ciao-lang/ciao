:- module(webserver_publish, [], []).

:- use_module(library(actmods/webbased_common)).
:- use_module(library(system)).

:- multifile save_addr_actmod/1.

save_addr_actmod([Address]) :-
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
save_addr_actmod(_) :-
        inform_user(['Bad number of arguments: either none or port number']),
 	halt(1).
