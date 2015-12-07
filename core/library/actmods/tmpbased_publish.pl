:- module(tmpbased_publish, [], []).

:- use_module(library(actmods/tmpbased_common)).
:- use_module(library(pathnames)).
:- use_module(library(system)).

:- multifile save_addr_actmod/1.

save_addr_actmod([Address]) :- !,
        current_executable(ExePath),
        path_basename(ExePath, ExeFile),
        ( path_splitext(ExeFile, Mod, _), ! ; Mod = ExeFile ),
        module_to_addressfile(Mod, AddrPath),
        ( file_exists(AddrPath) ->
	  catch(delete_file(AddrPath), error(A, B),
	  handler(A, B, AddrPath))
	; true ),
        get_pid(Pid),
        umask(OldUmask,0o077),
        open(AddrPath, write, ST),
        current_output(OldOut),
        set_output(ST),
        display_term(Address),
        display_term(pid(Pid)),
        set_output(OldOut),
        close(ST),
        umask(_, OldUmask).
save_addr_actmod(_) :-
        inform_user(['Bad number of arguments: either none or port number']),
 	halt(1).


handler(system_error, B, File) :-
	message( error , [ 'System Error in ', B, 
                          ' when working with file ' , File ] ).
handler(A, B, File) :-
	message( error , [ 'No message for ' , A , B, File ] ).
