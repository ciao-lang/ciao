:- module(filebased_publish, [], []).

:- use_module(library(system)).
:- use_module(library(pathnames)).
:- use_module(library(read)).

:- multifile save_addr_actmod/1.

% The file that contains the path to the directory that contains the 
% address file has to be called '.addr'

save_addr_actmod([Address]) :- !,
	(file_exists('./.addr')->
	 open('./.addr', read, S),
	 read(S, Location_Dir),
	 close(S)
	;
	 Location_Dir='./'),
	current_executable(ExePath),
        path_basename(ExePath, ExeFile),
        ( path_splitext(ExeFile, Mod, _), ! ; Mod = ExeFile ),
        atom_concat(Mod, '.addr', Mod_addr),
	atom_concat(Location_Dir, Mod_addr, Path_Mod_Addr), 
        get_pid(Pid),
        umask(OldUmask,0o022),
        open(Path_Mod_Addr, write, ST),
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
