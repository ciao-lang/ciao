:- module(_, [lock_file/2, unlock_file/1], []).

:- use_module(library(system), 
        [copy_file/3, get_pid/1, current_host/1, delete_file/1]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(pathnames), [path_concat/3, path_split/3]).

lock_file(File, Result) :-
        symlink_name(File, SymlinkName),
        get_pid(NPid),
        atom_number(Pid, NPid),
        current_host(Host),
        atom_concat(['ciao@',Host,'.',Pid], Target),
        catch(do_symlink(Target, SymlinkName, Result),
              error(system_error,_),
              Result = fail).

do_symlink(Target, SymlinkName, Result) :-
        copy_file(Target, SymlinkName, [symlink]),
        Result = true.

unlock_file(File) :-
        symlink_name(File, SymlinkName),
        delete_file(SymlinkName).

symlink_name(File, SymlinkName) :-
        path_split(File, Dir, Base),
        atom_concat('.#', Base, SymlinkBase),
        path_concat(Dir, SymlinkBase, SymlinkName).
