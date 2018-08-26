:- module(filebased_common, [], []).

% Common predicates for all file-based publish/locate methods

:- use_module(engine(stream_basic)).
:- use_module(engine(io_aux), [display_term/1]).
:- use_module(library(system), [file_exists/1, delete_file/1, umask/2]).
:- use_module(library(read), [read/2]).
:- use_module(library(pathnames), [path_concat/3]).

:- export(actI_to_addrpath/3).
% actI_to_addrpath(+BaseDir, +ActRef, -AddrPath):
%   File path for ActRef address. Resolves to
%   <<LocDir>>/<<ActRef>>.addr, where LocDir is the path specified at
%   the <<BaseDir>>/.addr file or BaseDir.
actI_to_addrpath(BaseDir, ActRef, AddrPath) :-
	path_concat(BaseDir, '.addr', Loc),
	( file_exists(Loc)->
	    open(Loc, read, S),
	    read(S, LocDir),
	    close(S)
	; LocDir = BaseDir
	),
        atom_concat(ActRef, '.addr', AddrFile),
	path_concat(LocDir, AddrFile, AddrPath).

:- export(file_save_addr/5).
file_save_addr(Umask, AddrPath, DMod, Address, Pid) :-
        ( file_exists(AddrPath) ->
	    delete_file(AddrPath)
	; true
	),
        umask(OldUmask, Umask),
        open(AddrPath, write, ST),
        current_output(OldOut),
        set_output(ST),
        display_term(DMod),
        display_term(Address),
        display_term(pid(Pid)),
        set_output(OldOut),
        close(ST),
        umask(_, OldUmask).

:- export(file_load_addr/3).
file_load_addr(AddrPath, DMod, Address) :-
	open(AddrPath, read, S),
	read(S, DMod0),
	read(S, Address0),
	close(S),
	DMod = DMod0,
	Address = Address0.

% ---------------------------------------------------------------------------
% Directory for ActRef name registry 

:- use_module(library(system), [get_tmp_dir/1]).

:- data reg_dir/1.

:- export(set_reg_dir/1).
set_reg_dir(Path) :-
	retractall_fact(reg_dir(_)),
	asserta_fact(reg_dir(Path)).

:- export(get_reg_dir/1).
get_reg_dir(Path) :-
	( current_fact(reg_dir(Path0)) -> Path = Path0
	; get_tmp_dir(Path) % TODO: use datadir instead!
	).
