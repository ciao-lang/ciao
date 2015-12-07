:- module(persdb_cache,
	[ add_term_to_file/3, add_term_to_file_db/3,
	  delete_bak_if_no_ops/2, delete_file1/1,
	  get_pred_files/7, keyword/1, persistent/5 ],
	[ assertions ]).

:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(file_locks)).
:- use_module(library(persdb/persdb_rt), [create/2]).

:- data persistent/5. % F/A (modulo expanded) is persistent and uses files
                      % FILE_OPS, FILE and FILE_BAK

add_term_to_file_db(Term, Pred, FilePerms) :-
        functor(Pred, F, N),
        current_fact(persistent(F, N, File_ops, _, File_bak)), !,
        delete_bak_if_no_ops(File_ops, File_bak),
        add_term_to_file(Term, File_ops, FilePerms).
add_term_to_file_db(_Term, _Pred, _FilePerms).

% This ensure that we not create an operations file in a transient state
delete_bak_if_no_ops(File_ops, File_bak) :-
        ( file_exists(File_ops) -> true
	; file_exists(File_bak) -> delete_file1(File_bak)
	; true).

delete_file1(File):-
	(file_exists(File)->
	 delete_file(File)
	;
	 true).

% add_term_to_file(Term,File,FilePerms) adds the term Term to a file
% File (if File is created, FilePerms are applied)
add_term_to_file(Term, File, FilePerms) :-
%jcf-begin
	( file_exists(File) ->
	  true
	; create(File, FilePerms) % just to put right permissions in File.
	),
%jcf-end
        current_output(OldOutput),
        lock_file(File, FD, _),
        open(File,append,Stream),
        set_output(Stream),
        display_term(Term),
        close(Stream),
        unlock_file(FD, _),
        set_output(OldOutput).

get_pred_files(Dir,DirPerms, Name, Arity, File, File_ops, File_bak):-
        add_final_slash(Dir, DIR),
        atom_codes(Name, NameString),
        append(Module,":"||PredName,NameString),
        atom_codes(Mod, Module),
        atom_concat(DIR, Mod, DirMod),
        create_dir(DirMod,DirPerms),
        number_codes(Arity, AS),
        append("/"||PredName, "_"||AS, FilePrefS),
        atom_codes(FilePref, FilePrefS),
        atom_concat(DirMod, FilePref, PathName),
        atom_concat(PathName, '.pl', File),
        atom_concat(PathName, '_ops.pl', File_ops),
        atom_concat(PathName, '_bak.pl', File_bak).

create_dir(Dir,DirPerms) :-
        file_exists(Dir), !,  % Assuming it's a directory
	( 
	    DirPerms = default -> 
	    true
	; 
%% commented out because chmod/2 prints an awful message when
%% it fails (e.g., when the user running this is not the owner of
%% the directory). 
%	    umask(OldUMask,0),
%	    ( chmod(Dir,DirPerms) -> true ; true ), % always succeeds
%	    umask(_,OldUMask),
	    true
	).
create_dir(Dir,DirPerms) :-
%        mkpath(Dir).
	% TODO: Why? document
	working_directory(CurrDir,CurrDir),
	absolute_file_name(Dir,'','',CurrDir,AbsDir,_,_),
	( DirPerms = default -> 
	    mkpath(AbsDir)
	; mkpath(AbsDir),
	  umask(OldUMask,0),
	  chmod(AbsDir,DirPerms),
%	  mkpath_mode(Dir, DirPerms),
	  umask(_,OldUMask)
	). 

add_final_slash(Dir, DIR) :-
        atom_concat(_,'/',Dir) -> DIR = Dir ; atom_concat(Dir, '/', DIR).

:- prop keyword(X) + regtype 
# "@var{X} is an atom corresponding to a directory identifier.".

keyword(X) :- callable(X).
