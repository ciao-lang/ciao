:- module(modblobs, [], [assertions, regtypes]).

:- doc(title, "Modules as blobs").
:- doc(author, "Isabel Garcia Contreras").
:- doc(author, "Jose F. Morales").

% TODO: make it similar to typeslib? (identifiers to global objects)
% TODO: new_modblob, delete_modblob, create new atom?
% TODO: modblob_file (get file)
% TODO: allow .c, .java, ... extensions?
% TODO: modblob_read_terms? (MERGE with string_to_term and enable_deepfind_syntax)

:- doc(module, "This module implements (temporary) modules-as-blobs
   facilities.  They allow the runtime creation of modules from
   strings and terms.

   @section{Implementation}

   Run-time creation of modules is currently implemented by writting
   the module code into a unique temporary directory (using the system
   @pred{mktemp_in_tmp/2} predicate).

   Currently they need to be deleted explicitly.

   Note that this is a temporary solution before read/write stream
   operations can be work on in-memory objects.

   @section{Examples}

   The predicate @pred{new_modblob/4} creates a new temporary module, that
   can be deleted with @pred{delete_modblob/1}. 

   Module creation requires the module name, export list, program
   clauses, and returns the module object (currently a path). If a
   @tt{module} directive is specified, the export list is ignored.

   Example:

@begin{verbatim}
?- new_modblob([(p(A) :- q(A)), (q(b))], [p/1], my_tmp_mod, ModBlob), modblob_path(ModBlob, Path).
Path = '/tmp/tmp_modsfbMY9Y/my_tmp_mod.pl' ?
@end{verbatim}

   where the contets of @tt{/tmp/tmp_modsfbMY9Y/my_tmp_mod.pl} are:

@begin{verbatim}
:-module(_,[p/1],[]).
p(A) :-
        q(A).
q(b).
@end{verbatim}
").

% ---------------------------------------------------------------------------

:- use_module(library(system), [mktemp_in_tmp/2, make_directory/1]).
:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(library(write), [portray_clause/2]).
:- use_module(library(pathnames), [path_concat/3, path_splitext/3]).

% These facts are used to keep the name of the files (without
% extension (.pl, .po, .itf, ...) that were created
:- data modblob_def/2.
:- data modblob_tmp_dir/1.

get_tmp_subdir(X) :-
	( modblob_tmp_dir(X) ->
	    true
	; mktemp_in_tmp('modblob_XXXXXX', FileBase),
	  del_file_nofail(FileBase),
	  make_directory(FileBase),
	  X = FileBase,
	  set_fact(modblob_tmp_dir(X))
	).

:- regtype modblob(X) # "@var{X} is a is a temporary module object".
modblob(X) :- atm(X).

:- export(new_modblob/4).
:- pred new_modblob(Clauses, ExportedPreds, ModName, ModBlob)
	: list * list * atm * var => list * list * atm * modblob
   # "Write clauses @var{Clauses} in a temporary module identified by @var{ModBlob}.".
new_modblob([FirstClause|Clauses], ExportedPreds, ModName, ModBlob) :-
	create_tmp_file_name(ModName, ModPath),
	atom_concat(ModPath, '.pl', Filename),
	ModBlob = Filename, % TODO: temporarily, the same
	% (assume this is new)
	( modblob_def(ModPath, _) ->
	    throw(bug_conflict_modblob(ModPath))
	; true
	),
	assertz_fact(modblob_def(ModBlob, Filename)),
	%
	open(Filename, write, S),
	%
	( module_clause(FirstClause) ->
	  portray_clause(S, FirstClause),
	  RestClauses = Clauses
	  ;
	    portray_clause(S, (:- module(_,ExportedPreds,[]))),
	    RestClauses = [FirstClause|Clauses]
	),
	write_clauses(S, RestClauses),
	close(S).

module_clause(Clause) :-
	nonvar(Clause),
	Clause = ':-'(X),
	functor(X, module, _).

:- pred create_tmp_file_name(+ModName, -FilePath)
	#"A file @var{ModName}, will be created, if there already
         exists one, it will overwrite its content".
create_tmp_file_name(FileName, FilePath) :-
	get_tmp_subdir(Dir),
	path_concat(Dir, FileName, FilePath).

:- pred write_clauses(Stream, Clauses) : atm * list.
write_clauses(Stream, [C|Cs]) :-
	portray_clause(Stream, C),
	write_clauses(Stream, Cs).
write_clauses(_, []).

:- export(delete_modblob/1).
:- pred delete_modblob(ModBlob) : modblob(ModBlob)
   # "Removes the data associated to @var{ModBlob}".
% NOTE: It cleans only itf file because when performing search no more
%   auxiliary files are created.
delete_modblob(ModBlob) :-
	% check_modblob(ModBlob), % (done by modblob_path/2)
	modblob_path(ModBlob, ModPath),
	retract_fact(modblob_def(ModBlob, _)),
	del_file_nofail(ModPath),
	path_splitext(ModPath, ModBase, '.pl'),
	atom_concat(ModBase, '.itf', ModITF),
	del_file_nofail(ModITF).

:- export(modblob_path/2).
:- pred modblob_path(ModBlob, ModPath) : atm(ModBlob) => atm(ModPath)
   # "Obtain the temporary file associated to @var{ModBlob}".
modblob_path(ModBlob, ModPath) :-
	check_modblob(ModBlob),
	ModPath = ModBlob.
	
check_modblob(ModBlob) :-
	nonvar(ModBlob),
	modblob_def(ModBlob, _), !,
	true.
check_modblob(ModBlob) :-
	throw(error(not_a_modblob(ModBlob), check_modblob/1)).
	
	

