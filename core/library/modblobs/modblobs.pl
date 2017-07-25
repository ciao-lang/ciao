:- module(modblobs, [tmp_mod/4, clean_tmp_mod/1], [assertions]).

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

   The predicate @pred{tmp_mod/4} creates a new temporary module, that
   can be deleted with @pred{clean_tmp_mod/1}. 

   Module creation requires the module name, export list, program
   clauses, and returns the module object (currently a path). If a
   @tt{module} directive is specified, the export list is ignored.

   Example:

@begin{verbatim}
?- tmp_mod([(p(A) :- q(A)), (q(b))], [p/1], my_tmp_mod, Path).
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

:- pred tmp_mod(Clauses, ExportedPreds, Name, ModPath)
	: list * list * atm * var => list * list * atm * atm
    #"This predicate writes a list of clauses in a file with @var{name}
     in a randomly generated folder of /tmp.".
tmp_mod([FirstClause|Clauses], ExportedPreds, Name, Filename) :-
	create_tmp_file_name(Name, FilePath),
	assertz_fact(modblob_def(FilePath, Filename)),
	%
	atom_concat(FilePath, '.pl', Filename),
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

:- pred clean_tmp_mod(X) : atm(X)
	#"Removes a created tmp file and its associated files".
% NOTE: It cleans only itf file because when performing search no more
%   auxiliary files are created.
clean_tmp_mod(FilePL) :-
%	atom_concat(FileBase, '.pl', FilePL),
	del_file_nofail(FilePL),
	path_splitext(FilePL, FileBase, '.pl'),
	atom_concat(FileBase, '.itf', FileITF),
	del_file_nofail(FileITF).

