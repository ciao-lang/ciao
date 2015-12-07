:- module(symfnames,[open/3],[assertions,isomodes]).

:- use_module(library(read), [read/2]).
:- use_module(library(system), [getenvstr/2]).

%% --------------------------------------------------------------------
:- doc(title, "Symbolic filenames").
:- doc(author, "Francisco Bueno").
:- doc(module,"This module provides a predicate for file opening
   which can use any term as an alias for the filename (i.e., symbolic
   filenames) instead of the usual constants which are file system path
   names of the actual files.

   The correspondence between an alias and the actual file path is done
   dynamically, without having to recompile the program. It is possible
   to define the correspondence via facts for @pred{file_alias/2}
   in a file declared with @pred{multifile:alias_file/1} in the program:
   those facts will be dynamically loaded when running the program.
   Alternatively, the correspondence can be defined via shell environment 
   variables, by defining the value of a variable by the (symbolic) name 
   of the file to be the path of the actual file.").
:- doc(appendix,"@include{symfnames/examples.lpdoc}").

%% --------------------------------------------------------------------
:- initialization(load_aliases).

:- multifile alias_file/1.
:- doc(alias_file(File), "Declares @var{File} to be a file defining
   symbolic names via @pred{file_alias/2}. Anything else in @var{File}
   is simply ignored.").

load_aliases:-
	retractall_fact(file_alias(_,_)),
	alias_file(File),
	absolute_file_name(File,AbsFile),
	streams_basic:open(AbsFile,read,S),
	load_it(S),
	close(S),
	fail.
load_aliases.

load_it(S):-
	repeat,
	  read(S,X),
	  ( X==end_of_file, !
	  ; X=file_alias(_,_),
	    assertz_fact(X),
	    fail
	  ).

:- multifile file_alias/2.
:- data file_alias/2.
:- doc(file_alias(Alias,File), "Declares @var{Alias} as a symbolic
   name for @var{File}, the real name of an actual file (or directory).").

:- doc(open(File, Mode, Stream), "Open @var{File} with mode
   @var{Mode} and return in @var{Stream} the stream associated with the
   file. It is like @pred{streams_basic:open/3}, but @var{File} is considered
   a symbolic name: either defined by @pred{user:file_alias/2} or as an 
   environment variable. Predicate @pred{user:file_alias/2} is inspected
   before the environment variables.").

:- true pred open(+term, +io_mode, ?stream).

open(SymFileName,Mode,Stream):-
	file_alias(SymFileName,FileName), !,
	streams_basic:open(FileName,Mode,Stream).
open(SymFileName,Mode,Stream):-
	getenvstr(SymFileName,FileName0),
	atom_codes(FileName,FileName0), !,
	streams_basic:open(FileName,Mode,Stream).
open(SymFileName,_Mode,_Stream):-
	throw(error(unknown_symbolic_name(SymFileName),'symfnames:open'/3)).
