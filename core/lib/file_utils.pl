:- module(file_utils, [
	  file_to_terms/2,
	  terms_to_file/2,
	  file_to_string/2,
	  file_to_string/3,
	  string_to_file/2,
	  stream_to_string/2,
	  stream_to_string/3,
	  output_to_file/2],
        [assertions,isomodes]).

:- use_module(library(read), [read/1]).
:- use_module(library(streams)).
:- use_module(library(strings)).

% TODO: Integrate with other stream module? 
:- doc(title,"File/Stream Utilities").

:- doc(author,"The CLIP Group").

:- doc(module,"This module implements a collection of predicates to
   read/write files (or streams) from/to several sources (lists of
   terms, strings, predicate output, etc.), in a compact way.").

% (pp) probably redundant
%:- pred file_terms(@File, ?Terms) : sourcename(File) =>  list(Terms) 
%   # "Transform a file @var{File} to/from a list of terms @var{Terms}.".


:- pred file_to_terms(File, Terms) :
	(sourcename(File), var(Terms)) => list(Terms)
   # "Unifies @var{Terms} with the list of all terms in @var{File}.".

file_to_terms(File, Terms) :-
        open_input(File, IO),
        read(T),
        read_terms(T, Terms),
        close_input(IO).

read_terms(end_of_file, []) :- !.
read_terms(T, [T|Ts]) :-
        read(T1),
        read_terms(T1, Ts).

:- pred terms_to_file(File, Terms) : sourcename * list
   # "Writes the terms in list @var{Terms} (including the ending '.')
      onto file @var{File}.".

terms_to_file(Terms, File) :-
	check_is_list(Terms, term_to_file/2-1), !,
        open_output(File, IO),
        display_term_list(Terms),
        close_output(IO).

display_term_list([]).
display_term_list([T|Ts]) :-
        display_term(T),
        display_term_list(Ts).

check_is_list(X, Loc):-
	var(X),
	throw(error(instantiation_error(X), Loc)).
check_is_list([], _Loc).
check_is_list([_|T], Loc):-
	check_is_list(T, Loc).
check_is_list(X, Loc):-
	throw(error(type_error(list, X), Loc)).

:- pred file_to_string(+FileName, -String) : sourcename(FileName) => string(String)
   # "Reads all the characters from the file @var{FileName}
      and returns them in @var{String}.".


file_to_string(File, String) :-
	file_to_string(File, String, []).


:- pred file_to_string(+FileName, -String, ?Tail) : sourcename(FileName) =>
   string(String) # "Reads all the characters from the file
   @var{FileName} and returns them in @var{String}.  @var{Tail} is the
   end of @var{String}.".


file_to_string(File, String, Tail) :-
        open(File, read, Stream),
        stream_to_string(Stream, String, Tail).

:- pred string_to_file(+String, +FileName): (string(String), sourcename(FileName))
   # "Reads all the characters from the string @var{String} and writes
    them to file @var{FileName}.".

string_to_file(String, File) :-
	open(File, write, Stream),
	write_string(Stream, String),
	close(Stream).


:- pred stream_to_string(+Stream, -String): stream(Stream) =>
   string(String) # "Reads all the characters from @var{Stream},
   returns them in @var{String}, and closes @var{Stream}.".

stream_to_string(Stream, String) :-
	stream_to_string(Stream, String, []).


:- pred stream_to_string(+Stream, -String, ?Tail): stream(Stream) #
   "Reads all the characters from @var{Stream}, returns them in
   @var{String}, and closes @var{Stream}.  @var{Tail} is the end of
   @var{String}".

stream_to_string(Stream, String, Tail) :-
        current_input(OldIn),
        set_input(Stream),
        read_to_close(String, Tail),
        set_input(OldIn),
        close(Stream).

read_to_close(L, T) :-
        get_code(C),
        read_to_close1(C, L, T).

read_to_close1(-1, T, T) :- !.
read_to_close1(C, [C|L], T) :-
        get_code(C1),
        read_to_close1(C1, L, T).

:- meta_predicate output_to_file(goal, ?).
output_to_file(Goal, File) :-
	open(File, write, OS),
	current_output(CO),
	set_output(OS),
	call(Goal),
	set_output(CO),
	close(OS).
