:- module(streams_utils, [
	  stream_to_string/2,
	  stream_to_string/3,
	  get_line/2, get_line/1, line/1,
	  write_string/2, write_string/1,
	  %
	  file_to_terms/2,
	  terms_to_file/2,
	  file_to_string/2,
	  file_to_string/3,
	  string_to_file/2,
	  output_to_file/2],
        [assertions,isomodes]).

:- use_module(library(read), [read/1]).
:- use_module(library(streams)).

:- doc(title, "Stream utilities").

:- doc(author,"The Ciao Development Team").

:- doc(module,"This module implements a collection of predicates to
   read/write streams (or files) from/to several sources (lists of
   terms, strings, predicate output, etc.).").

% ===========================================================================
:- doc(section, "Reading/writting from/to streams").

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

% ---------------------------------------------------------------------------

:- doc(get_line(Stream, Line), "Reads from @var{Stream} a line of text
   and unifies @var{Line} with it.  The end of the line can have Unix
   @tt{[10]} or Windows/DOS @tt{[13, 10]} termination, which is not
   included in @var{Line}.  At EOF, the term @tt{end_of_file} is
   returned.").

:- pred get_line(S,L)
         : stream(S)
        => line(L).

get_line(Stream, Line) :-
        current_input(OldIn),
        set_input(Stream),
        get_line(Line),
        set_input(OldIn).

:- doc(get_line(Line), "Behaves like @tt{current_input(S),
   get_line(S,Line)}.").

:- pred get_line(L) => line(L).

get_line(Line) :-
        get_code(C),
        ( C = -1 -> Line = end_of_file
        ; get_line_after(C, Cs),
          Line = Cs
        ).

get_line_after(-1,[]) :- !, % EOF
        current_input(S), clearerr(S).
get_line_after(10,[]) :- !. % Newline
get_line_after(13, R) :- !, % Return, delete if at end of line
        get_code(C),
        get_line_after(C, Cs),
        ( Cs = [] ->
              R = []
        ; R = [13|Cs]
        ).
get_line_after(C, [C|Cs]) :-
        get_code(C1),
        get_line_after(C1, Cs).

:- doc(doinclude,line/1).

:- prop line/1 + regtype.

line(L) :- string(L).
line(end_of_file).

% ---------------------------------------------------------------------------

:- doc(write_string(Stream, String), "Writes @var{String} onto
   @var{Stream}.").

:- pred write_string(Stream,String)
         : ( stream(Stream), string(String) ).

write_string(Stream, S) :-
        current_output(OldOut),
        set_output(Stream),
        write_string(S),
        set_output(OldOut).

:- doc(write_string(String), "Behaves like @tt{current_input(S),
   write_string(S, String)}.").

:- pred write_string(String)
         : string(String).

write_string(V) :- var(V), !,
        throw(error(instantiation_error,write_string/1-1)).
write_string([]).
write_string([C|Cs]) :- put_code(C), write_string(Cs).

% ===========================================================================
:- doc(section, "Reading/writting from/to files").

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

% ---------------------------------------------------------------------------

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

% ---------------------------------------------------------------------------

:- pred string_to_file(+String, +FileName): (string(String), sourcename(FileName))
   # "Reads all the characters from the string @var{String} and writes
    them to file @var{FileName}.".

string_to_file(String, File) :-
	open(File, write, Stream),
	write_string(Stream, String),
	close(Stream).

% ---------------------------------------------------------------------------

:- meta_predicate output_to_file(goal, ?).
output_to_file(Goal, File) :-
	open(File, write, OS),
	current_output(CO),
	set_output(OS),
	call(Goal),
	set_output(CO),
	close(OS).

