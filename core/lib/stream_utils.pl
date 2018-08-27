:- module(stream_utils, [
	  read_to_end/2,
	  read_to_end/3,
	  get_line/2, get_line/1, line/1,
	  write_string/2, write_string/1,
	  %
	  file_to_string/2,
	  file_to_string/3,
	  string_to_file/2,
	  %
	  output_to_file/2],
        [assertions,isomodes,hiord]).

:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic)).
:- use_module(library(streams)). % TODO: merge here

:- doc(title, "Stream utilities").

:- doc(author,"The Ciao Development Team").

:- doc(module,"This module implements a collection of predicates to
   read/write streams (or files) from/to several sources (lists of
   terms, strings, predicate output, etc.).").

% ===========================================================================
:- doc(section, "Reading/writting from/to streams").

:- pred read_to_end(+Stream, -String) : stream(Stream) => string(String)
# "Reads in @var{String} all the characters from @var{Stream} until an
   EOF is found.".

read_to_end(Stream, String) :-
	read_to_end(Stream, String, []).

:- pred read_to_end(+Stream, -String, ?Tail): stream(Stream)
# "Reads in the difference list @var{String}-@var{Tail} all the
   characters from @var{Stream} until an EOF is found.".

read_to_end(Stream, String, Tail) :-
        current_input(OldIn),
        set_input(Stream),
        read_to_end_(String, Tail),
        set_input(OldIn).

read_to_end_(L, T) :-
        get_code(C),
        read_to_end_1(C, L, T).

read_to_end_1(-1, T, T) :- !.
read_to_end_1(C, [C|L], T) :-
        get_code(C1),
        read_to_end_1(C1, L, T).

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
        read_to_end(Stream, String, Tail),
	close(Stream).

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

