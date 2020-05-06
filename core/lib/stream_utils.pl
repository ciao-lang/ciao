:- module(stream_utils, [
      get_line/2, get_line/1, line/1,
      read_string_to_end/2,
      read_bytes_to_end/2, discard_to_end/1, read_bytes/3,
      copy_stream/3,
      write_string/2, write_string/1,
      write_bytes/2, write_bytes/1,
      %
      file_to_string/2, string_to_file/2,
      file_to_bytes/2, bytes_to_file/2,
      %
      output_to_file/2,
      %
      open_input/2, close_input/1,
      open_output/2, close_output/1
    ],
    [assertions,isomodes,hiord]).

:- doc(title, "Stream utilities").
:- doc(author,"The Ciao Development Team").

:- doc(module,"This module implements a collection of predicates to
   read/write streams (or files) from/to several sources (lists of
   terms, strings, predicate output, etc.).").

:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic)).

% ===========================================================================
:- doc(section, "Reading/writing from/to streams").

:- pred get_line(S,L) : stream(S) => line(L)
   # "Reads from @var{Stream} a line of text and unifies @var{Line}
   with it.  The end of the line can have Unix @tt{[10]} or
   Windows/DOS @tt{[13, 10]} termination, which is not included in
   @var{Line}.  At EOF, the term @tt{end_of_file} is returned.".

get_line(Stream, Line) :-
    current_input(OldIn),
    set_input(Stream),
    get_line(Line),
    set_input(OldIn).

:- pred get_line(L) => line(L)
   # "Behaves like @tt{current_input(S), get_line(S,Line)}.".

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

:- pred read_string_to_end(Stream,String) : stream(Stream) => string(String)
   # "Reads in @var{String} all the characters from @var{Stream} until
   an EOF is found.".

read_string_to_end(Stream, String) :-
    current_input(OldIn),
    set_input(Stream),
    read_string_to_end_(String),
    set_input(OldIn).

read_string_to_end_(L) :- get_code(C), read_string_to_end__(C, L).

read_string_to_end__(-1, []) :- !.
read_string_to_end__(C, [C|L]) :- read_string_to_end_(L).

:- pred read_bytes_to_end(Stream,Bytes) : stream(Stream) => bytelist(Bytes)
   # "Reads in @var{Bytes} all the bytes from @var{Stream} until
   an EOF is found.".

read_bytes_to_end(Stream, Bytes) :-
    current_input(OldIn),
    set_input(Stream),
    read_bytes_to_end_(Bytes),
    set_input(OldIn).

read_bytes_to_end_(L) :- get_byte(C), read_bytes_to_end__(C, L).

read_bytes_to_end__(-1, []) :- !.
read_bytes_to_end__(C, [C|L]) :- read_bytes_to_end_(L).

:- pred discard_to_end(Stream) : stream(Stream)
   # "Reads in all the bytes from @var{Stream} until an EOF is found.".

discard_to_end(Stream) :-
    current_input(OldIn),
    set_input(Stream),
    discard_to_end_,
    set_input(OldIn).

discard_to_end_ :- get_byte(C), discard_to_end__(C).

discard_to_end__(-1) :- !.
discard_to_end__(_) :- discard_to_end_.

:- pred read_bytes(Stream,N,Bytes) : (stream(Stream), int(N)) => bytelist(Bytes)
   # "Reads in @var{Bytes} at most @var{N} bytes from @var{Stream}, or
   until an EOF is found.".

read_bytes(Stream, N, Bytes) :-
    current_input(OldIn),
    set_input(Stream),
    read_bytes_(N, Bytes),
    set_input(OldIn).

read_bytes_(N, []) :- N =< 0, !.
read_bytes_(N, L) :- get_byte(C), N1 is N-1, read_bytes__(C, N1, L).

read_bytes__(-1, _, []) :- !.
read_bytes__(C, N, [C|L]) :- read_bytes_(N, L).

% TODO: implement in C
:- pred copy_stream(InS, OutS, Copied)
   : (stream(InS), stream(OutS)) => int(Copied)
   # "Copies all bytes bytes (until EOF or error) from the @var{InS}
   stream into the @var{OutS} stream. The number of copied bytes is
   returned in @var{Copied}".

copy_stream(InS, OutS, Copied) :-
    copy_stream_(InS, OutS, 0, Copied).

copy_stream_(InS, OutS, Copied0, Copied) :-
    get_byte(InS, C),
    ( C = -1 -> Copied = Copied0
    ; Copied1 is Copied0 + 1,
      put_byte(OutS, C),
      copy_stream_(InS, OutS, Copied1, Copied)
    ).

% ---------------------------------------------------------------------------

:- pred write_string(Stream,String): (stream(Stream), string(String)) 
   # "Writes @var{String} onto @var{Stream}.".

write_string(Stream, S) :-
    current_output(OldOut),
    set_output(Stream),
    write_string(S),
    set_output(OldOut).

:- pred write_string(String): string(String)
   # "Behaves like @tt{current_input(S), write_string(S, String)}.".

write_string(V) :- var(V), !, throw(error(instantiation_error,write_string/1-1)).
write_string([]).
write_string([C|Cs]) :- put_code(C), write_string(Cs).

:- pred write_bytes(Stream,Bytes): (stream(Stream), bytelist(Bytes)) 
   # "Writes @var{Bytes} onto @var{Stream}.".

write_bytes(Stream, S) :-
    current_output(OldOut),
    set_output(Stream),
    write_bytes(S),
    set_output(OldOut).

:- pred write_bytes(Bytes): bytelist(Bytes)
   # "Behaves like @tt{current_input(S), write_bytes(S, Bytes)}.".

write_bytes(V) :- var(V), !, throw(error(instantiation_error,write_bytes/1-1)).
write_bytes([]).
write_bytes([C|Cs]) :- put_byte(C), write_bytes(Cs).

% ===========================================================================
:- doc(section, "Reading/writing from/to files").

:- pred file_to_string(FileName, String) : sourcename(FileName) => string(String)
   # "Reads all the characters from the file @var{FileName} and
   returns them in @var{String}.".

file_to_string(File, String) :-
    open(File, read, Stream),
    read_string_to_end(Stream, String),
    close(Stream).

:- pred string_to_file(String, FileName): (string(String), sourcename(FileName))
   # "Reads all the characters from the string @var{String} and writes
   them to file @var{FileName}.".

string_to_file(String, File) :-
    open(File, write, Stream),
    write_string(Stream, String),
    close(Stream).

:- pred file_to_bytes(FileName, Bytes) : sourcename(FileName) => bytelist(Bytes)
   # "Reads all the bytes from the file @var{FileName} and returns
   them in @var{Bytes}.".

file_to_bytes(File, Bytes) :-
    open(File, read, Stream),
    read_bytes_to_end(Stream, Bytes),
    close(Stream).

:- pred bytes_to_file(Bytes, FileName): (bytelist(Bytes), sourcename(FileName))
   # "Reads all the bytes from the bytes @var{Bytes} and writes
   them to file @var{FileName}.".

bytes_to_file(Bytes, File) :-
    open(File, write, Stream),
    write_bytes(Stream, Bytes),
    close(Stream).

% ===========================================================================
:- doc(section, "Redirect output to files").

:- meta_predicate output_to_file(goal, ?).
output_to_file(Goal, File) :-
    open(File, write, OS),
    current_output(CO),
    set_output(OS),
    call(Goal), % TODO: use port_reify
    set_output(CO),
    close(OS).

% ===========================================================================
:- doc(section, "Structured stream handling").

:- pred open_input(FileName,InputStreams)
     : sourcename(FileName)
    => input_handler(InputStreams).

open_input(FileName, i(OldInput, NewInput)) :-
    current_input(OldInput),
    open(FileName, read, NewInput),
    set_input(NewInput).

:- pred close_input(InputStreams)
     : input_handler(InputStreams)
    => input_handler(InputStreams).

close_input(i(OldInput, NewInput)) :- !,
    set_input(OldInput),
    close(NewInput).
close_input(X) :-
    throw(error(domain_error(open_input_handler, X), close_input/1-1)).

:- pred open_output(FileName,OutputStreams)
     : sourcename(FileName)
    => output_handler(OutputStreams).

open_output(FileName, o(OldOutput, NewOutput)) :-
    current_output(OldOutput),
    open(FileName, write, NewOutput),
    set_output(NewOutput).

:- pred close_output(OutputStreams)
     : output_handler(OutputStreams)
    => output_handler(OutputStreams).

close_output(o(OldOutput, NewOutput)) :- !,
    set_output(OldOutput),
    close(NewOutput).
close_output(X) :-
    throw(error(domain_error(open_output_handler, X), close_output/1-1)).

:- prop input_handler/1 + regtype.

input_handler(i(Old,New)) :-
    stream(Old),
    stream(New).

:- prop output_handler/1 + regtype.

output_handler(o(Old,New)) :-
    stream(Old),
    stream(New).


