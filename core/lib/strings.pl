:- module(strings,
        [get_line/2, get_line/1, line/1,
         write_string/2, write_string/1,
         whitespace/2, whitespace0/2,
         string/3
        ],
        [dcg,assertions,isomodes]).
% Utilities taken out from html.pl

:- doc(title, "String processing").
:- doc(author, "Daniel Cabeza").

:- doc(module, "This module provides predicates for doing
   input/output with strings (character code lists) and for including in
   grammars defining strings.").

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

:- doc(whitespace(String, Rest), "In a grammar rule, as
   @tt{whitespace/0}, represents whitespace (a positive
   number of space (32), tab (9), newline (10) or return (13)
   characters).  Thus, @var{Rest} is a proper suffix of @var{String}
   with one or more whitespace characters removed.  An example of use
   would be:
@begin{verbatim}
   attrs([]) --> """"
   attrs([N|Ns]) -->
       whitespace,
       attr(N),
       attrs(Ns).
@end{verbatim}
").

:- pred whitespace(S1,S2)
         : string(S1)
        => string(S2).

whitespace --> whitespace_char, whitespace0.

:- doc(whitespace0(String, Rest), "In a grammar rule, as
   @tt{whitespace0/0}, represents possible whitespace (any number of
   space (32), tab (9), newline (10) or return (13) characters). Thus,
   @var{Rest} is @var{String} or a proper suffix of @var{String} with
   one or more whitespace characters removed.  An example of use would
   be:

@begin{verbatim}
   assignment(N,V) -->
       variable_name(N), whitespace0, ""="", whitespace0, value(V).
@end{verbatim}
").


:- pred whitespace0(S1,S2)
         : string(S1)
        => string(S2).

whitespace0 --> whitespace_char, whitespace0.
whitespace0 --> [].

whitespace_char --> [10]. % newline
whitespace_char --> [13]. % return
whitespace_char --> [32]. % space
whitespace_char --> [9].  % tab

:- doc(string(String, Head, Tail), "In a @concept{grammar rule}, as
   @tt{string/1}, represents literally @var{String}.  An example of use
   would be:

@begin{verbatim}
double(A) -->
        string(A),
        string(A).
@end{verbatim}
").


:- pred string(?string,?string,?string).

:- pred string(A,B,C)
         : list(C)
        => ( list(A), list(B) ).

string([]) --> "".
string([C|Cs]) -->
        [C],
        string(Cs).
