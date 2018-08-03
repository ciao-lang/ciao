:- module(strings,
        [whitespace/2, whitespace0/2,
         string/3
        ],
        [dcg,assertions,isomodes]).

:- doc(title, "String processing").
:- doc(author, "Daniel Cabeza").

:- doc(module, "This module provides some string-related predicates,
   specially for including in grammars defining strings.").

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
