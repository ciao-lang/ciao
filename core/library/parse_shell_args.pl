:- module(_, [], [assertions, dcg, regtypes, isomodes]).

:- doc(title, "Shell-style argument parsing").

:- doc(author, "Jose F. Morales").

:- doc(module, "This module implements shell-style argument
   parsing. This is useful for parsing argument strings from other
   tools (e.g., those returned by @tt{pkg-config}) and using them in
   calls to @pred{process_call/3} (without using a full shell
   interpreter).

   It offers a @pred{parse_shell_args/2} predicate that splits a
   single input into a list of arguments, following these rules:

   @begin{itemize}
   @item arguments are separated by blanks (whitespaces or tabulators)
   @item arguments may contain double and single quoted substrings
     (quotes are removed)
   @item blanks are not separators when they appear in quoted strings
   @item escape character removes special meaning from blank or quote
     characters
   @end{itemize}

   Note that this library is not intended to do full shell parsing
   (command or variable substitutions, etc.).
").

:- export(parse_shell_args/2).
:- pred parse_shell_args(+Atm, -Args) :: atm * list(atm) 
   # "Parse shell-style arguments from atom @var{Atm} into @var{Args}".

parse_shell_args(Atm, Args) :-
	atom_codes(Atm, Str),
	blanks_and_args(Args, Str, []).

blanks_and_args(Args) -->
	blanks,
	( empty -> { Args = [] }
	; args(nquoted, Args)
	).

args(Kind, Args) -->
	parse_arg(Kind, Ws),
	{ atom_codes(X, Ws), Args = [X|Args0] },
	blanks_and_args(Args0).

parse_arg(Kind, []) --> stop(Kind), !. % stop
parse_arg(Kind, Ws) --> switch(Kind, Kind2), !, % switch Kind
	parse_arg(Kind2, Ws).
parse_arg(Kind, [C|Ws]) -->
	pick_char(Kind, C),
	parse_arg(Kind, Ws).

pick_char(Kind, C) -->
	( [0'\\, C], { escaped(Kind, C) } -> % escaped character
	    []
	; [C] % any char
	).

stop(_) --> empty, !.
stop(nquoted, Cs, Cs) :- Cs = [C|_], is_blank(C), !.

switch(nquoted, dquoted) --> "\"", !.
switch(nquoted, squoted) --> "\'", !.
switch(squoted, nquoted) --> "\'", !.
switch(dquoted, nquoted) --> "\"", !.

escaped(nquoted, _) :- !.
escaped(dquoted, 0'\") :- !.
escaped(dquoted, 0'\\) :- !.

empty([], []).

blanks --> [X], { is_blank(X) }, !, blanks.
blanks --> [].

is_blank(0'\t).
is_blank(0' ).

:- test parse_shell_args(Atm, Args) : (
    Atm = 'abc bcd efg'
   ) => (
    Args = ['abc', 'bcd', 'efg']
   ) # "Simple argument parsing".

:- test parse_shell_args(Atm, Args) : (
    Atm = '   \'fo"o\' a\'fo"o\' a\'fo"o\'"bar" "fo\\"o" -I/Users/foo/bar-1.0 -I/some/dir\\ with\\ blanks -O2 -W -Wall,-I/usr/local/include'
   ) => (
    Args = ['fo"o','afo"o','afo"obar','fo"o','-I/Users/foo/bar-1.0','-I/some/dir with blanks','-O2','-W','-Wall,-I/usr/local/include']
   ) # "Complex argument parsing".

