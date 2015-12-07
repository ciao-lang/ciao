:- module(checkline, [main/1], [assertions, fsyntax]).

:- use_module(library(format)).
:- use_module(library(file_utils)).
:- use_module(library(lists)).
:- use_module(library(checklines)).

:- doc(title, "Line Length Checker").

:- doc(author, "Edison Mera").

:- doc(module, "This program helps to verify that a file does not
	have lines with a length higher than a given number of
	characters.

@begin{verbatim}
@includefact{usage_message/1}
@end{verbatim}

@subsection{Related Tools}

The @tt{whitespace-mode} for @apl{emacs} offers a similar
functionality.
").

usage_message :=
	"Usage: checkline [-l Chars] -|FileName \n"||
	"Chars is the max number of characters. By default 80.\n"||
	"FileName is the file name to verify.\n"||
	"\'-\' Is used to indicate that the standard input will be used.\n".

% ---------------------------------------------------------------------------

main(Args) :-
	process_args(Args, Params),
	do_process(Params).

process_args(['-h'|Args], Params) :-
	show_help,
	process_args(Args, Params).

process_args(['-l', ALength|Args], params(FileName, Length)) :-
	atom_codes(ALength, SLength),
	number_codes(Length, SLength),
	process_args(Args, params(FileName, Length)).

process_args([FileName|Args], params(FileName, Length)) :-
	process_args(Args, params(FileName, Length)).

process_args([], params(_FileName, Length)) :-
	set_default_length(Length).

set_default_length(80) :- !.
set_default_length(_).

do_process(params(FileName, Length)) :-
	( var(FileName) ->
	    show_help
	;
	    (
		FileName == '-' ->
		current_input(CI),
		stream_to_string(CI, String)
	    ;
		file_to_string(FileName, String)
	    ),
	    checklines_string(FileName, String, Length)
	).

show_warnings_lines_too_long([]).
show_warnings_lines_too_long([L|Ls]) :-
	format("WARNING: (lns ~w-~w) line too long.\n", [L, L]),
	show_warnings_lines_too_long(Ls).

show_help :-
	display_string(~usage_message).

