:- module(plindent, [main/1], [assertions]).

:- use_package(plindent(plindent_decl)).

:- use_module(library(strings),  [write_string/1]).
:- use_module(library(messages), [show_message/3]).
:- use_module(library(file_utils)).

:- use_module(plindent(autoindent)).

:- doc(title,  "Automatic Source Indenter").
:- doc(author, "Edison Mera").

:- doc(module, "This module implements a command-line interface to the
	Prolog automatic indenter.

@begin{verbatim}
@includefact{usage_message/1}
@end{verbatim}
").

usage_message("
Automatic indenter of prolog files.

Usage:
	plindent [[Input] [-|Output]]|[--help]

	Where Input is the input file name and output the output file
	name.  '-' means to use the standard output.  Call it without
	arguments to use the standard input and standard output.

	--help shows this information.

	-v is verbose mode.
").

process_args(['--help']) :-
	show_help.
process_args(['-v'|Args]) :-
	display(Args),
	nl,
	process_args(Args).
process_args([Source, '-']) :-
	!,
	file_to_string(Source, SourceS),
	plindent(Source, SourceS, Target),
	write_string(Target).
process_args([Source, Target]) :-
	!,
	plindent_file(Source, Target).
process_args([Source]) :-
	!,
	plindent_file(Source).
process_args([]) :-
	!,
	current_input(CI),
	stream_to_string(CI, SourceS),
	plindent(_Source, SourceS, Target),
	write_string(Target).

show_help :-
	usage_message(M),
	display_string(M).

main(Args) :-
	process_args(Args),
	!.
main(Args) :-
	show_message(error, "Unable to process ~w", [Args]).
