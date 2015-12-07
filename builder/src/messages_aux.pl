:- module(messages_aux, [
	cmd_message/3,
	normal_message/2,
	verbose_message/2
	], [assertions, regtypes, hiord]).

:- doc(title, "Messages for the build process").

:- use_module(library(format), [format/3]).
:- use_module(library(lists), [append/3]).
:- use_module(library(messages)).

:- use_module(ciaobld(config_common), [verbose_build/1]).

cmd_message(Target, Mess, Args) :-
	format(user_output, "=> ~w: ", [Target]),
	format(user_output, Mess, Args),
	format(user_output, "~n",  []).

normal_message(Mess, Args) :-
	prefix_lines(Mess, "   ", NMess),
	format(user_output, NMess, Args),
	format(user_output, "~n",  []).

verbose_message(Mess, Args) :- verbose_build(yes), !,
	prefix_lines(Mess, "?? ", NMess),
	format(user_output, NMess, Args),
	format(user_output, "~n",  []).
verbose_message(_Mess, _Args).

% Add @var{Prefix} to each line in @var{String0}, put result in @var{String}
prefix_lines(String0, Prefix, String) :-
	append(Prefix, String1, String),
	prefix_lines_(String0, Prefix, String1).

prefix_lines_([],       _,      []).
prefix_lines_([0'\n|R], Prefix, NR) :- !,
	NR = [0'\n|NR1],
	append(Prefix, NR0, NR1),
	prefix_lines_(R, Prefix, NR0).
prefix_lines_([C|R], Prefix, [C|NR]) :- !,
	prefix_lines_(R, Prefix, NR).

