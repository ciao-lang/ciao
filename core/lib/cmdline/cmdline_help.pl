% Auxiliary definitions to define command line interfaces
% (included file)

% TODO: Extend this code to define declarative command line interfaces
%   E.g., 
%     https://wiki.haskell.org/Command_line_option_parsers
%     http://erratique.ch/software/cmdliner/doc/Cmdliner
%     etc.

% TODO: Merge with optparse package

% ---------------------------------------------------------------------------

:- discontiguous grp_def/2.
:- discontiguous grp_details/2.
:- discontiguous cmd_grp/2.
:- discontiguous cmd_usage/3.
:- discontiguous cmd_details/2.

% TODO: Use this version to turn this into a module
%
% :- meta_predicate cmdline_ctx(
% 	pred(2), % grp_def/2
% 	pred(2), % grp_details/2
% 	pred(2), % cmd_grp/2
% 	pred(3), % cmd_usage/2
% 	pred(2), % cmd_details/2
% 	?).
% cmdline_ctx(A, B, C, D, E, ''(A, B, C, D, E)).
% 
% cmdline_grp_def(    ''(P, _, _, _, _), X, Y) :- P(X,Y).
% cmdline_grp_details(''(_, P, _, _, _), X, Y) :- P(X,Y).
% cmdline_cmd_grp(    ''(_, _, P, _, _), X, Y) :- P(X,Y).
% cmdline_cmd_usage(  ''(_, _, _, P, _), X, Y, Z) :- P(X,Y,Z).
% cmdline_cmd_details(''(_, _, _, _, P), X, Y) :- P(X,Y).

% ---------------------------------------------------------------------------
:- doc(section, "Help modifiers").

:- data help_mode_/2.

set_help_mode(Level, Prof) :-
	retractall_fact(help_mode_(_, _)),
	assertz_fact(help_mode_(Level, Prof)).

help_mode(Level, Prof) :- help_mode_(Level, Prof).

all_or_details(all).
all_or_details(details).

advanced :- help_mode(Level, _), all_or_details(Level).

:- regtype help_level/1.
help_level(summary). % Just a summary of most useful commands
help_level(all).     % All commands
help_level(details). % Details about commands

% ---------------------------------------------------------------------------
:- doc(section, "Help message").

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(lists), [length/2]).
:- use_module(library(streams_utils), [write_string/1]).

:- export(show_help/2).
:- pred show_help(+Level, +Prof) :: help_level * atm
   # "Show help for the command line interface. @var{Level} specifies
      the detail level and @var{Prof} the command line profile (useful
      when the same command may act in different ways).".

show_help(Level, Prof) :- show_help_cmd_('', Level, Prof).

:- export(show_help_cmd/2).
:- pred show_help_cmd(Cmd, Prof) # "Show detailed help on the
   specified command @var{Cmd} (shows help of the command group)".

show_help_cmd(Cmd, Prof) :-
	show_help_cmd_(Cmd, details, Prof).

show_help_cmd_(Cmd, Level, Prof) :-
	set_help_mode(Level, Prof),
	help_str(Cmd, Str, []),
	write_string(Str).

% Format the help message as a string
help_str('') --> !,
	banner_help_str,
	%
	{ findall(Grp, grp_def(Grp, _), Grps) },
	grps_str(Grps),
	show_sep,
	ending_help_str.
help_str(Cmd) -->
	{ \+ Cmd = '--' }, % (reserved for help separators)
	{ cmd_grp(Cmd, Grp) },
	!,
	grps_str([Grp]),
	show_sep.
help_str(_) -->
	"No help for command\n".

grps_str(Grps) -->
	{ grps_items(Grps, Xs, []) },
	show_items(Xs).

% ---------------------------------------------------------------------------
:- doc(section, "Generate documentation items").

grps_items([]) --> [].
grps_items([Grp|Grps]) --> grp_items(Grp), grps_items(Grps).

grp_items(Grp) --> { grp_def(Grp, Name) }, !,
	[s(Name)],
	{ findall(Cmd, cmd_grp(Cmd, Grp), Cmds) },
	cmds_items(Cmds),
	( { help_mode(details, _), grp_details(Grp, Text) } ->
	    [sep, d(Text)]
	; []
	),
	[sep].
grp_items(_Grp) --> [].

cmds_items([]) --> [].
cmds_items([Cmd|Cmds]) --> cmd_items(Cmd), cmds_items(Cmds).

cmd_items('--') --> !, [sep].
cmd_items(Cmd) -->
	{ findall(u(Cmd, Args, Text), cmd_usage(Cmd, Args, Text), Us) },
	emit(Us),
	( { \+ Us = [], help_mode(details, _), cmd_details(Cmd, Text) } ->
	    [sep, d(Text), sep]
	; []
	).

% ---------------------------------------------------------------------------
:- doc(section, "Format documentation items").

show_items([]) --> !, [].
show_items([sep]) --> !, []. % skip trailing sep
show_items([sep|Xs]) --> { Xs = [sep|_] }, !, show_items(Xs). % collapse sep
show_items([X|Xs]) --> show_item(X), show_items(Xs).

show_item(s(Str)) -->
	help_section(Str).
show_item(u(Cmd,Args,Text)) -->
	show_cmd_usage(Cmd, Args, Text).
show_item(d(Text)) -->
	show_details(Text).
show_item(sep) -->
	show_sep.

top_cmd_str(Str, Args) :- top_cmd_name(X, Args), atom_codes(X, Str).

% ---------------------------------------------------------------------------
:- doc(section, "Help banner").

banner_help_str -->
	{ top_cmd_str(Cmd, Args) },
	"Usage: ", string(Cmd), " ", string(Args), "\n",
	"\n",
	( { \+ help_mode(summary, _), top_cmd_details(Text) } ->
	    show_details(Text),
	    "\n"
	; []
	),
	( { help_mode(summary, _) } ->
            "The most commonly used commands are:\n\n"
	; []
	).

ending_help_str --> { help_mode(summary, _) }, !,
	"\n",
	{ top_cmd_str(Cmd, _) },
	"Use \"", string(Cmd), " help-all\" for a list of all the available commands.\n",
	"Use \"", string(Cmd), " help <cmd>\" for more information about a command.\n",
	"\n".
ending_help_str --> { help_mode(all, _) }, !,
	{ top_cmd_str(Cmd, _) },
	"Use \"", string(Cmd), " help <cmd>\" for more information about a command.\n",
	"\n".
ending_help_str --> [].

% Group of a command
% TODO: Some commands should be unified using options, this is a
%   temporary hack.

% ---------------------------------------------------------------------------
:- doc(section, "Formatting").

dashify(X, Y) :-
	atom_codes(X, Xs),
	dashify_(Xs, Ys),
	atom_codes(Y, Ys).

dashify_([], []) :- !.
dashify_([0'_|Xs], [0'-|Ys]) :- !, dashify_(Xs, Ys).
dashify_([X|Xs], [X|Ys]) :- dashify_(Xs, Ys).

% (use for codes)
string([]) --> [].
string([X|Xs]) --> [X], string(Xs).

% (use for anything else)
emit([]) --> [].
emit([X|Xs]) --> [X], emit(Xs).

% TODO: Use ANSI colors?
%
% % # Setup ANSI color if the output is a TTY and the terminal does support them
% % 
% % #if tty -s <&1 && ( [ x"${TERM}" = x"xterm" ] || [ x"${EMACS}" = x"t" ] ) ; then
% % if tty -s <&1 && [ x"${TERM}" = x"xterm" ] ; then
% %     BOLD_B="\033[1m"
% %     BOLD_E="\033[0m"
% %     UNDER_B="\033[4m"
% %     UNDER_E="\033[0m"
% % else
% %     BOLD_B=""
% %     BOLD_E=""
% %     UNDER_B=""
% %     UNDER_E=""
% % fi
% % 
% % # Echo in 'bold' style
% % bold_echo() {
% %     echo "${BOLD_B}${1}${BOLD_E}"
% % }
% % # Echo in 'under' style
% % under_echo() {
% %     echo "${UNDER_B}${1}${UNDER_E}"
% % }

bold_echo(X) --> string(X).

under_echo(X) --> string(X).

blanks(N) --> { N =< 0 }, !, [].
blanks(N) --> " ", { N1 is N - 1 }, blanks(N1).

marginsize(2).
col1size(27).

% Name of the command from the command-line (replace '_' by '-')
show_cmd_usage(Cmd, _Args, Desc) -->
	{ help_mode(summary, _) }, % Do not show args in summary
	!,
	show_cmd_usage_(Cmd, "", Desc).
show_cmd_usage(Cmd, Args, Desc) -->
	show_cmd_usage_(Cmd, Args, Desc).

show_cmd_usage_(Cmd, Args, Desc) -->
	% Margin
	{ marginsize(Margin) },
	blanks(Margin),
	% Command name
	{ dashify(Cmd, Cmd1) },
	{ atom_codes(Cmd1, Cmd2) },
	bold_echo(Cmd2),
	% Command arguments (optional)
	{ Args = "" -> Args2 = ""
	; Args2 = " "||Args
	},
	string(Args2),
	% Compute size for alignment
	{ length(Cmd2, La) },
	{ length(Args2, Le) },
	{ L is La + Le + Margin },
	{ col1size(Col) },
	{ L < Col -> Pad is Col - L ; Pad = 0 },
	( { Pad = 0 } -> "\n", blanks(Col)
	; blanks(Pad)
	),
	aligntext(Desc, Col).

% Additional description of a command
show_details(Text) -->
	% Margin (twice)
	{ marginsize(Margin), Margin2 is 2 * Margin },
	blanks(Margin2),
	aligntext(Text, Margin2).

% Display an aligned text. The text is given as a list of lists.
% (assume that the alignment padding for the first line is already
% printed)
aligntext([Row|Rows], Col) --> !,
	string(Row), "\n",
	( { Rows = [] } -> []
	; % More rows, align and print
	  blanks(Col),
	  aligntext(Rows, Col)
	).
aligntext([], _Col) --> [].

help_section(_) --> { help_mode(summary, _) }, !, [].
help_section(Text) -->
	under_echo(Text),
	":\n",
	"\n".

show_sep --> { help_mode(summary, _) }, !, [].
show_sep -->
	"\n".

