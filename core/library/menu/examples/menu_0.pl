:- module(_, [menu_demo/0], [assertions, regtypes, menu]).

% --------------------------------------------------------------------------
% Entry point for testing:
menu_demo :-
	menu(entry),
	print_flag(verb),
	print_flag(optimize).

print_flag(F) :-
	nl, display('Value of '), display(F), display(' is: '),
	get_menu_flag(entry, F, X),
	display(X), nl.

% --------------------------------------------------------------------------
% Menu hooks:

hook_menu_flag_values(_, A, B) :-
	flag(A, B, _).

% Help for flag.
hook_menu_flag_help(_, F, H) :-
	flag(F, _, H).

% --------------------------------------------------------------------------
% Flags (format determined by hooks):

%label , title_text               # flag_name - def_value.
entry, 'Select verbosity level' # verb - off.
entry, 'Select optim level' # optimize - off.

flag(verb,     [off, quiet, loud], 'Selects verbosity level.').
flag(optimize, [off, on],          'Whether to optimize.').
% --------------------------------------------------------------------------
