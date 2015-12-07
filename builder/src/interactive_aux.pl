:- module(interactive_aux, [], [assertions, basicmodes, dcg, fsyntax, hiord]).
% TODO: Not the best module name
% TODO: Merge with library/menu

:- doc(title, "Auxiliary Code for User Interaction").
:- doc(author, "Ciao Development Team").

:- use_module(library(messages)).

:- export(ask_option_value/6).
:- pred ask_option_value(+Description, +Name, +ValidValues,
	                 +DefaultValue, +PreviousValue, -Value)
   :: string * atm * list(term) * term * term * term
   # "Ask (interactively) for a value for variable @var{Name}. Repeat
      until the user provides one of the @var{ValidValues}, or chose
      @var{PreviousValue} if none was provided.".

ask_option_value(Description, Name, ValidValues,
	         DefaultValue, PreviousValue, Value) :-
	% Show description and default value
	nl, display_string(Description), nl,
	% Loop until we get a valid value
	ask_option_loop(Name, ValidValues,
	                DefaultValue, PreviousValue, Value).

ask_option_loop(Name, ValidValues, DefaultValue, PreviousValue, Value) :-
	display_list(['\n', Name]),
	( nonvar(PreviousValue) ->
	    display_list(['=[', PreviousValue, ']'])
	; true
	),
	( DefaultValue == PreviousValue ->
	    true
	; display_list([' (default: ', DefaultValue, ')'])
	),
	display_list([' ? ']),
	( read_value(ValidValues, PreviousValue, Value) ->
	    true
	; display_list(['Please enter a valid value (', ValidValues, ')\n']),
	  ask_option_loop(Name, ValidValues, DefaultValue, PreviousValue, Value)
	).

read_value(ValidValues, PreviousValue, Value) :-
	current_input(S),
	read_line_as_atom(S, Value1),
	( Value1 = '' ->
	    Value = PreviousValue
	; Value = Value1
	),
	% Check value
	( var(ValidValues) ->
	    % no checks
	    true
	; nonvar(Value), member(Value, ValidValues) ->
	    true
	; fail
	).

% Read atom from Stream
read_line_as_atom(Stream, Atom) :-
	read_line(Stream, String),
	atom_codes(Atom, String).

% Read a line (string) from Stream
read_line(Stream, String) :-
	get_code(Stream, Code),
	( Code = 0'\n ->
	    String = []
	; String = [Code|String2],
	  read_line(Stream, String2)
	).

% ---------------------------------------------------------------------------

:- use_module(library(strings), [get_line/1, write_string/1]).

:- export(ask_yesno/1).
% Prompt Question to the user, wait until 'yes' or 'no' is written
ask_yesno(Question) :-
	write_string(Question), write_string(" (y/n) "),
	ask_yesno_loop.

ask_yesno_loop :-
	get_line(Answer),
	( Answer = "y" ->
	    true
	; Answer = "n" ->
	    fail
	; display_list(['Unrecognized answer. Please type \'y\' for \'yes\' or \'n\' for \'no\'.\n']),
	  ask_yesno_loop
	).

