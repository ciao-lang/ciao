:- module(plisettings, [
		autospace_style/6,
		remove_currspace/3,
		not_autospace/4,
		update_indent/3,
		update_alines/3,
		% inc_lines_indent_level/3,
		init_pliconfig/1,
		is_opener/1,
		is_end_argument/1,
		not_auto_tabuled/2,
		bookmark_begin/2,
		indentation_style_t/1,
		set_max_length_line/3,
		set_indentation_style/3,
		requires_previous_indent_level/2,
		% regtypes
		indentation_style_t/1,
		pliconfig_t/1,
		token_type_t/1,
		indent_level_t/1
	    ],
	    [assertions, basicmodes, nortchecks, regtypes, fsyntax,
		plindent(plindent_decl)]).

:- doc(author, "Edison Mera").

:- doc(module, "This file contains internal settings that are "||
	    "difficult to calibrate manually.").

:- regtype indentation_style_t/1.

indentation_style_t := full_spaced|norm_spaced|left_spaced|save_spaces.

set_max_length_line(MLL,
	    pliconfig${indentation_style => IS},
	    pliconfig${indentation_style => IS, max_length_line => MLL}).

set_indentation_style(IS,
	    pliconfig${max_length_line => MLL},
	    pliconfig${max_length_line => MLL, indentation_style => IS}).

:- pred init_pliconfig(?pliconfig_t) # "Get default configuration values".

init_pliconfig(pliconfig${
		max_length_line => 81,
		indentation_style => norm_spaced
	    }).

:- regtype pliconfig_t/1.
pliconfig_t(pliconfig${max_length_line => MLL, indentation_style => IS}) :-
	nnegint(MLL),
	indentation_style_t(IS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% used in normspaced:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_currspace(comment1, _,         _) :- !, fail.
remove_currspace(_,        separator, _) :- !.
remove_currspace(_,        endclause, _).

% not_autospace(operator, atom,      ":",  _).
% not_autospace(operator, atom,      "~",  _).
% not_autospace(atom,     operator,  _,    ":").
% not_autospace(atom,     operator,  _,    "/").
% not_autospace(atom,     operator,  _,    "$").

not_autospace(atom,     operator,  _,    _).
not_autospace(operator, atom,      _,    _).
not_autospace(var,      operator,  _,    ":").
not_autospace(operator, var,       _,    _).
not_autospace(operator, openfunc,  _,    _).
not_autospace(operator, openmeta,  _,    _).
not_autospace(operator, openpar,   _,    _).
not_autospace(operator, number,    _,    _).
not_autospace(atom,     openpar,   _,    _).
not_autospace(var,      openpar,   _,    _).
not_autospace(var,      operator,  _,    _).
not_autospace(var,      number,    _,    _).
not_autospace(number,   operator,  _,    _).
not_autospace(string,   operator,  _,    "||").
not_autospace(operator, string,    "||", _).
not_autospace(operator, var,       "||", _).
not_autospace(spaces,   _,         _,    _).
not_autospace(_,        separator, _,    _).
not_autospace(_,        endclause, _,    _).

autospace_style(full_spaced, _,          _,          _,      _,      " ").
autospace_style(norm_spaced, TokenType0, TokenType1, Value0, Value1, Space) :-
	autospace_norm_spaced(TokenType0, TokenType1, Value0, Value1, Space).
autospace_style(save_spaces, TokenType0, TokenType1, Value0, Value1, Space) :-
	autospace_save_spaces(TokenType0, TokenType1, Value0, Value1, Space).
autospace_style(left_spaced, TokenType0, TokenType1, Value0, Value1, Space) :-
	autospace_left_spaced(TokenType0, TokenType1, Value0, Value1, Space).

autospace_norm_spaced(openpar,   _,         _,   _,    "") :- !.
autospace_norm_spaced(openoper,  _,         _,   _,    "") :- !.
autospace_norm_spaced(openfunc,  _,         _,   _,    "") :- !.
autospace_norm_spaced(openmeta,  _,         _,   _,    "") :- !.
autospace_norm_spaced(_,         closepar,  _,   _,    "") :- !.
autospace_norm_spaced(operator,  _,         "|", _,    "") :- !.
autospace_norm_spaced(operator,  operator,  _,   _,    " ") :- !.
autospace_norm_spaced(operator,  openoper,  _,   _,    " ") :- !.
autospace_norm_spaced(_,         operator,  _,   "|",  "") :- !.
autospace_norm_spaced(separator, openoper,  _,   _,    " ") :- !.
autospace_norm_spaced(atom,      openoper,  _,   "${", "") :- !.
autospace_norm_spaced(atom,      openoper,  _,   _,    " ") :- !.
autospace_norm_spaced(_,         openoper,  _,   _,    "") :- !.
autospace_norm_spaced(_,         separator, _,   _,    "") :- !.
autospace_norm_spaced(_,         _,         _,   _,    " ") :- !.

autospace_save_spaces(atom,     openfunc, _, _, " ") :- !.
autospace_save_spaces(atom,     openmeta, _, _, " ") :- !.
autospace_save_spaces(operator, operator, _, _, " ") :- !.
autospace_save_spaces(operator, openoper, _, _, " ") :- !.
autospace_save_spaces(_,        _,        _, _, "") :- !.

autospace_left_spaced(openpar,   _,        _,   _,    " ") :- !.
autospace_left_spaced(openoper,  _,        _,   _,    " ") :- !.
autospace_left_spaced(openfunc,  _,        _,   _,    " ") :- !.
autospace_left_spaced(openmeta,  _,        _,   _,    " ") :- !.
autospace_left_spaced(_,         closepar, _,   _,    "") :- !.
autospace_left_spaced(operator,  _,        "|", _,    "") :- !.
autospace_left_spaced(operator,  operator, _,   _,    " ") :- !.
autospace_left_spaced(operator,  openoper, _,   _,    " ") :- !.
autospace_left_spaced(_,         operator, _,   "|",  "") :- !.
autospace_left_spaced(separator, openoper, _,   _,    " ") :- !.
autospace_left_spaced(atom,      openoper, _,   "${", "") :- !.
autospace_left_spaced(atom,      openoper, _,   _,    " ") :- !.
autospace_left_spaced(_,         openoper, _,   _,    "") :- !.
autospace_left_spaced(_,         _,        _,   _,    " ") :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% used in idfunctors:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

not_auto_tabuled(norm_spaced) := closepar.

is_opener := openpar|openfunc|openmeta|openoper.

is_end_argument := closepar|separator.

:- regtype indent_level_t/1.
indent_level_t(operator).
indent_level_t(openpar(N, S)) :-
	nnegint(N),
	list(S).

:- regtype token_type_t/1.

token_type_t := spaces|string|openfunc|openmeta|openoper|atom|number|cut|var|
	endclause|comment1|commentn|operator|separator|openpar|closepar|unknown.

:- pred update_indent(+token_type_t, ?list(indent_level_t),
	    ?list(indent_level_t)).

update_indent(operator,  [],             [operator]) :- !.
update_indent(endclause, [operator],     []) :- !.
update_indent(closepar,  [openpar(_)|I], I) :- !.
update_indent(openpar,   I,              [openpar(_)|I]).
update_indent(openfunc,  I,              [openpar("" -_)|I]).
update_indent(openmeta,  I,              [openpar("" -_)|I]).
update_indent(openoper,  I,              [openpar("" -_)|I]).
update_indent(_,         I,              I).

update_alines(closepar,  L0, L) :- !, (L0 > 0 -> L is L0 - 1 ; L is 0).
update_alines(endclause, _,  0).
update_alines(_,         L,  L).

bookmark_begin(separator, _).
bookmark_begin(endclause, _).
bookmark_begin(operator) := "-->"|":-"|";"|"->"|":=".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% used in autoindent:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

requires_previous_indent_level(operator, A) :-
	requires_previous_indent_level_op(A).
requires_previous_indent_level(closepar, _).

requires_previous_indent_level_op := ";"|"->"|"#".
