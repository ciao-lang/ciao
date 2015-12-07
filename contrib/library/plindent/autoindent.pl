:- module(autoindent, [plindent/3, plindent_file/1, plindent_file/2],
	    [assertions, dcg, fsyntax]).

:- use_package(plindent(plindent_decl)).

:- use_module(library(file_utils)).
:- use_module(library(lists)).

:- use_module(plindent(idtokens),    [identify_tokens/6]).
:- use_module(plindent(poslastchar), [pos_last_char/3]).
:- use_module(plindent(normspaced), [normalize_spaced/3, new_lines/3,
		num_lines/3]).
:- use_module(plindent(idfunctors)).
:- use_module(plindent(plisettings)).

:- doc(author, "Edison Mera").
:- doc(module, "Automatic indenter and formatter for prolog.").

auto_indent_tokens([], _, _, _, Values, Values, _).
auto_indent_tokens([Token0|Tokens0], PliConfig, IndentLevel, ALines, Values0,
	    Values, Pos0) :-
	Token0 = token${type => TokenType0},
	auto_indent_tokens_(TokenType0, Token0, Tokens0, PliConfig,
	    IndentLevel, ALines, Values0, Values, Pos0).

auto_indent_tokens_(spaces, _, Tokens, PliConfig, IndentLevel0, ALines0,
	    Values0, Values, Pos0) :-
	!,
	auto_indent_tokens(Tokens, PliConfig, IndentLevel0, ALines0, Values0,
	    Values, Pos0).
auto_indent_tokens_(_, Token0, Tokens0, PliConfig, IndentLevel0, ALines0,
	    Values0, Values, Pos0) :-
	Token0 = token${value => Value0},
	pos_last_char(Value0, Pos0, Pos1),
	auto_indent_tokens__(Tokens0, Tokens0, Token0, PliConfig,
	    IndentLevel0, ALines0, Values0, Values, Pos1).

auto_indent_tokens__([], _, Token0, _, _, _,
	    [Token0, token${type => spaces, value => "\n "}|Values],
	    Values, _).
auto_indent_tokens__([Token1|Tokens1], Tokens0, Token0, PliConfig,
	    IndentLevel0, ALines0, Values0, Values, Pos1) :-
	Token0 = token${type => TokenType0},
	Token1 = token${type => TokenType1, value => Value1},
	update_indent(TokenType0, IndentLevel0, IndentLevel1),
	update_alines(TokenType0, ALines0, ALines1),
	auto_indent_tokens___(TokenType1, PliConfig, Value1, IndentLevel1,
	    ALines1, Tokens1, Tokens0, Token0, Values0, Values, Pos1).

recalculate_num_lines(TokenType0, ALines0, ALines, DLines0, DLines) :-
	ALines0 =\= 0,
	DLines0 =\= 0 ->
	(
	    TokenType0 \== comment1 ->
	    ALines is ALines0 - 1,
	    DLines is DLines0 - 1
	;
	    (
		DLines0 > 1 ->
		ALines is ALines0 - 1,
		DLines is DLines0 - 1
	    ;
		ALines = ALines0,
		DLines = DLines0
	    )
	)
    ;
	ALines = ALines0,
	DLines = DLines0.

auto_indent_tokens___(spaces, PliConfig, Value1, IndentLevel, ALines1,
	    Tokens1, _Tokens0, Token0, Values0, Values, Pos1) :-
	!,
	(
	    Tokens1 = [Token2|Tokens2]
	->
	    num_lines(Value1, 0, DLines0),
	    Token2 = token${type => TokenType2, value => _Value2},
	    Token0 = token${type => TokenType0},
	    create_autospace(ALines1, ALines2, IndentLevel, PliConfig, DLines0,
		DLines, TokenType0, Token2, Tokens2, Pos1, Value1, Spaces0),
	    (
		DLines == 0,
		IndentLevel = [openpar(S-Line0)|_],
		Token0 = token${type => openpar, value => "("},
		Spaces0 == "" ->
		Spaces = S,
		Pos1 = pos(_, Line0)
	    ;
		IndentLevel = [openpar(S-Line0)|_],
		var(S),
		nonvar(Line0),
		TokenType2 == closepar ->
		Pos1 = pos(_, Line1),
		(Line0 == Line1 -> S = "" ; S = " "),
		(Spaces0 == "" -> Spaces = S ; Spaces = Spaces0)
	    ;
		Spaces = Spaces0
	    )
	;
	    Spaces = Value1
	),
	pos_last_char(Spaces, Pos1, Pos2),
	Values0 = [Token0, token${type => spaces, value => Spaces}|Values1],
	auto_indent_tokens(Tokens1, PliConfig, IndentLevel, ALines2, Values1,
	    Values, Pos2).
auto_indent_tokens___(_TokenType1, PliConfig, _Value1, IndentLevel1, ALines1,
	    _Tokens1, Tokens0, Token0, Values0, Values, Pos1) :-
	Values0 = [Token0|Values1],
	auto_indent_tokens(Tokens0, PliConfig, IndentLevel1, ALines1, Values1,
	    Values, Pos1).

pos_last_char_no_nl(Value0, Pos0, Pos) :-
	remove_nl(Value0, Value),
	pos_last_char(Value, Pos0, Pos).

create_autospace(ALines, ALines, _, PliConfig, 0, 0, _,
	    Token2, Tokens2, Pos0, Value1, Value1) :-
	pos_last_char(Value1, Pos0, Pos1),
	Token2 = token${value => Value2},
	pos_last_char(Value2, Pos1, Pos2),
	(
	    Tokens2 = [Token3|Tokens3],
	    Tokens3 = [token${value => Value4}|_] ->
	    Token3 = token${type => TokenType3, value => Value3},
	    (
		TokenType3 == spaces ->
		pos_last_char_no_nl(Value3, Pos2, Pos)
	    ;
		pos_last_char(Value3, Pos2, Pos3),
		pos_last_char_no_nl(Value4, Pos3, Pos)
	    )
	;
	    Pos = Pos2
	),
	Pos = pos(Col, _Line),
	PliConfig = pliconfig${max_length_line => MLL},
	Col =< MLL,
	!.
create_autospace(ALines, ALines, _, _, Lines0, Lines0, _,
	    token${type => TokenType2, value => Value}, _, _, _, Spaces) :-
	% member(TokenType2, [comment1, commentn]),
	TokenType2 == commentn,
	num_lines(Value, 0, Lines),
	Lines > 0,
	!,
	(Lines0 == 0 -> Spaces0 = " " ; Spaces0 = ""),
	new_lines(Lines0, Spaces, Spaces0).
create_autospace(ALines0, ALines, IndentLevel, PliConfig, DLines0, DLines,
	    TokenType0, Token2, _, Pos0, _, Spaces) :-
	Token2 = token${type => TokenType2, value => Value0},
	( DLines0 > 0 -> DLines1 = DLines0, ALines1 = ALines0,
	    recalculate_num_lines(TokenType0, ALines1, ALines, DLines1, DLines)
	; ALines is ALines0 + 1, DLines is DLines0 + 1 ),
	new_lines(DLines, Spaces, Spaces1),
	(
	    DLines == 0 ->
	    Spaces1 = " "
	;
	    create_cond_indent(IndentLevel, TokenType2, Value0, Spaces2, ""),
	    (
		member(TokenType2, [string, atom]),
		Pos0 = pos(_, Line0_),
		Line01_ is Line0_ + DLines,
		Pos01 = pos(0, Line01_),
		pos_last_char(Spaces2, Pos01, Pos1_),
		pos_last_char(Value0,  Pos1_, Pos2_),
		Pos1_ = pos(_,   Line1),
		Pos2_ = pos(Col, Line2),
		PliConfig = pliconfig${max_length_line => MLL},
		(Col > MLL ; Line2 - Line1 > 0)
	    ->
		Spaces1 = ""

	    ;
		Spaces1 = Spaces2
	    )
	).

create_cond_indent([],           _,          _,      Spaces,  Spaces) :- !.
create_cond_indent(IndentLevel0, TokenType0, Value0, Spaces0, Spaces) :-
	requires_previous_indent_level(TokenType0, Value0) ->
	[_|IndentLevel1] = IndentLevel0,
	( IndentLevel1 = []
	-> special_spaced(TokenType0, Value0, Spaces0, Spaces)
	; create_indent(IndentLevel1, Spaces0, Spaces) )
    ;
	create_indent(IndentLevel0, Spaces0, Spaces).

special_spaced(operator, "#", Spaces,           Spaces) :- !.
special_spaced(operator, _,   "    " || Spaces, Spaces) :- !.
special_spaced(_,        _,   Spaces,           Spaces).

:- export(indent_length/3).
create_indent(IndentLevel, Spaces0, Spaces) :-
	indent_length(IndentLevel, 0, L),
	NTabs is L // 8,
	NSpac is L mod 8,
	length(Tabs, NTabs),
	list(Tabs, '='(0'\t)),
	length(Spac, NSpac),
	list(Spac, '='(0' )),
	append(Tabs,    Spac,   Spaces1),
	append(Spaces1, Spaces, Spaces0).

fix_indent(L, 48) :-
	L >= 48.

long_indent(L, L) :-
	L >= 32.

inc_indent(I, L0, L) :-
	L is L0 + I.

indent_length(_) --> fix_indent, !.
indent_length([]) --> [].
indent_length([operator]) --> !, inc_indent(8), indent_length([]).
indent_length([_]) --> !, inc_indent(12), indent_length([]).
indent_length([_|IndentLevel]) -->
	long_indent,
	!,
	inc_indent(1),
	indent_length(IndentLevel).
indent_length([_|IndentLevel]) -->
	inc_indent(4),
	indent_length(IndentLevel).

tokens_strings([]) --> [].
tokens_strings([token${value => String}|Tokens]) -->
	dcg_app(String),
	tokens_strings(Tokens).

dcg_app(String, L0, L) :-
	append(String, L, L0).

plindent(Source, SourceS, TargetS) :-
	init_pliconfig(PliConfig0),
	identify_tokens(Tokens0, Source, PliConfig0, PliConfig, SourceS, []),
	push_prolog_flag(write_strings, on),
	normalize_spaced(Tokens0, PliConfig, Tokens1),
	identify_functors(Tokens1, Tokens2, PliConfig, [], Argdescs0),
	% debug_display(Tokens2, Argdescs0),
	auto_space_argdescs(Argdescs0, []),
	% gnd(Tokens2),
	auto_indent_tokens(Tokens2, PliConfig, [], 0, Tokens, [], pos(0, 1)),
	tokens_strings(Tokens, TargetS, []),
	% flatten(TargetS0, TargetS),
	pop_prolog_flag(write_strings).

/*
debug_display(Tokens2, Argdescs) :-
	varset(Tokens2, Vars),
	enum_vars(Vars, 1),
	tokens_strings(Tokens2, TargetsS, []),
	writeq(TargetsS), nl, nl,
	writeq(Argdescs), nl, nl,
	fail.
debug_display(_, _).
*/

plindent_file(Source, Target) :-
	file_to_string(Source, SourceS),
	plindent(Source, SourceS, TargetS),
	string_to_file(TargetS, Target).

plindent_file(File) :- plindent_file(File, File).

/*
enum_vars([],      _).
enum_vars([Ns|Xs], N) :-
	number_codes(N, Ns0),
	append("("||Ns0, ")", Ns),
	N1 is N + 1,
	enum_vars(Xs, N1).
*/
