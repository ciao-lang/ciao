:- module(_, _, [assertions, dcg]).

:- use_package(plindent(plindent_decl)).

:- use_module(plindent(plisettings)).

:- doc(author, "Edison Mera").
:- doc(module, "Space normalizer.").

normalize_spaced([],               _,         []).
normalize_spaced([Token0|Tokens0], PliConfig, Values0) :-
	Token0 = token${type => TokenType0},
	(
	    TokenType0 \== spaces ->
	    Token1 = token${type => spaces, value => "\n "},
	    Tokens1 = [Token0|Tokens0]
	;
	    Token1 = Token0,
	    Tokens1 = Tokens0
	),
	normalize_spaced__(Tokens1, Token1, Tokens1, PliConfig, Values0).

normalize_spaced_r([],               _,          _,         _,      []).
normalize_spaced_r([Token0|Tokens0], TokenType1, PliConfig, Value1, Values0) :-
	Token0 = token${type => TokenType0, value => Value0},
	PliConfig = pliconfig${indentation_style => IndentationStyle},
	autospace_style(IndentationStyle, TokenType1, TokenType0, Value1,
	    Value0, Space),
	normalize_space_(TokenType0, Space, Value0, Value2),
	normalize_spaced__(Tokens0,
	    token${type => TokenType0, value => Value2}, Tokens0, PliConfig,
	    Values0).

normalize_space_(spaces, Space, Value0, Spaces) :-
	!,
	normalize_space(Space, Value0, Spaces).
normalize_space_(_, _, Value, Value).

normalize_space(Space, Value0, Spaces) :-
	num_lines(Value0, 0, DLines),
	(
	    DLines =:= 0 ->
	    new_lines(DLines, Spaces, Space)
	;
	    new_lines(DLines, Spaces, " ")
	).

normalize_spaced__([], token${type => spaces, value => Value0}, _, _,
	    [token${type => spaces, value => Value0}]) :-
	!.
normalize_spaced__([], Token0, _, _,
	    [Token0, token${type => spaces, value => "\n "}]).

normalize_spaced__([Token1|Tokens1], Token0, Tokens0, PliConfig, Values0) :-
	Token1 = token${type => TokenType1, value => Value1},
	normalize_spaced___(TokenType1, Value1, Token0, Tokens0,
	    PliConfig, Tokens1, Values0).

remove_currspace_token(token${type => TokenType0},
	    token${type => TokenType2, value => Value2}) :-
	remove_currspace(TokenType0, TokenType2, Value2).

autospace_ttoken(token${type => TokenType0, value => Value0},
	    token${type => TokenType2, value => Value2}, PliConfig, Space) :-
	PliConfig = pliconfig${indentation_style => IndentationStyle},
	autospace_style(IndentationStyle, TokenType0, TokenType2, Value0,
	    Value2, Space).

normalize_spaced___(spaces, Value1, Token0, _, PliConfig, Tokens1, Values0) :-
	!,
	(
	    Tokens1 = [Token2|_]
	->
	    (
		remove_currspace_token(Token0, Token2)
	    ->
		Values0 = [Token0|Values1]
	    ;
		autospace_ttoken(Token0, Token2, PliConfig, Space),
		normalize_space(Space, Value1, Spaces),
		Values0 = [Token0,
		    token${type => spaces, value => Spaces}|Values1]
	    )
	;
	    Spaces = "\n",
	    Values0 = [Token0,
		token${type => spaces, value => Spaces}|Values1]
	),
	normalize_spaced_r(Tokens1, spaces, PliConfig, Value1, Values1).
normalize_spaced___(TokenType1, Value1, Token0, Tokens0, PliConfig, _,
	    Values0) :-
	!,
	(
	    not_autospace_token(Token0, TokenType1, Value1)
	->
	    Values0 = [Token0|Values1]
	;
	    autospace_token(Token0, TokenType1, Value1, PliConfig, Space),
	    Values0 = [Token0, token${type => spaces, value => Space}
		|Values1]
	),
	normalize_spaced_r(Tokens0, TokenType1, PliConfig, Value1, Values1).

autospace_token(token${type => TokenType0, value => Value0}, TokenType1,
	    Value1, PliConfig, Space) :-
	PliConfig = pliconfig${indentation_style => IndentationStyle},
	autospace_style(IndentationStyle, TokenType0, TokenType1, Value0,
	    Value1, Space).

not_autospace_token(token${type => TokenType0, value => Value0},
	    TokenType1, Value1) :-
	not_autospace(TokenType0, TokenType1, Value0, Value1),
	!.

new_lines(0,  I,         I) :- !.
new_lines(N0, [0'\n|I0], I) :-
	N0 > 0,
	N is N0 - 1,
	new_lines(N, I0, I).

num_lines([],           Lines,  Lines).
num_lines([0'\n|Chars], Lines0, Lines) :-
	!,
	Lines1 is Lines0 + 1,
	num_lines(Chars, Lines1, Lines).
num_lines([_|Chars], Lines0, Lines) :-
	num_lines(Chars, Lines0, Lines).
