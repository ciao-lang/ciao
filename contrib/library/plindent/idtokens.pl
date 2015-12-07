:- module(idtokens, [identify_tokens/6, identify_tokens_/5],
	    [assertions, dcg, fsyntax]).

:- use_module(library(messages)).
:- use_module(library(lists)).
:- use_module(library(llists)).

:- use_module(plindent(plisettings)).
:- use_module(plindent(poslastchar)).

:- doc(author, "Edison Mera").
:- doc(module, "Tokenizer.").

lowchar(A) :- A >= 0'a, A =< 0'z.
uppchar(A) :- A >= 0'A, A =< 0'Z.

expchar := 0'e|0'E.
sign := 0'+|0'-.

alpha(A) :- lowchar(A), !.
alpha(A) :- uppchar(A).

digit(A) :- A >= 0'0, A =< 0'9.

hexdigit(A) :- digit(A).
hexdigit(A) :- A >= 0'a, A =< 0'f.
hexdigit(A) :- A >= 0'A, A =< 0'F.

octaldigit(A) :- 0'0 =< A, A =< 0'7.

bindigit(0'0).
bindigit(0'1).

alphanum(A) :- alpha(A), !.
alphanum(A) :- digit(A).

labelchar(A) :- alphanum(A), !.
labelchar(A) :- underscore(A).

underscore(0'_).

cutchar := 0'!.

floatsep := 0'..

endclausechar := 0'..

space := 0' |0'\n|0'\t|0'|0'\r|0' .

opchar := 0'+|0'-|0'*|0'/|0'\\|0'=|0'<|0'>|0':|0'&|0'||0'$|0'.|0'?|0';|0'~|0'#|
	0'^|0'@.

var0(A) :- uppchar(A), !.
var0(A) :- underscore(A).

chartype(lowchar) := ~lowchar.
chartype(expchar) := ~expchar.
chartype(opchar) := ~opchar.
chartype(space) := ~space.
chartype(digit) := ~digit.
chartype(cutchar) := ~cutchar.
chartype(labelchar) := ~labelchar.
chartype(floatsep) := ~floatsep.
chartype(hexdigit) := ~hexdigit.
chartype(octaldigit) := ~octaldigit.
chartype(bindigit) := ~bindigit.
chartype(sign) := ~sign.
chartype(var0) := ~var0.
chartype(endclausechar) := ~endclausechar.
chartype(openpar) := ~openpar.
chartype(closepar) := ~closepar.

openpar := 0'(|0'[|0'{.
closepar := 0')|0']|0'}.

% ----------------------------------------------------------------------------

process_command_in_comment(comment1, Value) --> !,
	do_process_command_in_comment(Value).
process_command_in_comment(commentn, Value) --> !,
	do_process_command_in_comment(Value).
process_command_in_comment(_, _) --> [].

max_length_line_id := "max_length_line"|"ml".

indentation_style_id := "indentation_style"|"is".

do_process_command_in_comment(Value) -->
	{append([_, ":- ", ~max_length_line_id, "(" || MLLS, ")."||_],
		Value), number_codes(MLL, MLLS)} ->
	set_max_length_line(MLL).
do_process_command_in_comment(Value) -->
	{append([_, ":- ", ~indentation_style_id, "(" || ISS, ")."||_],
		Value), atom_codes(IS, ISS)} ->
	set_indentation_style(IS).
do_process_command_in_comment(_) --> [].

identify_tokens(Tokens, Source, PliConfig0, PliConfig, String0, String) :-
	identify_tokens_(Tokens, PliConfig0, PliConfig, String0, String),
	(
	    member(token(unknown, Value), Tokens) ->
	    append(InitString, Value, String0),
	    pos_last_char(InitString, pos(0, 1), pos(_Col, Line)),
	    (
		append(Value0, "\n" || _, Value) -> true
	    ;
		Value0 = Value
	    ),
	    show_message(error, loc(Source, Line, Line), "Unknown token ~n~s",
		[Value0]),
	    fail
	; true
	).

:- test identify_tokens_(Tokens, P, _, String, Tail) : (
	    String =
	    ":- module(_, _, [assertions,language]).\nmain :- p(a).\n",
	    P = pliconfig(80, norm_spaced),
	    Tail = []
	) =>
	(
	    Tokens =
	    [
		token(operator, ":-"),
		token(spaces, " "),
		token(openfunc, "module("),
		token(var, "_"),
		token(separator, ","),
		token(spaces, " "),
		token(var, "_"),
		token(separator, ","),
		token(spaces, " "),
		token(openpar, "["),
		token(atom, "assertions"),
		token(separator, ","),
		token(atom, "language"),
		token(closepar, "]"),
		token(closepar, ")"),
		token(endclause, "."),
		token(spaces, "\n"),
		token(atom, "main"),
		token(spaces, " "),
		token(operator, ":-"),
		token(spaces, " "),
		token(openfunc, "p("),
		token(atom, "a"),
		token(closepar, ")"),
		token(endclause, "."),
		token(spaces, "\n")
	    ]
	).

identify_tokens_([Token|Tokens], PliConfig0, PliConfig) -->
	get_token(Token, PliConfig0, PliConfig1),
	!,
	identify_tokens_(Tokens, PliConfig1, PliConfig).
identify_tokens_([], PliConfig, PliConfig) --> "".

get_token(token(TokenType, Value), PliConfig0, PliConfig, String0, String) :-
	parse_token(TokenType, String0, String),
	append(Value, String, String0),
	process_command_in_comment(TokenType, Value, PliConfig0, PliConfig).

parse_token(spaces) -->
	parse_spaces,
	!.
parse_token(string) -->
	parse_string,
	!.
parse_token(openfunc) -->
	parse_atom,
	parse_openpar,
	!.
parse_token(openmeta) -->
	parse_var,
	parse_openpar,
	!.
parse_token(openoper) -->
	parse_operator,
	parse_openpar,
	!.
parse_token(atom) -->
	parse_atom,
	!.
parse_token(number) -->
	parse_number,
	!.
parse_token(cut) -->
	parse_cut,
	!.
parse_token(var) -->
	parse_var,
	!.
parse_token(endclause) -->
	parse_endclause,
	!.
parse_token(comment1) -->
	parse_comment1,
	!.
parse_token(commentn) -->
	parse_commentn,
	!.
parse_token(operator) -->
	parse_operator,
	!.
parse_token(separator) -->
	parse_separator,
	!.
parse_token(openpar) -->
	parse_openpar,
	!.
parse_token(closepar) -->
	parse_closepar,
	!.
parse_token(unknown) -->
	parse_unknown.

parse_spaces -->
	parse_char(space),
	parse_chars(space).

parse_comment1 -->
	"%",
	!,
	parse_comment1_.

parse_comment1_(String, String) :-
	(String == [] ; \+ \+ (String = "\n" || _)),
	!.
parse_comment1_ -->
	[_C],
	parse_comment1_,
	!.

parse_commentn -->
	"/*",
	!,
	parse_commentn_.

parse_commentn_ -->
	"*/",
	!.
parse_commentn_ -->
	[_C],
	parse_commentn_.

parse_unknown -->
	[_C],
	!,
	parse_unknown_.

parse_unknown_ -->
	[_C],
	!,
	parse_unknown_.
parse_unknown_ --> "".

parse_string -->
	parse_enclosed(0'\").

parse_cut -->
	parse_char(cutchar).

parse_atom -->
	"[]",
	!.
parse_atom -->
	parse_aatom,
	!.
parse_atom -->
	parse_qatom,
	!.

parse_aatom -->
	parse_char(lowchar),
	parse_chars(labelchar).

parse_qatom -->
	parse_enclosed(0'\').

parse_number -->
	parse_float.

parse_number -->
	parse_int.

parse_float -->
	parse_char(sign),
	parse_nnegfloat.
parse_float -->
	parse_nnegfloat.

parse_nnegfloat -->
	"0.Nan",
	!.
parse_nnegfloat -->
	"0.Inf",
	!.
parse_nnegfloat -->
	parse_int,
	parse_char(floatsep),
	parse_nnegint,
	optional_exp.

optional_exp -->
	parse_char(expchar),
	parse_int,
	!.
optional_exp --> "".

parse_nnegint -->
	parse_ascii.
parse_nnegint -->
	parse_octal.
parse_nnegint -->
	parse_bin.
parse_nnegint -->
	parse_hex.
parse_nnegint -->
	parse_decimal.

parse_decimal -->
	parse_char(digit),
	parse_chars(digit).

parse_ascii -->
	"0\'\'\'",
	!.
parse_ascii -->
	"0\'\\",
	[_C],
	!.
parse_ascii -->
	"0\'",
	[_C].

parse_octal -->
	"0o",
	parse_octaldigits.

parse_hex -->
	"0x",
	parse_hexdigits.

parse_bin -->
	"2\'",
	parse_bindigits.

parse_bin -->
	"0b",
	parse_bindigits.

parse_hexdigits -->
	parse_char(hexdigit),
	parse_chars(hexdigit).

parse_octaldigits -->
	parse_char(octaldigit),
	parse_chars(octaldigit).

parse_bindigits -->
	parse_char(bindigit),
	parse_chars(bindigit).

parse_int -->
	parse_nnegint,
	!.
parse_int -->
	parse_char(sign),
	parse_nnegint.

parse_var -->
	parse_char(var0),
	parse_chars(labelchar).

%:- meta_predicate parse_chars(pred(1), ?, ?).

parse_chars(CharType) -->
	parse_char(CharType),
	!,
	parse_chars(CharType).
parse_chars(_) --> "".

%:- meta_predicate parse_char(pred(1), ?, ?).

% parse_char(CharType) -->
% 	[C],
% 	{chartype(CharType, C)},
% 	!.

parse_char(CharType, [C|T], T) :-
	chartype(CharType, C),
	!.

parse_operator -->
	parse_char(opchar),
	parse_chars(opchar).

parse_endclause -->
	parse_char(endclausechar),
	\+ parse_char(opchar),
	\+ parse_char(openpar).

parse_separator --> ",".

parse_openpar -->
	parse_char(openpar).

parse_closepar -->
	parse_char(closepar).

parse_enclosed(EncloseChar) -->
	[EncloseChar],
	parse_enclosed_(EncloseChar).

parse_enclosed_(EncloseChar) -->
	[EncloseChar, EncloseChar],
	!,
	parse_enclosed_(EncloseChar).
parse_enclosed_(EncloseChar) -->
	"\\",
	[_C],
	!,
	parse_enclosed_(EncloseChar).
parse_enclosed_(EncloseChar) -->
	[EncloseChar],
	!.
parse_enclosed_(EncloseChar) -->
	[_C],
	parse_enclosed_(EncloseChar).
