:- module(tokenize, [], [assertions, define_flag]).

:- doc(title, "Tokenizer").
:- doc(author, "The Ciao Development Team").
:- doc(author, "Jose F. Morales (curly blocks, Unicode source support)").

:- doc(module, "This module defines the tokenizer for Ciao.  In
   addition to optional flags, the main differences w.r.t. the ISO-Prolog
   standard are:

   @begin{itemize}
   @item @tt{`} is a graphic char, there are no @tt{back_quoted_strings}.
   @item @tt{\\\\} followed by any layout char (not only
     @tt{new_line}) in a string is a @tt{continuation_escape_sequence}.
   @item @tt{\\\\^} starts a @tt{control_escape_char} in a string.
   @item @tt{\\\\c} skips layout in a string.
   @item @tt{\\\\e} = @tt{ESC}, @tt{\\\\d} = @tt{DEL}, @tt{\\\\s} = @tt{SPACE}.
   @item @tt{13'23} is 23 in base 13 (same for other bases).
   @item @tt{0'}@tt{'} is accepted as @tt{0'}@tt{'}@tt{'} (if not followed by @tt{'}).
   @item Quoted atoms and strings support Unicode escape
     @tt{\\\\uDDDD} (four hexadecimal digits) and @tt{\\\\UDDDDDDDD}
     (eight hexadecimal digits)
   @item Support for Unicode identifiers (see @ref{Unicode source code}).
   @end{itemize}

   @section{Unicode source code}

   @cindex{unicode}

   The tokenizer for Ciao extends ISO-Prolog with
   support for Unicode source identifiers, based on the the
   @href{https://unicode.org/reports/tr31/#Case_and_Stability}{Unicode
   Standard Annex 31}, as follows:

   @begin{itemize}  
   @item Identifiers can begin with @tt{XID_Start} characters and must
     be followed with zero or more @tt{XID_Continue} (see the Unicode
     Derived Core Properties), extended with categorty @tt{No}.
     Variables are those identifiers that start with characters in the
     @tt{Lu} category.
   @item Use @tt{Z*} as layout characters, as well as other control
     characters (@tt{Cc} categoty) with bidirectional category @tt{WS}
     (whitespace), @tt{S} (segment separator), or @tt{B} (paragraph
     separator).
   @item Use @tt{S*} (@tt{Sm}, @tt{Sc}, @tt{Sk}, @tt{So}) and @tt{P*}
     (@tt{Pc}, @tt{Pd}, @tt{Ps}, @tt{Pe}, @tt{Pi}, @tt{Pf}, @tt{Po})
     as symbols.
   @item Identifiers that begin with @tt{XID_Continue} are treated as
     @em{solo} tokens.
   @end{itemize}  
   ").

:- use_module(engine(runtime_control), [current_prolog_flag/2]).
:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic)).
:- use_module(engine(internals)).
:- use_module(library(read), [second_prompt/2]).
:- use_module(library(dict)).

:- set_prolog_flag(multi_arity_warnings, off).

define_flag(character_escapes, [iso, sicstus, off], iso).
define_flag(doccomments, [on, off], off).

% Existing tokens:
%    atom(atom)
%    badatom(string)
%    number(number)
%    string(string)
%    var(term,string)
%    '/* ...'
%    ',' | '(' | ' (' | ')' | '[' | ']' | '|' | '{' | '}'
%    '.' % end of term 

:- export(token/1).
:- prop token(T) + regtype.
token(atom(A)):- atm(A).
token(badatom(S)):- string(S).
token(number(N)):- num(N).
token(string(S)):- string(S).
token(var(T,S)):- term(T), string(S).
token('/* ...').
token(',').
token('(').
token(' (').
token(')').
token('[').
token(']').
token('|').
token('{').
token('}').
token('.').

% See io_basic:code_class/2 documentation for the available character
% types retured by getct1/2 and getct/2.

:- export(read_tokens/2).
:- pred read_tokens(TokenList, Dictionary) 
    => (list(token,TokenList), dictionary(Dictionary)).

read_tokens(TokenList, Dictionary) :-
    getct1(Ch, Type),
    second_prompt(SP, SP),
    '$prompt'(Old, SP),
    read_tokens(Type, Ch, Dictionary, TokenList),
    '$prompt'(_, Old).

% The only difference between read_tokens_after_layout(Typ, Ch, D, Tokens)
% and read_tokens/4 is what they do when Ch is "(".  The former finds the
% token to be ' (', while the later finds the token to be '('.  This is
% how the parser can tell whether <atom> <paren> must be an operator
% application or an ordinary function symbol application.

read_tokens_after_layout(5, 0'(, Dict, Tokens) :- !,
    getct1(NextCh, NextTyp),
    ( NextCh = 0') ->
        Tokens = [atom('()')|Tokens_],
        getct(NextCh2, NextTyp2),
        read_tokens(NextTyp2, NextCh2, Dict, Tokens_)
    ;
        Tokens = [' ('|Tokens_],
        read_tokens(NextTyp, NextCh, Dict, Tokens_)
    ).
read_tokens_after_layout(Typ, Ch, Dict, Tokens) :-
    read_tokens(Typ, Ch, Dict, Tokens).

read_tokens(-1, _, _, []).                      % end of file
read_tokens(0, _, Dict, Tokens) :-              % layout
    getct1(NextCh, NextTyp),
    read_tokens_after_layout(NextTyp, NextCh, Dict, Tokens).
read_tokens(1, Ch0, Dict, [Atom|Tokens]) :-     % small letter: atom
    S = [Ch0|S0],
    getct(Ch, Typ),
    read_name(Typ, Ch, S0, NextCh, NextTyp),
    atom_token(S, Atom),
    read_tokens(NextTyp, NextCh, Dict, Tokens).
read_tokens(2, Ch0, Dict, [var(Var,S2)|Tokens]) :- % capital letter: variable
    S = [Ch0|S0],
    getct(Ch, Typ),
    read_name(Typ, Ch, S0, NextCh, NextTyp),
    string_bytes(S, S2),
    ( S2 = "_" ->                            % anonymous variable
        true
    ; dic_lookup(Dict, S2, Node),            % lookup/enter in dictionary
      check_singleton(Node, Var)
    ),
    read_tokens(NextTyp, NextCh, Dict, Tokens).
read_tokens(3, Ch, Dict, [number(N)|Tokens]) :-  % number
    read_number(Ch, N, Dict, Tokens).         % reads continuation too.
read_tokens(4, 0'/, Dict, Tokens) :- !,          % comment if an '*' follows
    getct(NextCh, NextTyp),
    read_possible_comment(NextTyp, NextCh, Dict, Tokens).
read_tokens(4, 0'., Dict, Tokens) :- !,          % end token or graphic atom
    getct(NextCh, NextTyp),         
    read_fullstop(NextTyp, NextCh, Dict, Tokens).
read_tokens(4, Ch, Dict, [Atom|Tokens]) :-       % graphic atom
    S = [Ch|Chars],
    getct(AnotherCh, Typ),
    read_symbol(Typ, AnotherCh, Chars, NextCh, NextTyp),
    atom_token(S, Atom),
    read_tokens(NextTyp, NextCh, Dict, Tokens).
read_tokens(5, Ch, Dict, Tokens) :-
    read_tokens_solo(Ch, Dict, Tokens).
read_tokens(6, Ch, Dict, [Atom|Tokens]) :- !,
    % Other Unicode XID_Continue is treated as 'solo' when it appears
    % as first character.
    atom_token([Ch], Atom),
    getct(NextCh, NextTyp),
    read_tokens(NextTyp, NextCh, Dict, Tokens).

% read_possible_comment(Typ, Ch, Dict, Tokens)
% checks to see whether / + Ch is a / + * comment or a symbol.  If the
% former, it skips the comment.  If the latter it just calls read_symbol.
%
% ('/' has been read)
read_possible_comment(4, 0'*, Dict, Tokens) :- !,
    read_comment(block, Dict, Tokens).
read_possible_comment(Typ, Ch, Dict, [Atom|Tokens]) :-
    read_symbol(Typ, Ch, Chars, NextCh, NextTyp), % might read 0 chars
    atom_token([0'/|Chars], Atom),
    read_tokens(NextTyp, NextCh, Dict, Tokens).

read_tokens_solo(0'!, Dict, [atom(!)|Tokens]) :-
    getct(NextCh, NextTyp),
    read_tokens(NextTyp, NextCh, Dict, Tokens).
read_tokens_solo(0';, Dict, [atom(;)|Tokens]) :-
    getct(NextCh, NextTyp),
    read_tokens(NextTyp, NextCh, Dict, Tokens).
read_tokens_solo(0'%, Dict, Tokens) :-          % comment
    read_comment(line, Dict, Tokens).
read_tokens_solo(0'(, Dict, Tokens) :-
    getct1(NextCh, NextTyp),
    ( NextCh = 0') ->
        Tokens = [atom('()')|Tokens_],
        getct(NextCh2, NextTyp2),
        read_tokens(NextTyp2, NextCh2, Dict, Tokens_)
    ;
        Tokens = ['('|Tokens_],
        read_tokens(NextTyp, NextCh, Dict, Tokens_)
    ).
read_tokens_solo(0'), Dict, [')'|Tokens]) :-
    getct1(NextCh, NextTyp),
    read_tokens(NextTyp, NextCh, Dict, Tokens).
read_tokens_solo(0',, Dict, [','|Tokens]) :-
    getct1(NextCh, NextTyp),
    read_tokens(NextTyp, NextCh, Dict, Tokens).
read_tokens_solo(0'[, Dict, ['['|Tokens]) :-
    getct1(NextCh, NextTyp),
    read_tokens(NextTyp, NextCh, Dict, Tokens).
read_tokens_solo(0'], Dict, [']'|Tokens]) :-
    getct1(NextCh, NextTyp),
    read_tokens(NextTyp, NextCh, Dict, Tokens).
read_tokens_solo(0'{, Dict, ['{'|Tokens]) :-
    ( current_prolog_flag(read_curly_blocks, on) ->
        % When reading curly blocks, the tokenizer suspends
        % when '{' is found (not only after '.').
        Tokens = []
    ; getct1(NextCh, NextTyp),
      read_tokens(NextTyp, NextCh, Dict, Tokens)
    ).
read_tokens_solo(0'|, Dict, ['|'|Tokens]) :-
    getct1(NextCh, NextTyp),
    read_tokens(NextTyp, NextCh, Dict, Tokens).
read_tokens_solo(0'}, Dict, ['}'|Tokens]) :-
    getct1(NextCh, NextTyp),
    read_tokens(NextTyp, NextCh, Dict, Tokens).
read_tokens_solo(0'", Dict, Tokens) :-  % "string"
    getct(Ch, Typ),
    read_quoted(Typ, Ch, 0'", Dict, Tokens).
read_tokens_solo(0'', Dict, Tokens) :- % 'atom'
    getct(Ch, Typ),
    read_quoted(Typ, Ch, 0'', Dict, Tokens).

% read_name(Typ, Char, String, LastCh, LastTyp)
% reads a sequence of letters, digits, and underscores, and returns
% them as String.  The first character which cannot join this sequence
% is returned as LastCh.

read_name(1, Char, String, LastCh, LastTyp) :- !,
    read_name_(Char, String, LastCh, LastTyp).
read_name(2, Char, String, LastCh, LastTyp) :- !,
    read_name_(Char, String, LastCh, LastTyp).
read_name(3, Char, String, LastCh, LastTyp) :- !,
    read_name_(Char, String, LastCh, LastTyp).
read_name(6, Char, String, LastCh, LastTyp) :- !,
    read_name_(Char, String, LastCh, LastTyp).
read_name(LastTyp, LastCh, [], LastCh, LastTyp).

read_name_(Char, String, LastCh, LastTyp) :-
    String = [Char|Chars],
    getct(NextCh, NextTyp),
    read_name(NextTyp, NextCh, Chars, LastCh, LastTyp).

% read_symbol(Typ, Ch, String, NextCh, NextTyp)
% reads the other kind of atom which needs no quoting: one which is
% a string of "symbol" characters.

read_symbol(4, Char, String, LastCh, LastTyp) :- !,
    String = [Char|Chars],
    getct(NextCh, NextTyp),
    read_symbol(NextTyp, NextCh, Chars, LastCh, LastTyp).
read_symbol(LastTyp, LastCh, [], LastCh, LastTyp).

% read_fullstop(Typ, Char, Dict, Tokens)
% looks at the next character after a full stop.  If the next character is
% an end of file, a layout character or %, this is a clause terminator, else
% this is just an ordinary symbol and we call read_symbol to process it.

read_fullstop(-1, _, _, [.]) :- !. % end of file
read_fullstop(0, _, _Dict, [.]) :- !. % END OF CLAUSE
read_fullstop(5, 0'%, _, [.]) :- !,             % END OF CLAUSE
    % TODO: read doccomment?
    skip_line.                              % skip newline
read_fullstop(Typ, Ch, Dict, [Atom|Tokens]) :-
    read_symbol(Typ, Ch, S, NextCh, NextTyp),   % symbol
    atom_token([0'.|S], Atom),
    read_tokens(NextTyp, NextCh, Dict, Tokens).

% read_quoted(Typ, Ch, Quote, Dict, Tokens)
% reads an atom or string (delimited by Quote characters), then
% continue reading tokens.

read_quoted(Typ, Ch, Quote, Dict, Tokens) :-
    read_quoted_(Typ, Ch, S, Quote, Dict, S, Tokens).

read_quoted_(-1, _, [], Quote, _Dict, S, Tokens) :- !,
    % Not expected end_of_file in quoted atom or string
    quoted_token(Quote, S, Tokens, Tokens1),
    Tokens1 = [unexpected(-1)].
read_quoted_(5, Quote, Chars, Quote, Dict, S, Tokens) :- !,
    getct(Ch, Typ),                     % closing or doubled quote
    read_quoted_end(Typ, Ch, Quote, Chars, Dict, S, Tokens).
read_quoted_(4, 92, Chars, Quote, Dict, S, Tokens) :-
    current_prolog_flag(character_escapes, CEF),     % escaped character
    CEF \== off,
    !,
    getct(Ch, Typ),                
    escape_sequence(Typ, Ch, CEF, Chars, RestChars, OtTyp, OtCh),
    read_quoted_(OtTyp, OtCh, RestChars, Quote, Dict, S, Tokens).
read_quoted_(_Typ, Char, Chars, Quote, _Dict, S, Tokens) :-
    % Do not allow newline if quoting an atom
    Char = 0'\n, Quote = 0'', !,
    Chars = [],
    quoted_token(Quote, S, Tokens, Tokens1),
    Tokens1 = [unexpected(Char)].
read_quoted_(_, Char, [Char|Chars], Quote, Dict, S, Tokens) :-
    getct(Ch, Typ),                     % ordinary character
    read_quoted_(Typ, Ch, Chars, Quote, Dict, S, Tokens).

read_quoted_end(5, Quote, Quote, [Quote|Chars], Dict, S, Tokens) :- !,
    getct(Ch, Typ),                     % doubled quote
    read_quoted_(Typ, Ch, Chars, Quote, Dict, S, Tokens).
read_quoted_end(NextTyp, NextCh, Quote, [], Dict, S, Tokens) :-
    quoted_token(Quote, S, Tokens, Tokens0),
    read_tokens(NextTyp, NextCh, Dict, Tokens0).

quoted_token(0'', S, Tokens, Tokens0) :-
    atom_token(S, Token), Tokens = [Token|Tokens0].
quoted_token(0'", S, Tokens, Tokens0) :-
    string_bytes(S, S2),
    Tokens = [string(S2)|Tokens0].

escape_sequence(0, _, _, Chars, Chars, Typ, Ch) :- !,
    getct(Ch, Typ).                     % continuation escape sequence
escape_sequence(4, 0'^, _, [Char|Chars], Chars, NextTyp, NextCh) :- !,
    getct(Ch, Typ),                     % starts a control_escape_char
    ( control_character(Typ, Ch, CC) ->
        Char = CC,
        getct(NextCh, NextTyp)
    ; Char = 0'^,
      NextTyp = Typ,
      NextCh = Ch
    ).
escape_sequence(3, D, CEF, [Char|Chars], Chars, NextTyp, NextCh) :-
    D =< 0'7, !,                         % octal escape sequence
    getct(Ch, Typ),
    ( CEF = sicstus ->
        read_n_octal(Typ, Ch, 2, Ds, NextTyp, NextCh)
    ; read_octal_iso(Typ, Ch, Ds),
      getct(NextCh, NextTyp)
    ),
    number_codes(Char, 8, [D|Ds]).
escape_sequence(1, 0'x, CEF, [Char|Chars], Chars, NextTyp, NextCh) :- !,
    getct(Ch, Typ),                     % hexadecimal escape sequence
    ( CEF = sicstus ->
        read_n_hexa(Typ, Ch, 2, Ds, NextTyp, NextCh)
    ; read_hexa_iso(Typ, Ch, Ds),
      getct(NextCh, NextTyp)
    ),
    number_codes(Char, 16, [0'0|Ds]).
escape_sequence(1, 0'u, _, [Char|Chars], Chars, NextTyp, NextCh) :- !,
    getct(Ch, Typ),                     % Unicode escape sequence (4 digits)
    read_n_hexa(Typ, Ch, 4, Ds, NextTyp, NextCh), % TODO: deal with errors?
    number_codes(Char, 16, [0'0|Ds]).
escape_sequence(2, 0'U, _, [Char|Chars], Chars, NextTyp, NextCh) :- !,
    getct(Ch, Typ),                     % Unicode escape sequence (8 digits)
    read_n_hexa(Typ, Ch, 8, Ds, NextTyp, NextCh), % TODO: deal with errors?
    number_codes(Char, 16, [0'0|Ds]).
escape_sequence(1, 0'c, _, Chars, Chars, NextTyp, NextCh) :- !,
    getct1(NextCh, NextTyp).              % skip layout
escape_sequence(1, L, _, [Char|Chars], Chars, NextTyp, NextCh) :-
    symbolic_control_char(L, Char), !,   % control escape sequence
    getct(NextCh, NextTyp).
escape_sequence(_, Ch, _, [Ch|Chars], Chars, NextTyp, NextCh) :-
    getct(NextCh, NextTyp).

control_character(4, 0'?, 127).
control_character(4, 0'@, 0).
control_character(2, C, CC) :-    % This includes also "_"
    CC is C mod 32.
control_character(1, C, CC) :-
    CC is C mod 32.
control_character(4, C, CC) :-
    C >= 0'[, C =< 0'^,
    CC is C mod 32.

symbolic_control_char(0'a, 7).
symbolic_control_char(0'b, 8).
symbolic_control_char(0't, 9).
symbolic_control_char(0'n,10).
symbolic_control_char(0'v,11).
symbolic_control_char(0'f,12).
symbolic_control_char(0'r,13).
symbolic_control_char(0'e,27).
symbolic_control_char(0's,32).
symbolic_control_char(0'd,127).

% Read at most N octal digits
read_n_octal(3, D, N, [D|Ds], EndTyp, EndCh) :-
    N >= 1, D =< 0'7, !,
    getct(Ch, Typ),
    N1 is N - 1,
    read_n_octal(Typ, Ch, N1, Ds, EndTyp, EndCh).
read_n_octal(EndTyp, EndCh, _, [], EndTyp, EndCh).

% Read at most N hexadecimal digits
read_n_hexa(TD, D, N, [D|Ds], EndTyp, EndCh) :-
    N >= 1, hexa_digit(TD, D), !,
    getct(Ch, Typ),
    N1 is N - 1,
    read_n_hexa(Typ, Ch, N1, Ds, EndTyp, EndCh).
read_n_hexa(EndTyp, EndCh, _, [], EndTyp, EndCh).

read_octal_iso(4, 92, []) :- !.  % ends in \
read_octal_iso(3, D, [D|Ds]) :-
    D =< 0'7, !,
    getct(Ch, Typ),
    read_octal_iso(Typ, Ch, Ds).
read_octal_iso(_, _, Ds) :- % ignore other characters % TODO: error instead?
    getct(Ch, Typ),
    read_octal_iso(Typ, Ch, Ds).

read_hexa_iso(4, 92, []) :- !.  % ends in \
read_hexa_iso(TD, D, [D|Ds]) :-
    hexa_digit(TD, D), !,
    getct(Ch, Typ),
    read_hexa_iso(Typ, Ch, Ds).
read_hexa_iso(_, _, Ds) :- % ignore other characters % TODO: error instead?
    getct(Ch, Typ),
    read_hexa_iso(Typ, Ch, Ds).

hexa_digit(3, _).
hexa_digit(2, D) :- D =< 0'F.
hexa_digit(1, D) :- D =< 0'f.

% read_number reads an unsigned integer or float. This is the most difficult
% part of the tokenizer. There are seven forms of number:
%   <digits>                                    integer in decimal
%   <base> ' <base-digits>                      integer in other base (2..36)
%   <digits> . <digits>                         float
%   <digits> . <digits> (e|E) (-|+| ) <digits>  float with exponent
%   0.Nan                                       Not-a-number value
%   0.Inf                                       Infinite
%   0 ' <character>                             ascii code of the character
%   0 b <bin-digits>                            binary integer
%   0 o <oct-digits>                            octal integer
%   0 x <hex-digits>                            hexadecimal integer
%

read_number(0'0, N, Dict, Tokens) :- !,
    getct(Ch, Typ),
    read_after_0(Typ, Ch, N, Dict, Tokens).
read_number(D, N, Dict, Tokens) :-
    getct(Ch, Typ),
    read_digits(Typ, Ch, S, [D|S], N, Dict, Tokens).

read_after_0(3, D, N, Dict, Tokens) :- !,
    getct(Ch, Typ),
    read_digits(Typ, Ch, S, [D|S], N, Dict, Tokens).
read_after_0(4, 0'., N, Dict, Tokens) :- !,
    getct(Ch, Typ),
    read_after_period(Typ, Ch, S, [0'0|S], N, Dict, Tokens).
read_after_0(1, 0'b, N, Dict, Tokens) :- !,
    read_based_int(2, S, EndTyp, EndCh),
    based_int_or_atom(S, 2, 0'b, N, EndTyp, EndCh, Dict, Tokens).
read_after_0(1, 0'o, N, Dict, Tokens) :- !,
    read_based_int(8, S, EndTyp, EndCh),
    based_int_or_atom(S, 8, 0'o, N, EndTyp, EndCh, Dict, Tokens).
read_after_0(1, 0'x, N, Dict, Tokens) :- !,
    read_based_int(16, S, EndTyp, EndCh),
    based_int_or_atom(S, 16, 0'x, N, EndTyp, EndCh, Dict, Tokens).
read_after_0(5, 0'', N, Dict, Tokens) :- !,
    getct(Ch, Typ),
    read_quoted_character(Typ, Ch, N, Dict, Tokens).
read_after_0(Typ, Ch, 0, Dict, Tokens) :-
    read_tokens(Typ, Ch, Dict, Tokens).

read_digits(3, D, [D|S], S0, N, Dict, Tokens) :- !,
    getct(Ch, Typ),
    read_digits(Typ, Ch, S, S0, N, Dict, Tokens).
read_digits(4, 0'., S, S0, N, Dict, Tokens) :- !,
    getct(Ch, Typ),
    read_after_period(Typ, Ch, S, S0, N, Dict, Tokens).
read_digits(5, 0'', [], S0, N, Dict, Tokens) :-
    number_codes(Base, S0),
    Base >= 2,
    Base =< 36, !,
    read_based_int(Base, S1, EndTyp, EndCh),
    based_int_or_quoted(S1, Base, N, EndTyp, EndCh, Dict, Tokens).
read_digits(Typ, Ch, [], S0, N, Dict, Tokens) :-
    number_codes(N, S0),
    read_tokens(Typ, Ch, Dict, Tokens).

read_after_period(3, D, [0'.,D|S], S0, N, Dict, Tokens) :- !,
    getct(Ch, Typ),
    read_after_float(Typ, Ch, S, S0, N, Dict, Tokens).
read_after_period(2, 0'N, [], "0", Nan, Dict, Tokens) :- !,
    getct(Ch, Typ),
    read_inf_or_nan(Ch, Typ, 0'N, Nan, Dict, Tokens).
read_after_period(2, 0'I, [], "0", Inf, Dict, Tokens) :- !,
    getct(Ch, Typ),
    read_inf_or_nan(Ch, Typ, 0'I, Inf, Dict, Tokens).
read_after_period(Typ, Ch, [], S0, N, Dict, Tokens) :-
    number_codes(N, S0),
    read_fullstop(Typ, Ch, Dict, Tokens).

read_after_float(3, D, [D|S], S0, N, Dict, Tokens) :- !,
    getct(Ch, Typ),
    read_after_float(Typ, Ch, S, S0, N, Dict, Tokens).
read_after_float(1, 0'e, S, S0, N, Dict, Tokens) :- !,
    getct(Ch, Typ),
    read_after_float_e(Typ, Ch, 0'e, S, S0, N, Dict, Tokens).
read_after_float(2, 0'E, S, S0, N, Dict, Tokens) :- !,
    getct(Ch, Typ),
    read_after_float_e(Typ, Ch, 0'E, S, S0, N, Dict, Tokens).
read_after_float(Typ, Ch, [], S0, N, Dict, Tokens) :-
    number_codes(N, S0),
    read_tokens(Typ, Ch, Dict, Tokens).

read_after_float_e(3, D, E, [E,D|S], S0, N, Dict, Tokens) :- !,
    getct(Ch, Typ),
    read_after_exp(Typ, Ch, S, S0, N, Dict, Tokens).
read_after_float_e(4, 0'+, E, S, S0, N, Dict, Tokens) :- !,
    getct(Ch, Typ),
    read_after_float_e_sign(Typ, Ch, E, 0'+, S, S0, N, Dict, Tokens).
read_after_float_e(4, 0'-, E, S, S0, N, Dict, Tokens) :- !,
    getct(Ch, Typ),
    read_after_float_e_sign(Typ, Ch, E, 0'-, S, S0, N, Dict, Tokens).
read_after_float_e(Typ, Ch, E, [], S0, N, Dict, Tokens) :-
    number_codes(N, S0),
    token_start_e(E, Typ, Ch, Dict, Tokens).

read_after_float_e_sign(3, D, E, Sign, [E,Sign,D|S], S0, N, Dict, Tokens):-!,
    getct(Ch, Typ),
    read_after_exp(Typ, Ch, S, S0, N, Dict, Tokens). 
read_after_float_e_sign(Typ, Ch, E, Sign, [], S0, N, Dict, Tokens) :-
    number_codes(N, S0),
    token_start_e_sign(E, Sign, Typ, Ch, Dict, Tokens).

read_after_exp(3, D, [D|S], S0, N, Dict, Tokens) :- !,
    getct(Ch, Typ),
    read_after_exp(Typ, Ch, S, S0, N, Dict, Tokens).
read_after_exp(Typ, Ch, [], S0, N, Dict, Tokens) :-
    number_codes(N, S0),
    read_tokens(Typ, Ch, Dict, Tokens).

token_start_e(0'e, Typ, Ch, Dict, [Atom|Tokens]) :-
    read_name(Typ, Ch, S0, NextCh, NextTyp),
    atom_token([0'e|S0], Atom),
    read_tokens(NextTyp, NextCh, Dict, Tokens).
token_start_e(0'E, Typ, Ch, Dict, [var(Var,S2)|Tokens]) :-
    S = [0'E|S0],
    read_name(Typ, Ch, S0, NextCh, NextTyp),
    string_bytes(S, S2),
    dic_lookup(Dict, S2, Node),
    check_singleton(Node, Var),
    read_tokens(NextTyp, NextCh, Dict, Tokens).

token_start_e_sign(0'e, Sign, Typ, Ch, Dict, [atom(e)|Tokens]) :-
    token_start_sign(Sign, Typ, Ch, Dict, Tokens).
token_start_e_sign(0'E, Sign, Typ, Ch, Dict, [var(Var,[0'E])|Tokens]) :-
    dic_lookup(Dict, [0'E], Node),
    check_singleton(Node, Var),
    token_start_sign(Sign, Typ, Ch, Dict, Tokens).

token_start_sign(Sign, Typ, Ch, Dict, [Atom|Tokens]) :- 
    read_symbol(Typ, Ch, Chars, NextCh, NextTyp),
    atom_token([Sign|Chars], Atom),
    read_tokens(NextTyp, NextCh, Dict, Tokens).

% Maybe read 0.Inf or 0.Nan ('0'+'.'+Char0 has been read)
read_inf_or_nan(Ch, Typ, Char0, Num, Dict, Tokens) :- !,
    S = [Char0|S0],
    read_name(Typ, Ch, S0, NextCh, NextTyp),
    string_bytes(S, S2),
    ( S2 = "Nan" ->
        number_codes(Num,"0.Nan"), % Num is 0/0
        Tokens = Tokens0
    ; S2 = "Inf" ->
        number_codes(Num,"0.Inf"), % Num is 1/0
        Tokens = Tokens0
    ; % none, read as a 0+'.'+variable
      Num = 0,
      dic_lookup(Dict, S2, Node),
      check_singleton(Node, Var),
      Tokens = [atom(.),var(Var,S2)|Tokens0]
    ),
    read_tokens(NextTyp, NextCh, Dict, Tokens0).

read_based_int(Base, S, EndTyp, EndCh) :-
    MaxDigit is 0'0+Base-1,
    MaxLetter is 0'A+Base-11,
    getct(Ch, Typ),
    read_based_int_digits(Typ, Ch, MaxDigit, MaxLetter, S, EndTyp, EndCh).

read_based_int_digits(3, D, MaxDigit, MaxLetter, [D|Ds], EndTyp, EndCh) :-
    D =< MaxDigit, !,
    getct(Ch, Typ),
    read_based_int_digits(Typ, Ch, MaxDigit, MaxLetter, Ds, EndTyp, EndCh).
read_based_int_digits(2, D, MaxDigit, MaxLetter, [D|Ds], EndTyp, EndCh) :-
    D =< MaxLetter, !,
    getct(Ch, Typ),
    read_based_int_digits(Typ, Ch, MaxDigit, MaxLetter, Ds, EndTyp, EndCh).
read_based_int_digits(1, D, MaxDigit, MaxLetter, [D|Ds], EndTyp, EndCh) :-
    D =< MaxLetter+0'a-0'A, !,
    getct(Ch, Typ),
    read_based_int_digits(Typ, Ch, MaxDigit, MaxLetter, Ds, EndTyp, EndCh).
read_based_int_digits(Typ, Ch, _, _, [], Typ, Ch).

based_int_or_atom([], _, L, 0, Typ, Ch, Dict, [Atom|Tokens]) :- !,
    read_name(Typ, Ch, S0, NextCh, NextTyp),    % not based int, start of
    atom_token([L|S0], Atom),                   % atom with letter L      
    read_tokens(NextTyp, NextCh, Dict, Tokens).
based_int_or_atom(S, Base, _, N, Typ, Ch, Dict, Tokens) :-
    number_codes(N, Base, S),
    read_tokens(Typ, Ch, Dict, Tokens).

based_int_or_quoted([], Base, Base, Typ, Ch, Dict, Tokens) :- !,
    % not based int, start of quoted atom
    read_quoted(Typ, Ch, 0'', Dict, Tokens).
based_int_or_quoted(S, Base, N, Typ, Ch, Dict, Tokens) :-
    number_codes(N, Base, S),
    read_tokens(Typ, Ch, Dict, Tokens).

read_quoted_character(4, 92, N, Dict, Tokens) :- % backslash
    current_prolog_flag(character_escapes, CEF),
    CEF \== off,
    !,
    getct(Ch, Typ),
    escape_sequence(Typ, Ch, CEF, Chars, [], OtTyp, OtCh),
    continue_quoted_character(Chars, N, OtTyp, OtCh, Dict, Tokens).
read_quoted_character(5, 0'', 0'', Dict, Tokens) :-
    current_prolog_flag(character_escapes, iso),
    !,
    getct(Ch, Typ),
    read_another_quote(Typ, Ch, Dict, Tokens).
read_quoted_character(_, N, N, Dict, Tokens) :-
    getct(NextCh, NextTyp),
    read_tokens(NextTyp, NextCh, Dict, Tokens).

continue_quoted_character([], N, Typ, Ch, Dict, Tokens) :- !, % was null
    read_quoted_character(Typ, Ch, N, Dict, Tokens).
continue_quoted_character([N], N, Typ, Ch, Dict, Tokens) :-
    read_tokens(Typ, Ch, Dict, Tokens).

read_another_quote(5, 0'', Dict, Tokens) :- !,
    getct(Ch, Typ),
    read_tokens(Typ, Ch, Dict, Tokens).
read_another_quote(Typ, Ch, Dict, Tokens) :- % Accept also only a "'"
    read_tokens(Typ, Ch, Dict, Tokens).

check_singleton(Node, Var) :-
    var(Node), !, Node = [Var|_].
check_singleton([Var|[]], Var). % The [] marks it is not singleton

% TODO: speedup - use some internal atom_codes/2 that fail
atom_token(String, atom(Atom)) :-
    % atom_codes/2 may throw an exception if:
    %  - it tries to produce a very long atom and dynamic atom size
    %    is deactivated (the following assumes dynamic atom size is
    %    activated).
    %  - escaped sequence in the atom produces non valid character_code
    string_bytes(String, Bytes),
    catch(atom_codes(Atom, Bytes),
          error(representation_error(character_code), _),
          fail), !.
atom_token(String, badatom(String)).

skip_code_prot(C) :- catch(skip_code(C), _, fail).

% ===========================================================================
% Parsing comments

valid_doccomment_mark(Ch) :-
    current_prolog_flag(doccomments, Enabled), % doccomments allowed
    Enabled == on,
    doccomment_mark(Ch).

% Marks for doccomments. Those appear as the next character after a
% '%...'  or '/*...*/' comment.
doccomment_mark(0'!).
doccomment_mark(0'<).

% ('%' or '/'+'*' has been read)
read_comment(Style, Dict, Tokens) :-
    getct(NextCh, NextTyp),
    ( valid_doccomment_mark(NextCh) -> % doccomment found
        Tokens = [DocToken|Tokens0],
        read_doccomment(Style, NextCh, DocToken, Cont)
    ; Tokens = Tokens0,
      skip_comment(Style, NextTyp, NextCh, Cont)
    ),
    read_comment_cont(Cont, Dict, Tokens0).

% Continue reading comments
read_comment_cont(Cont, Dict, Tokens) :-
    ( Cont = cont_after_layout(Typ, Ch) -> % after_layout
        read_tokens_after_layout(Typ, Ch, Dict, Tokens)
    ; Cont = cont_after_layout0 -> % read next and after_layout 
        getct1(Ch, Typ),
        read_tokens_after_layout(Typ, Ch, Dict, Tokens)
    ; Cont = cont_eof_comment ->
        Tokens = ['/* ...']
    ; Cont = cont_doccomment(MarkCh), % another doccomment
      Tokens = [DocToken|Tokens0],
      read_doccomment(line, MarkCh, DocToken, Cont),
      read_comment_cont(Cont, Dict, Tokens0)
    ).

% ---------------------------------------------------------------------------
% Skip a non-documenting comment

% Skip the comment
% ('%'+NextCh has just been read)
skip_comment(line, NextTyp, NextCh, Cont) :-
    ( ( NextCh = 0'\n % no more chars in the line
      ; NextTyp = -1 % no more chars in the file
      ) ->
        Cont = cont_after_layout(NextTyp, NextCh)
    ; skip_line,
      Cont = cont_after_layout0
    ).
% ('/'+'*'+NextCh has just been read)
skip_comment(block, NextTyp, NextCh, Cont) :-
    ( NextTyp = -1 -> % end of file
        Cont = cont_eof_comment
    ; NextTyp = 4, NextCh = 0'* -> % another asterisk
        skip_comment_asterisk(Cont)
    ; skip_comment_block(Cont)
    ).

% Skip codes until '*' is found
% ('/'+'*'+... has been read)
skip_comment_block(Cont) :-
    ( skip_code_prot(0'*) ->
        skip_comment_asterisk(Cont)
    ; % end of file during comment
      Cont = cont_eof_comment
    ).

% '*' was found inside a '/'+'*'+... comment, continue read
skip_comment_asterisk(Cont) :-
    getct(Ch, Typ),
    skip_comment_asterisk_(Typ, Ch, Cont).

skip_comment_asterisk_(4, 0'/, Cont) :- !, % */ found
    Cont = cont_after_layout0.
skip_comment_asterisk_(4, 0'*, Cont) :- !, % '**' found, continue read
    skip_comment_asterisk(Cont).
skip_comment_asterisk_(_, _, Cont) :- % neither '**' nor '*/' found
    skip_comment_block(Cont).

% ---------------------------------------------------------------------------

% Read a documenting comment (doccomment)
read_doccomment(Style, MarkCh, DocToken, Cont) :-
    current_input(InStream),
    line_position(InStream, Col),
    get_doccomment_token(Style, Col, MarkCh, Chars, DocToken),
    getct(NextCh1, NextTyp1),
    read_doccomment_(NextTyp1, NextCh1, Style, Chars, Cont).

% In '%' comments, a space is added to the first line to help in
% processing of layout-sensitive code.
% TODO: We assume that all the '%' lines following '%!' comments are
%       aligned.
get_doccomment_token(line, Col, MarkCh, Chars, DocToken) :-
    Col2 is Col - 1,
    DocToken = doccomment(Col2, MarkCh, " "||Chars).
% In '/*...*/' comments, we add blanks until we reach the column
% number. That effectively aligns the first with the rest of the
% columns (to help in processing of layout-sensitive code).
get_doccomment_token(block, Col, MarkCh, Chars, DocToken) :-
    n_blanks(Col, Chars2, Chars),
    DocToken = doccomment(0, MarkCh, Chars2).

% Reads the contents of a doccomment
read_doccomment_(-1, _, line, Chars, Cont) :- !, % end of file
    Chars = [],
    Cont = cont_after_layout(-1, -1).
read_doccomment_(-1, _, _, Chars, Cont) :- !, % end of file
    Chars = [],
    Cont = cont_eof_comment.
read_doccomment_(_, 0'*, block, Chars, Cont) :- !, % '*'
    getct(Ch, Typ),
    read_doccomment_asterisk(Typ, Ch, Chars, Cont).
read_doccomment_(_, 0'\n, line, Chars, Cont) :- !, % new line
    getct(Ch, Typ),
    Chars = [0'\n|Chars0],
    read_doccomment_nl(Typ, Ch, Chars0, Cont).
read_doccomment_(_, Char, Style, [Char|Chars], Cont) :-
    getct(Ch, Typ),                     % ordinary character
    read_doccomment_(Typ, Ch, Style, Chars, Cont).

% Continue reading the next (maybe) doccomment line after a new line.
read_doccomment_nl(0, C, Chars, Cont) :-
    \+ C = 0'\n, % skip blanks except newlines
    !,
    getct(Ch, Typ),
    read_doccomment_nl(Typ, Ch, Chars, Cont).
read_doccomment_nl(_, 0'%, Chars, Cont) :- !,
    getct(Ch, Typ),
    ( valid_doccomment_mark(Ch) -> % another doccomment found
        Chars = [],
        Cont = cont_doccomment(Ch)
    ; % continue in the same doccomment
      read_doccomment_(Typ, Ch, line, Chars, Cont)
    ).
read_doccomment_nl(NextTyp, NextCh, Chars, Cont) :-
    Chars = [],
    Cont = cont_after_layout(NextTyp, NextCh).

% '*' inside a doccomment block has been read
read_doccomment_asterisk(_, NextCh0, Chars, Cont) :- NextCh0 = 0'/, !,
    Chars = [], 
    Cont = cont_after_layout0.
read_doccomment_asterisk(_, NextCh0, Chars, Cont) :-
    Chars = [0'*, NextCh0|Chars0],
    getct(Ch, Typ),
    read_doccomment_(Typ, Ch, block, Chars0, Cont).

n_blanks(I, Cs, Cs0) :- I =< 0, !, Cs = Cs0.
n_blanks(I, [0' |Cs], Cs0) :- I1 is I - 1, n_blanks(I1, Cs, Cs0).

