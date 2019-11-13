:- module(http_grammar, [], [assertions, isomodes, dcg, doccomments]).

%! \title  Common grammar definitions for HTTP
%  \author The Ciao Development Team
%
%  \module
%
%  Some grammar definitions for the HTTP protocol.

:- use_module(library(strings), [string/3]).

% Quick check to determine if the DCG is executed for parsing or printing
:- export('PARSING'/2).
'PARSING'(Cs, Cs0) :- nonvar(Cs), Cs = Cs0.
:- export('PRINTING'/2).
'PRINTING'(Cs, Cs0) :- var(Cs), Cs = Cs0.

% ---------------------------------------------------------------------------
% Blanks, digits, alphanum

:- export(loalpha/3).
loalpha(C) --> [C], {C >= 0'a, C =< 0'z}.

:- export(upalpha/3).
upalpha(C) --> [C], {C >= 0'A, C =< 0'Z}.

:- export(digit/3).
digit(C) --> [C], {C >= 0'0, C =< 0'9}.

:- export(http_sp/2).
http_sp --> 'PRINTING', !, " ".
http_sp --> [32], !, http_sp0.
http_sp --> [9], http_sp0.

http_sp0 --> http_sp, !.
http_sp0 --> [].

:- export(http_lws/2).
http_lws -->
    http_sp, !.
http_lws -->
    http_crlf,
    http_sp.

:- export(http_lws0/2).
http_lws0 --> 'PRINTING', !.
http_lws0 --> "".
http_lws0 --> http_lws.

:- export(http_crlf/2).
http_crlf --> [13,10], !.
http_crlf --> [10].

% ----------------------------------------------------------------------------
% Integers

:- export(integer_str/3).
integer_str(L) --> 'PRINTING', !,
    { number_codes(L,Codes) },
    string(Codes).
integer_str(L) -->
    parse_integer(L).

:- export(parse_integer/3). % (exported for url.pl)
parse_integer(N) -->
    digit(D),
    parse_integer_rest(Ds),
    { number_codes(N,[D|Ds]) }.

parse_integer_rest([D|Ds]) -->
    digit(D),
    parse_integer_rest(Ds).
parse_integer_rest([]) --> "".

% ---------------------------------------------------------------------------
% Tokens

:- export(http_token/3).
http_token(T) --> 'PRINTING', !,
    { atom_codes(T, Codes) },
    string(Codes).
http_token(T) -->
    http_token_str(St),
    { atom_codes(T, St) }.

http_token_str([C|Cs]) -->
    http_token_char(C),
    http_token_rest(Cs).

http_token_rest([C|Cs]) -->
    http_token_char(C),
    http_token_rest(Cs).
http_token_rest([]) --> "".

http_token_char(C) --> loalpha(C), !.
http_token_char(C) --> upalpha(C), !.
http_token_char(C) --> digit(C), !.
http_token_char(C) --> http_token_symb(C).

http_token_symb(0'!) --> "!".
http_token_symb(0'#) --> "#".
http_token_symb(0'$) --> "$".
http_token_symb(0'%) --> "%".
http_token_symb(0'&) --> "&".
http_token_symb(0'') --> "'".
http_token_symb(0'*) --> "*".
http_token_symb(0'+) --> "+".
http_token_symb(0'-) --> "-".
http_token_symb(0'.) --> ".".
http_token_symb(0'^) --> "^".
http_token_symb(0'_) --> "_".
http_token_symb(0'`) --> "`".
http_token_symb(0'|) --> "|".
http_token_symb(0'~) --> "~".

http_quoted_string(S) -->
    """",
    http_qs_text(S).

http_qs_text([]) -->
    """", !.
http_qs_text([X|T]) -->
    [X],
    http_qs_text(T).

% ---------------------------------------------------------------------------
% Tokens (lowercase)

:- export(http_lo_up_token/3).
http_lo_up_token(T) --> 'PRINTING', !,
    { atom_codes(T, Codes) },
    string(Codes).
http_lo_up_token(T) -->
    http_lo_up_token_char(C),
    http_lo_up_token_rest(Cs),
    { atom_codes(T, [C|Cs]) }.

http_lo_up_token_rest([C|Cs]) -->
    http_lo_up_token_char(C), !,
    http_lo_up_token_rest(Cs).
http_lo_up_token_rest([]) --> "".

http_lo_up_token_char(C) --> loupalpha(C), !.
http_lo_up_token_char(C) --> digit(C), !.
http_lo_up_token_char(C) --> http_token_symb(C).

loupalpha(C) --> loalpha(C), !.
loupalpha(C) --> upalpha(CU), { C is CU+0'a-0'A }.

% ---------------------------------------------------------------------------
% Optionally quoted tokens

http_token_or_quoted(V) --> http_token_str(V), !. % TODO: what it this fail in 'PRINTING' mode?
http_token_or_quoted(V) --> http_quoted_string(V).

% ---------------------------------------------------------------------------
% Lines

:- export(http_line/3).
http_line([]) --> http_crlf, !.
http_line([X|T]) --> [X], http_line(T).

:- export(http_line_atm/3).
% (line as an atom)
http_line_atm(X) --> 'PRINTING', !,
    { atom_codes(X,Str) },
    http_line(Str).
http_line_atm(X) -->
    http_line(Str),
    { atom_codes(X,Str) }.

:- export(http_lines/3).
% :- pred http_lines(Lines, String, Tail) :: list(string) * string * string
%    # "@var{Lines} is a list of the lines with occur in @var{String}
%       until @var{Tail}.  The lines may end Unix-style or DOS-style
%       in @var{String}, in @var{Lines} they have not end of line
%       characters. Suitable to be used in DCGs.".

http_lines([L|Ls]) --> http_line(L), !, http_lines(Ls).
http_lines([]) --> "".

% ----------------------------------------------------------------------------
% Fields

:- export(http_field/3).
http_field(T) -->
    http_lo_up_token(T),
    ":", http_lws.

% ----------------------------------------------------------------------------
% Auth-params

:- export(http_auth_params/3).
http_auth_params([P|Ps]) -->
    http_auth_param(P), http_lws0,
    http_auth_params_rest(Ps).
http_auth_params([]) --> "".

http_auth_params_rest([P|Ps]) -->
    ",", http_lws0,
    http_auth_param(P), http_lws0,
    http_auth_params(Ps).
http_auth_params_rest([]) --> "".

http_auth_param(P=V) -->
    http_lo_up_token(P),
    "=",
    http_quoted_string(V).

% ---------------------------------------------------------------------------
% HTTP protocol string

:- export(http_http/4).
http_http(Major, Minor) -->
    "HTTP/", integer_str(Major), ".", integer_str(Minor).

% ----------------------------------------------------------------------------
% Status

:- export(http_status_line/3).
http_status_line(status(Ty,SC,RP)) -->
    % TODO: change version for 'PRINTING'? implement HTTP/1.1? 
    % TODO: store version on 'PARSING'?
    ( 'PRINTING' -> { Major = 1, Minor = 0 } ; [] ),
    http_http(Major, Minor),
    http_sp,
    http_status_code(Ty,SC),
    http_sp,
    http_line(RP), !.

http_status_code(Ty,SC) -->
    [X,Y,Z],
    {
        type_of_status_code(X,Ty), !,
        number_codes(SC,[X,Y,Z])
    }.

type_of_status_code(0'1, informational) :- !.
type_of_status_code(0'2, success) :- !.
type_of_status_code(0'3, redirection) :- !.
type_of_status_code(0'4, request_error) :- !.
type_of_status_code(0'5, server_error) :- !.
%??? type_of_status_code(_, extension_code).

% ---------------------------------------------------------------------------
% Media type

:- export(http_media_type/5).
http_media_type(Type,SubType,Params) -->
    http_lo_up_token(Type),
    "/",
    http_lo_up_token(SubType),
    http_lws0,
    http_type_params(Params).

:- export(http_type_params/3). % (exported for multipart)
http_type_params([P|Ps]) -->
    ";", http_lws0,
    http_type_param(P), http_lws0,
    http_type_params(Ps).
http_type_params([]) --> "".

http_type_param(A = V) --> 'PRINTING', !, % TODO: INCONSISTENT!!!!!!!! V should be atm or str in both modes
    http_lo_up_token(A),
    "=",
    http_token(V).
http_type_param(A = V) -->
    http_lo_up_token(A),
    "=",
    http_token_or_quoted(V).

