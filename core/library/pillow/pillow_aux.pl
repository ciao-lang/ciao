/**** Be careful when changing code, to not break auto distribution generation
 ****/
:- module(pillow_aux, [
        http_lws0/2, http_lws/2, http_crlf/2, http_sp/2, http_line/3,
        http_media_type/5, http_type_params/3, http_lo_up_token/3,
        http_lo_up_token_char/3, http_lo_up_token_rest/3, loupalpha/3,
        loalpha/3, upalpha/3, digit/3, parse_integer/3, http_token/3,
        http_quoted_string/3
        ], [dcg]).

%%% HTTP and basic parsing %%%

http_media_type(Type,SubType,Params) -->
        http_lo_up_token(Type),
        "/",
        http_lo_up_token(SubType),
        http_lws0,
        http_type_params(Params).

http_type_params([P|Ps]) -->
        ";", http_lws0,
        http_type_param(P), http_lws0,
        http_type_params(Ps).
http_type_params([]) --> "".

http_type_param(A = V) -->
        http_lo_up_token(A),
        "=",
        http_token_or_quoted(V).

http_token_or_quoted(V) --> http_token_str(V), !.
http_token_or_quoted(V) --> http_quoted_string(V).

http_token_str([C|Cs]) -->
        http_token_char(C),
        http_token_rest(Cs).

http_token(T) -->
        http_token_str(St),
        {
            atom_codes(T, St)
        }.

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

% ----------------------------------------------------------------------------

parse_integer(N) -->
        digit(D),
        parse_integer_rest(Ds),
        {
            number_codes(N,[D|Ds])
        }.

parse_integer_rest([D|Ds]) -->
        digit(D),
        parse_integer_rest(Ds).
parse_integer_rest([]) --> "".

http_lo_up_token(T) -->
        http_lo_up_token_char(C),
        http_lo_up_token_rest(Cs),
        {
            atom_codes(T, [C|Cs])
        }.

http_lo_up_token_rest([C|Cs]) -->
        http_lo_up_token_char(C),
        http_lo_up_token_rest(Cs).
http_lo_up_token_rest([]) --> "".

http_lo_up_token_char(C) --> loupalpha(C), !.
http_lo_up_token_char(C) --> digit(C), !.
http_lo_up_token_char(C) --> http_token_symb(C).

loupalpha(C) --> loalpha(C), !.
loupalpha(C) --> upalpha(CU), { C is CU+0'a-0'A }.

loalpha(C) --> [C], {C >= 0'a, C =< 0'z}.

upalpha(C) --> [C], {C >= 0'A, C =< 0'Z}.

digit(C) --> [C], {C >= 0'0, C =< 0'9}.

http_line([]) -->
        http_crlf, !.
http_line([X|T]) -->
        [X],
        http_line(T).

http_sp -->
        [32], !,
        http_sp0.
http_sp -->
        [9],
        http_sp0.

http_sp0 --> http_sp, !.
http_sp0 --> [].

http_lws -->
        http_sp, !.
http_lws -->
        http_crlf,
        http_sp.

http_lws0 --> "".
http_lws0 --> http_lws.


http_crlf -->
        [13,10], !.
http_crlf -->
        [10].
