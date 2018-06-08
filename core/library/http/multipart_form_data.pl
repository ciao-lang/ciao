:- module(multipart_form_data, [], [assertions, regtypes, isomodes, dcg, hiord, doccomments]).

%! \title  Multipart Form data
%  \author The Ciao Development Team
%
%  \module
%
%  Parsing of multipart/form-data media type (RFC7578).

:- use_module(library(lists), [append/3]).
:- use_module(library(http/http_forms), [lines_to_value/2]).
:- use_module(library(http/http_grammar)).

:- export(parse_multipart_form_data/3).
:- pred parse_multipart_form_data(Cs, B, Dic)
   # "Parse @var{Cs} content string as multipart/form-data with
      @var{B} as boundary delimiter. Store the contents as
      @tt{Name=Value} pairs in @var{Dic}.".

parse_multipart_form_data(Cs, B, Dic) :-
        Boundary = "--"||B,
	parse_multipart_form_data0(Boundary, Dic, Cs, []).

parse_multipart_form_data0(Boundary, Dic) -->
        parse_lines_to_boundary(Boundary, _, Stop),
        parse_multipart_form_data_(Stop, Boundary, Dic).

parse_multipart_form_data_(stop, _, []) --> [].
parse_multipart_form_data_(continue, Boundary, [Name=Value|NVs]) -->
        parse_multipart_header(HeadLines),
        { extract_name_type(HeadLines, Name, Type) },
        parse_multipart_value(Type, Boundary, Stop, Value),
        parse_multipart_form_data_(Stop, Boundary, NVs).

parse_multipart_header(Lines) -->
        parse_line(Line, []),
        ( { Line = [] } -> { Lines = [] }
	; { Lines = [Line|Lines2] },
	  parse_multipart_header(Lines2)
	).

extract_name_type(HLs, N, T) :-
        member(HL, HLs),
        content_disposition_header(Params, HL, []),
        extract_name(Params, N),
        extract_type(Params, T), !.

content_disposition_header(Params) -->
        "Content-Disposition: form-data",
        http_type_params(Params).

extract_name(Params, N) :-
        member((name=NS), Params), !,
        atom_codes(N, NS).

extract_type(Params, T) :-
        ( member((filename=FS), Params) ->
            atom_codes(F, FS),
            T = file(F)
        ; T = data
        ).

parse_multipart_value(data, Boundary, Stop, Value) -->
        parse_lines_to_boundary(Boundary, Lines, Stop),
	% TODO: append lines? (then we just need the _raw version); split in lines_to_value/2 if needed
        { lines_to_value(Lines, Value) }.
parse_multipart_value(file(F), Boundary, Stop, file(F,Content)) -->
	parse_lines_to_boundary_raw(Boundary, Content, Stop).

parse_lines_to_boundary(Boundary, Lines, Stop) -->
        parse_line(Line, []),
        ( { check_boundary(Boundary, Stop, Line, []) } ->
	    { Lines = [] }
	; { Lines = [Line|Lines2] },
	  parse_lines_to_boundary(Boundary, Lines2, Stop)
	).

parse_lines_to_boundary_raw(Boundary, Content, Stop) -->
        parse_line_raw(Line, Tail),
        ( { check_boundary_raw(Boundary, Stop, Line, []) } ->
	    { Content = [] }
	; { Content = Line },
	  parse_lines_to_boundary_raw(Boundary, Tail, Stop)
	).

check_boundary(Boundary, Stop) -->
        check_str(Boundary),
        check_end(Stop).

check_boundary_raw(Boundary, Stop) -->
	check_str(Boundary),
	check_end(Stop),
	http_crlf.

check_end(stop) --> "--".
check_end(continue) --> [].

check_str(Str, Cs, Cs0) :-
	append(Str, Cs0, Cs).

% ---------------------------------------------------------------------------

% Parse a line (stops at crlf), do not include crlf characters
parse_line(Cs, Tail) --> 
	( http_crlf -> { Cs = Tail }
	; [C] ->
	    { Cs = [C|Cs2] },
	    parse_line(Cs2, Tail)
	; { Cs = Tail }
	).

% Parse a line (stops at \n), include \n
parse_line_raw(Cs, Tail) -->
        ( [C] ->
	    { Cs = [C|Cs2] },
	    ( { C = 0'\n } -> { Cs2 = Tail }
            ; parse_line_raw(Cs2, Tail)
            )
	; { Cs = Tail }
	).
