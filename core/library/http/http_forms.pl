:- module(http_forms, [], [assertions,isomodes,dcg,hiord,doccomments,define_flag]).

%! \title  Form Data and Query Strings
%  \author The Ciao Development Team
%
%  \module
%
%  This module implements a printer/parser for @concept{query strings}
%  and @concept{multipart/form-data} contents of HTTP requests.
%
%  Query strings are typically encoded in the URLs of @tt{GET}
%  messages. Form-data is encoded in the contents of @tt{POST}
%  messages. Both are useful to send list of name=value information in
%  HTTP requests (originally from HTML forms, lately for many other
%  uses).

:- use_module(library(strings), [whitespace0/2]).
:- use_module(library(lists), [member/2, append/3]).
:- use_module(library(http/http_grammar), [http_media_type/5, http_crlf/2, http_lines/3, http_type_params/3]).
:- use_module(library(http/multipart_form_data)).

% ---------------------------------------------------------------------------

:- export(form_dict/1).
:- prop form_dict(Dict) + regtype
   # "@var{Dict} is a dictionary of values of the attributes of a
     form.  It is a list of @tt{form_assignment}".

form_dict(Dict) :- list(Dict,form_assignment).

:- export(form_assignment/1).
:- prop form_assignment(Eq) + regtype
   # "@var{Eq} is an assignment of value of an attribute of a form.
      It is defined by:
      @includedef{form_assignment/1} @includedef{form_value/1}".

form_assignment(A=V) :-
        atm(A),
        form_value(V).

:- export(form_value/1).
:- prop form_value(V) + regtype
   # "@var{V} is a value of an attribute of a form.".

form_value(A) :- atm(A).
form_value(N) :- num(N).
form_value(L) :- list(L,string).

:- export(value_dict/1).
:- prop value_dict(Dict) + regtype
   # "@var{Dict} is a dictionary of values. It is a list of
      pairs @em{atom}=@em{constant}.".

value_dict(Dict) :- list(Dict,value_assignment).

value_assignment(A=V) :-
        atm(A),
        constant(V).

% ---------------------------------------------------------------------------

% TODO: Remove this flag? (it goes better as a parameter)

:- doc(define_flag/3,"Defines a flag as follows:
	@includedef{define_flag/3}
	(See @ref{Changing system behaviour and various flags}).

        If flag is @tt{on}, values returned by @pred{http_parse_form/2}
        are always atoms, unchanged from its original value.").

define_flag(raw_form_values, [on,off], off).

:- use_module(engine(prolog_flags), [current_prolog_flag/2]).

% ----------------------------------------------------------------------------

:- export(get_form_value/3).
:- doc(get_form_value(Dict,Var,Val), "Unifies @var{Val} with the
   value for attribute @var{Var} in dictionary @var{Dict}. Does not
   fail: value is @tt{''} if not found (this simplifies the programming
   of form handlers when they can be accessed directly).").

:- pred get_form_value(+form_dict,+atm,?form_value).

% Get value Val for attribute Var in dictionary Dic
% Does not fail: value is '' if not found.
get_form_value([],_Var,'').
get_form_value([Var=Val|_],Var,Val) :- !.
get_form_value([_|Dic],Var,Val) :- 
        get_form_value(Dic,Var,Val).

:- export(get_form_value_string/3).
:- doc(get_form_value_string(Dict,Var,Val), "Like @pred{get_form_value/3} but
   obtain @var{Val} as a string.").

:- pred get_form_value_string(+form_dict,+atm,?string).
get_form_value_string(Input, Name, String) :-
	get_form_value(Input, Name, Lines),
	( Lines = '$empty' -> String = "" % TODO: strange
	; atom(Lines) -> % TODO: This should not be needed! Fix http form support?!
	    atom_codes(Lines, String)
	; lines_to_string(Lines, String)
	).

:- export(get_form_value_atm/3).
:- doc(get_form_value_atm(Dict,Var,Val), "Like @pred{get_form_value/3} but
   obtain @var{Val} as an atom.").

:- pred get_form_value_atm(+form_dict,+atm,?atm).
get_form_value_atm(Input, Name, Value) :-
	get_form_value_string(Input, Name, Value0),
	atom_codes(Value, Value0).

:- use_module(library(lists), [append/3]).

lines_to_string([], []).
lines_to_string([S], T) :- !,
	T = S.
lines_to_string([S|Ss], T) :-
	append(S, "\n"||S0, T),
	lines_to_string(Ss, S0).

:- export(form_empty_value/1).
:- pred form_empty_value(Term)
   # "Checks that @var{Term}, a value comming from a text area is
      empty (can have spaces, newlines and linefeeds).".

% Some generic help for dealing with the very weird things that empty text 
% areas and boxes can send
form_empty_value(T) :-
        text_lines(T, Ls),
        empty_lines(Ls).

empty_lines([]).
empty_lines([L|Ls]) :-
        whitespace0(L, []),
        empty_lines(Ls), !.

:- doc(text_lines(Val,Lines), "Transforms a value @var{Val} from a
  text area to a list of lines @var{Lines}.  Not needed now,
  automatically done.").

:- pred text_lines(+form_value,-list(string)).

% Transform input from a text area to a list of lines
text_lines('$empty', []) :- !.
text_lines(A, [L]) :-
        atomic(A), !,
        name(A,L).
text_lines(T,T).

:- export(form_default/3).
:- pred form_default(+Val,+Default,-NewVal)
   # "Useful when a form is only partially filled, or when the
      executable can be invoked either by a link or by a form, to set
      form defaults. If the value of @var{Val} is empty then
      @var{NewVal}=@var{Default}, else @var{NewVal}=@var{Val}.".

% Set form defaults
form_default(Val,Default,NewVal) :- 
        ( Val == '' -> NewVal = Default; NewVal = Val).

% ----------------------------------------------------------------------------

:- export(url_query_values/2).
:- doc(url_query_values(URLArgs,Dict), "@var{Dict} is a dictionary
   of parameter values and @var{URLArgs} is the URL-encoded string of
   those assignments, which may appear after an URL pointing to a CGI
   script preceded by a '?'.  @var{Dict} is computed according to the
   @tt{raw_form_values} flag.  The use of this predicate is
   reversible.").

:- pred url_query_values(-string, +value_dict).
:- pred url_query_values(+string, -value_dict).

url_query_values(URLencoded, Dict) :-
        var(URLencoded), !,
        dic_to_urlencoded(Dict, 0'?, [0'?|URLencoded]).
url_query_values(URLencoded, Dict) :-
        urlencoded_to_dic(Dict, URLencoded).

dic_to_urlencoded([], _, "").
dic_to_urlencoded([N=V|NVs], C, [C|String]) :-
        assign_to_urlencoded(N, V, String, Rest),
        dic_to_urlencoded(NVs, 0'&, Rest).

assign_to_urlencoded(N, V, String, Rest) :-
        name(N,NS),
        name(V,VS),
        form_encode_value(NS,String,[0'=|EVS]),
        form_encode_value(VS,EVS,Rest).

:- export(form_encode_value/3).
form_encode_value([]) --> "".
form_encode_value([32|Cs]) --> !, % " " = [32]
        "+",
        form_encode_value(Cs).
form_encode_value([C|Cs]) -->
        {no_conversion(C)}, !,
        [C],
        form_encode_value(Cs).
form_encode_value([C|Cs]) -->
        {hex_chars(C,C1,C2)},
        [0'%,C1,C2],
        form_encode_value(Cs).

no_conversion(0'*).
no_conversion(0'-).
no_conversion(0'.).
no_conversion(0'_).
no_conversion(C) :- C >= 0'0, C =< 0'9, !.
no_conversion(C) :- C >= 0'@, C =< 0'Z, !.
no_conversion(C) :- C >= 0'a, C =< 0'z, !.

hex_chars(C, H, L) :-
        Hn is C >> 4,
        hex_char(Hn,H),
        Ln is C /\ 15,
        hex_char(Ln,L).

hex_char(N,C) :- N < 10, !, C is N+0'0.
hex_char(N,C) :- C is N-10+0'A.

% Converts string "name1=val1&name2=val2&...&nameN=valN" into
% list of pairs [name1='val1', name2='val2', ..., nameN='valN'], etc.
% Funny chars, eg = and & never occur in vals (they appear as
% escape sequences). Empty values are mapped to '$empty' atom.
urlencoded_to_dic(Dic, "") :- Dic = [].
urlencoded_to_dic(Dic, Cs) :-
	append(Cs, "&", Cs2), % TODO: parse without appending this
	urlencoded_to_dic_(Dic, Cs2, []).

urlencoded_to_dic_([]) --> "".
urlencoded_to_dic_([N1=V1|NVs]) -->
        chars_to(N,0'=),
        { form_decode_value(N,EN,[]),
          atom_codes(N1, EN) },
        chars_to(V,0'&),
        { form_decode_value(V,EV,[13,10]),
          http_lines(Ls, EV, []),
          lines_to_value(Ls,V1) },
        urlencoded_to_dic_(NVs).

chars_to([],C) --> [C].
chars_to([C|Cs],D) -->
        [C],
        {C \== D},
        chars_to(Cs,D).

:- export(form_decode_value/3).
% Decode a string encoded with form_encode_value/3
% (Expands escape sequences and converts "+" back into " " in a string)
form_decode_value([]) --> "".
form_decode_value([0'+|Cs]) --> !,
        " ",
        form_decode_value(Cs).
form_decode_value([0'%,C1,C2|Cs]) --> !,
        { hex_digit(C1,D1),
          hex_digit(C2,D2),
          C is D1 * 16 + D2},
        [C],
        form_decode_value(Cs).
form_decode_value([C|Cs]) -->
        [C],
        form_decode_value(Cs).

hex_digit(C, D) :-
        (C >= 0'A ->
          D is ((C /\ 223) - 0'A) + 10 % 223 = bin(11011111)
        ;
          D is C - 0'0
        ).

:- export(lines_to_value/2).
% list of lines -> value
lines_to_value([L|Ls], V) :-
        lines_to_value_(Ls, L, V).

lines_to_value_(Ls, L, V) :-
        ( current_prolog_flag(raw_form_values, on) ->
	    lines_to_value_raw(Ls, L, V)
	; lines_to_value_cooked(Ls, L, V)
	).

lines_to_value_raw([], L, V) :- !,
        atom_codes(V, L).
lines_to_value_raw(Ls, L, V) :-
        append_lines(Ls, L, Lines),
        atom_codes(V, Lines).

append_lines([], L, L).
append_lines([L|Ls], Line, Lines) :-
        append(Line, "\n"||Tail, Lines),
        append_lines(Ls, L, Tail).

lines_to_value_cooked([], [], '$empty') :- !.
lines_to_value_cooked([], L, V) :- !,
        name(V, L).               % if only a line, return an atom or number
lines_to_value_cooked(Ls, L, [L|Ls]).   % else, return the list of lines

% ===========================================================================

:- export(http_parse_form/2).
:- pred http_parse_form(Request, Dic) # "Get form data @var{Dic} from
   HTTP request @var{Request}".

http_parse_form(Request, Dic) :-
	( member(method(Method), Request) -> true ; fail ),
        http_parse_form_method(Method, Request, Dic), !.
http_parse_form(_, []).

get_query_string(Request, QueryString) :-
	member(uri(URIStr), Request),
        append(_, "?"||Q, URIStr),
	!,
	QueryString = Q.
get_query_string(_Request, "").

http_parse_form_method(get, Request, Dic) :-
        get_query_string(Request, Q),
	urlencoded_to_dic(Dic, Q),
        !.
http_parse_form_method(post, Request, Dic) :-
	( member(content_type(Type,Subtype,Params), Request) -> true ; fail ),
        http_parse_form_of_type(Type,Subtype,Request,Params,Dic), !.
http_parse_form_method(M, _, _) :-
        throw(error(unknown_request_method_or_bad_request(M), http_parse_form_method/4)).

http_parse_form_of_type(application, 'x-www-form-urlencoded', Request, _, Dic) :-
        ( member(content(Cs), Request) -> true ; fail ),
	urlencoded_to_dic(Dic, Cs),
	!.
% TODO: implement multipart in POST --- try not read the contents!
http_parse_form_of_type(multipart, 'form-data', Request, Params, Dic) :-
        ( member(content(Cs), Request) -> true ; fail ),
        member((boundary=B), Params),
	parse_multipart_form_data(Cs, B, Dic),
	!.
http_parse_form_of_type(Type,Subtype,_,_,_) :-
        throw(error(unknown_content_type(Type, Subtype), http_parse_form_of_type/4)).

% ----------------------------------------------------------------------------
