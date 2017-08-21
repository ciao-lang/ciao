:- module(cgi, [
        form_dict/1, form_assignment/1, form_value/1, value_dict/1,
	%
        html_report_error/1, get_form_input/1, get_form_value/3,
        form_empty_value/1, form_default/3, % text_lines/2, 
        set_cookie/2, get_cookies/1,
        url_query_amp/2, url_query_values/2,
        my_url/1,
        form_request_method/1, html_protect/1
        ], [assertions,isomodes,dcg,define_flag]).

:- doc(title, "CGI programming").
:- doc(author, "Ciao Development Team").

:- doc(module, "This module implements the predicates for
   @concept{CGI} and form handlers programming.").

:- use_module(library(strings), [whitespace0/2]).
:- use_module(library(strings), [get_line/1]).
:- use_module(library(lists), [append/3]).
:- use_module(library(system), [getenvstr/2]).
:- use_module(library(pillow/html), [output_html/1]).
:- use_module(library(pillow/pillow_aux)).

% ---------------------------------------------------------------------------

:- true prop form_dict(Dict) + regtype
        # "@var{Dict} is a dictionary of values of the attributes of a
          form.  It is a list of @tt{form_assignment}".

form_dict(Dict) :- list(Dict,form_assignment).

:- true prop form_assignment(Eq) + regtype
        # "@var{Eq} is an assignment of value of an attribute of a form.
          It is defined by:
          @includedef{form_assignment/1} @includedef{form_value/1}".

form_assignment(A=V) :-
        atm(A),
        form_value(V).

:- true prop form_value(V) + regtype
        # "@var{V} is a value of an attribute of a form.".

form_value(A) :- atm(A).
form_value(N) :- num(N).
form_value(L) :- list(L,string).

:- true prop value_dict(Dict) + regtype
        # "@var{Dict} is a dictionary of values. It is a list of
           pairs @em{atom}=@em{constant}.".

value_dict(Dict) :- list(Dict,value_assignment).

value_assignment(A=V) :-
        atm(A),
        constant(V).

% ---------------------------------------------------------------------------

:- doc(define_flag/3,"Defines a flag as follows:
	@includedef{define_flag/3}
	(See @ref{Changing system behaviour and various flags}).

        If flag is @tt{on}, values returned by @pred{get_form_input/1}
        are always atoms, unchanged from its original value.").

define_flag(raw_form_values, [on,off], off).

% ---------------------------------------------------------------------------
% Parsing of forms input

:- doc(get_form_input(Dict), "Translates input from the form (with
   either the POST or GET methods, and even with CONTENT_TYPE
   multipart/form-data) to a dictionary @var{Dict} of
   @em{attribute}=@em{value} pairs. If the flag @tt{raw_form_values} is
   @tt{off} (which is the default state), it translates empty values
   (which indicate only the presence of an attribute) to the atom
   @tt{'$empty'}, values with more than one line (from text areas or
   files) to a list of lines as strings, the rest to atoms or numbers
   (using @pred{name/2}).  If the flag @tt{on}, it gives all values as
   atoms, without translations.").

:- true pred get_form_input(-form_dict).

get_form_input(Dic) :-
        form_request_method(M),
        get_form_input_method(M, Dic), !.
get_form_input([]).

get_form_input_method('GET', Dic) :-
        ( getenvstr('QUERY_STRING',Q), Q \== [] ->
            append(Q,"&",Cs),
            form_urlencoded_to_dic(Dic, Cs, [])
        ; Dic = []
        ), !.
get_form_input_method('POST', Dic) :-
        getenvstr('CONTENT_TYPE', ContentType),
        http_media_type(Type,Subtype,Params,ContentType,[]),
        get_form_input_of_type(Type,Subtype,Params,Dic), !.
get_form_input_method(M, _) :-
        html_report_error(['Unknown request method ', tt(M),
                           ' or bad request.']).

get_form_input_of_type(application, 'x-www-form-urlencoded', _, Dic) :-
        getenvstr('CONTENT_LENGTH', N),
        number_codes(No,N),
        ( No > 0 ->
            read_all(No,Cs,"&"),
            form_urlencoded_to_dic(Dic, Cs, [])
        ; Dic = []
        ), !.
get_form_input_of_type(multipart, 'form-data', Params, Dic) :-
        member((boundary=B), Params),
        Boundary = [0'-,0'-|B],
        get_lines_to_boundary(Boundary, _, End),
        get_multipart_form_data(End, Boundary, Dic), !.
get_form_input_of_type(Type,Subtype,_,_) :-
        html_report_error(['Unknown Content-type ',tt([Type,"/",Subtype]),
                           ' or bad request.']).

% read N chars from input (N>=0)
read_all(0) --> !, "".
read_all(N) -->
        {get_code(C)},
        [C],
        {N1 is N - 1},
        read_all(N1).

% Converts string "name1=val1&name2=val2&name3=&" into
% list of pairs [name1='val1', name2='val2', name3='$empty'] etc
% Funny chars, eg = and & never occur in vals (they appear as
% escape sequences)
form_urlencoded_to_dic([]) --> "".
form_urlencoded_to_dic([N1=V1|NVs]) -->
        chars_to(N,0'=),
        {expand_esc_plus(N,EN,[]),
         atom_codes(N1, EN)},
        chars_to(V,0'&),
        {expand_esc_plus(V,EV,[13,10]),
         http_lines(Ls, EV, []),
         to_value(Ls,V1)},
        form_urlencoded_to_dic(NVs).

chars_to([],C) --> [C].
chars_to([C|Cs],D) -->
        [C],
        {C \== D},
        chars_to(Cs,D).

% Expands escape sequences and converts "+" back into " " in a string
expand_esc_plus([]) --> "".
expand_esc_plus([0'+|Cs]) --> !,
        " ",
        expand_esc_plus(Cs).
expand_esc_plus([0'%,C1,C2|Cs]) --> !,
        {hex_digit(C1,D1),
         hex_digit(C2,D2),
         C is D1 * 16 + D2},
        [C],
        expand_esc_plus(Cs).
expand_esc_plus([C|Cs]) -->
        [C],
        expand_esc_plus(Cs).

hex_digit(C, D) :-
        (C >= 0'A ->
          D is ((C /\ 223) - 0'A) + 10 % 223 = bin(11011111)
        ;
          D is C - 0'0
        ).

to_value([L|Ls], V) :-
        to_value_(Ls, L, V).

to_value_(Ls, L, V) :-
        current_prolog_flag(raw_form_values, on), !,
        to_value_raw(Ls, L, V).
to_value_(Ls, L, V) :-
        to_value_cooked(Ls, L, V).

to_value_raw([], L, V) :- !,
        atom_codes(V, L).
to_value_raw(Ls, L, V) :-
        append_lines(Ls, L, Lines),
        atom_codes(V, Lines).

append_lines([], L, L).
append_lines([L|Ls], Line, Lines) :-
        append(Line, "\n"||Tail, Lines),
        append_lines(Ls, L, Tail).

to_value_cooked([], [], '$empty') :- !.
to_value_cooked([], L, V) :- !,
        name(V, L).               % if only a line, return an atom or number
to_value_cooked(Ls, L, [L|Ls]).   % else, return the list of lines

% ----------------------------------------------------------------------------

get_multipart_form_data(end, _, []).
get_multipart_form_data(continue, Boundary, [Name=Value|NVs]) :-
        get_m_f_d_header(HeadLines),
        extract_name_type(HeadLines, Name, Type),
        get_m_f_d_value(Type, Boundary, End, Value),
        get_multipart_form_data(End, Boundary, NVs).

get_m_f_d_header(Lines) :-
        get_line(Line),
        get_m_f_d_header_(Line, Lines).

get_m_f_d_header_([], []) :- !.
get_m_f_d_header_(Line, [Line|Lines]) :-
        get_line(Line1),
        get_m_f_d_header_(Line1, Lines).

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
        (
          member((filename=FS), Params) ->
              atom_codes(F, FS),
              T = file(F)
        ; T = data
        ).

get_m_f_d_value(data, Boundary, End, Value) :-
        get_lines_to_boundary(Boundary, Lines, End),
        to_value(Lines, Value).
get_m_f_d_value(file(F), Boundary, End, file(F,Content)) :-
        get_line_raw(Line, Tail),
        get_lines_to_boundary_raw(Line, Tail, Boundary, Content, End).

get_lines_to_boundary(Boundary, Lines, End) :-
        get_line(Line),
        get_lines_to_boundary_(Line, Boundary, Lines, End).

get_lines_to_boundary_(Line, Boundary, Lines, End) :-
        append(Boundary, R, Line),
        check_end(R, End), !,
        Lines = [].
get_lines_to_boundary_(Line, Boundary, [Line|Lines], End) :-
        get_line(OtherLine),
        get_lines_to_boundary_(OtherLine, Boundary, Lines, End).

check_end("--", end).
check_end([], continue).

get_line_raw([C|Cs], Tail) :-
        get_code(C),
        get_line_raw_after(C, Cs, Tail).

get_line_raw_after(0'\n, Tail, Tail) :- !.
get_line_raw_after(_, [C|Cs], Tail) :-
        get_code(C),
        get_line_raw_after(C, Cs, Tail).

get_lines_to_boundary_raw(Line, _Tail, Boundary, Content, End) :-
        append(Boundary, R, Line),
        check_end_raw(End, R, []), !,
        Content = [].
get_lines_to_boundary_raw(Line, Tail, Boundary, Line, End) :-
        get_line_raw(Line1, Tail1),
        get_lines_to_boundary_raw(Line1, Tail1, Boundary, Tail, End).

check_end_raw(end) --> "--", http_crlf.
check_end_raw(continue) -->  http_crlf.

% ----------------------------------------------------------------------------

:- doc(get_form_value(Dict,Var,Val), "Unifies @var{Val} with the
   value for attribute @var{Var} in dictionary @var{Dict}. Does not
   fail: value is @tt{''} if not found (this simplifies the programming
   of form handlers when they can be accessed directly).").

:- true pred get_form_value(+form_dict,+atm,?form_value).

% Get value Val for attribute Var in dictionary Dic
% Does not fail: value is '' if not found.
get_form_value([],_Var,'').
get_form_value([Var=Val|_],Var,Val) :- !.
get_form_value([_|Dic],Var,Val) :- 
        get_form_value(Dic,Var,Val).

:- doc(text_lines(Val,Lines), "Transforms a value @var{Val} from a
  text area to a list of lines @var{Lines}.  Not needed now,
  automatically done.").

:- true pred text_lines(+form_value,-list(string)).

% Transform input from a text area to a list of lines - not needed now
text_lines('$empty', []) :- !.
text_lines(A, [L]) :-
        atomic(A), !,
        name(A,L).
text_lines(T,T).

:- true pred form_empty_value(Term)
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

:- true pred form_default(+Val,+Default,-NewVal)
   # "Useful when a form is only partially filled, or when the
      executable can be invoked either by a link or by a form, to set
      form defaults. If the value of @var{Val} is empty then
      @var{NewVal}=@var{Default}, else @var{NewVal}=@var{Val}.".

% Set form defaults
form_default(Val,Default,NewVal) :- 
        ( Val == '' -> NewVal = Default; NewVal = Val).

:- true pred form_request_method(Method) => atm
        # "Unifies @var{Method} with the method of invocation of the form
           handler (@tt{GET} or @tt{POST}).".

form_request_method(M) :-
        getenvstr('REQUEST_METHOD', MS),
        atom_codes(M,MS).

% ---------------------------------------------------------------------------

:- doc(my_url(URL), "Unifies @var{URL} with the Uniform
   Resource Locator (WWW address) of this cgi executable.").

:- true pred my_url(?string).

my_url(URL) :-
        getenvstr('SERVER_NAME', Server),
        getenvstr('SCRIPT_NAME', File),
        getenvstr('SERVER_PORT', Port),
        (
            Port = "80" ->
                mappend(["http://",Server,File], URL)
        ;   mappend(["http://",Server,[0':|Port],File], URL)
        ).

% ---------------------------------------------------------------------------
% Cookies (contributed by Samir Genaim)

% sending a cookie is done by printing
%
%  Set-Cookie: var=value
%
% before sending Content-Type

:- doc(set_cookie(Name,Value), "Sets a cookie of name @var{Name} and
   value @var{Value}.  Must be invoked before outputting any data,
   including the @tt{cgi_reply} html-term.").

:- true pred set_cookie(+atm,+constant).

set_cookie(Name,Value) :-
        name(Value, String),
        encoded_value(String, EValue, []),
	display_list(['Set-Cookie: ',Name,'=']),
        display_string(EValue),
        nl.

:- doc(get_cookies(Cookies), "Unifies @var{Cookies} with a
   dictionary of @em{attribute}=@em{value} pairs of the active cookies
   for this URL.  If the flag @tt{raw_form_values} is @tt{on},
   @em{value}s are always atoms even if they could be interpreted as
   numbers.").

:- true pred get_cookies(-value_dict).

% Cookies are available in the environment variable "HTTP_COOKIE".
% The cookies string is of the form:
% 
%      var1=val1; var2=val2; ..... varn=valn

get_cookies(Cs) :-
	getenvstr('HTTP_COOKIE',CookiesStr),
	cookies(Cs,[0';,0' |CookiesStr],[]), !.
get_cookies([]).

cookies([]) --> "".
cookies([C=V|Cs]) -->
	"; ",
	cookie_str(StrC),
	"=",
	cookie_str(StrV),
	{
          atom_codes(C,StrC),
          expand_esc_plus(StrV,UStrV,[]),
          ( current_prolog_flag(raw_form_values, on) ->
              atom_codes(V,UStrV)
          ; name(V,UStrV)
          )
        },
	cookies(Cs).

cookie_str([C|Cs]) -->
	legal_cookie_char(C),
	cookie_str(Cs), !.
cookie_str([C]) -->
	legal_cookie_char(C).

legal_cookie_char(C) -->
	[C],
	{C \== 0';, C\== 0'=}.

% ----------------------------------------------------------------------------

%% To compute GET parameters for CGI's
%  -- from an idea of Markus Fromherz <fromherz@parc.xerox.com> */

:- doc(url_query_amp(Dict,URLArgs), "Translates a dictionary
   @var{Dict} of parameter values into a string @var{URLArgs} for
   appending to a URL pointing to a form handler to be used in the href
   of a link (uses &amp; instead of &).").

:- true pred url_query_amp(+value_dict,-string).

url_query_amp(Args, [0'?|URLArgs]) :-
        params_to_string_amp(Args, URLArgs).

:- doc(url_query_values(Dict,URLArgs), "@var{Dict} is a dictionary
   of parameter values and @var{URLArgs} is the URL-encoded string of
   those assignments, which may appear after an URL pointing to a CGI
   script preceded by a '?'.  @var{Dict} is computed according to the
   @tt{raw_form_values} flag.  The use of this predicate is
   reversible.").

:- true pred url_query_values(+value_dict,-string).
:- true pred url_query_values(-value_dict,+string).

url_query_values(URLencoded, Dict) :-
        var(URLencoded), !,
        params_to_string(Dict, 0'?, [0'?|URLencoded]).
url_query_values(URLencoded, Dict) :-
        append(URLencoded, "&", Values),
        form_urlencoded_to_dic(Dict, Values, []).

params_to_string([], _, "").
params_to_string([N=V|NVs], C, [C|String]) :-
        param_to_string(N, V, String, Rest),
        params_to_string(NVs, 0'&, Rest).

params_to_string_amp([], "").
params_to_string_amp([N=V|NVs], String) :-
        param_to_string(N, V, String, Rest),
        params_to_string_amp_(NVs, Rest).

params_to_string_amp_([], "").
params_to_string_amp_([N=V|NVs], [0'&,0'a,0'm,0'p,0';|String]) :-
        param_to_string(N, V, String, Rest),
        params_to_string_amp_(NVs, Rest).

param_to_string(N, V, String, Rest) :-
        name(N,NS),
        name(V,VS),
        encoded_value(NS,String,[0'=|EVS]),
        encoded_value(VS,EVS,Rest).

encoded_value([]) --> "".
encoded_value([32|Cs]) --> !, % " " = [32]
        "+",
        encoded_value(Cs).
encoded_value([C|Cs]) -->
        {no_conversion(C)}, !,
        [C],
        encoded_value(Cs).
encoded_value([C|Cs]) -->
        {hex_chars(C,C1,C2)},
        [0'%,C1,C2],
        encoded_value(Cs).

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

% ---------------------------------------------------------------------------

:- doc(html_protect(Goal), "Calls @var{Goal}.  If an error occurs
   during its execution, or it fails, an HTML page is output informing
   about the incident.  Normaly the whole execution of a CGI is
   protected thus.").

:- true pred html_protect/1 : callable.

:- meta_predicate(html_protect(goal)). % For compatibility

html_protect(Goal) :-
        catch(Goal,E,html_report_error(E)).
html_protect(_) :-
        html_report_error('Application failed.').

% ---------------------------------------------------------------------------
% Support predicates

% Concatenates a list of lists
mappend([], []).
mappend([S|Ss], R) :-
        append(S, R0, R),
        mappend(Ss, R0).

% ---------------------------------------------------------------------------

:- include(library(pillow/ops)). % for '$'/2 operator

:- true pred html_report_error(Error)
        # "Outputs error @var{Error} as a standard HTML page.".

% Error handling
html_report_error(Contents) :-
	error500_template(Contents, HTML),
        output_html([cgi_reply, HTML]),
        flush_output,
        halt.

% % (old, simpler template)
% error500_template(Contents, HTML) :-
% 	HTML = [
%           start,
%           title("Error Report"), 
%           --,
%           h1(['Error:']),
%           --,
%           Contents,
%           --,
%           end
%         ].

% HTML template for internal server error
error500_template(Contents, HTML) :-
	Title = "Error 500 (Internal Server Error)",
	HTML = [
          declare("DOCTYPE html"),
          start,
          head([
              meta$['charset'="utf-8"],
              meta$['name'="viewport",
	            'content'="initial-scale=1, "||
                              "minimum-scale=1, "||
                              "width=device-width"],
	      title(Title)
          ]),
	  env(style, [type='text/css'], [
	      'html,code {',
	      '  font-family: arial, sans-serif;',
	      '  font-size:16px; line-height: 1.3em;',
	      '}',
	      'html {',
	      '  background: white; color: black;',
	      '}',
	      'body {',
	      '  margin: 10% auto 0;',
	      '  max-width:400px;',
	      '  min-height:200px;',
	      '  padding: 40px 0 20px',
	      '}'
          ]),
          begin(body, []),
	  b(Title), \\,
	  %
	  Contents,
          end(body),
	  end
        ].

