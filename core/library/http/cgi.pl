:- module(cgi, [], [assertions,isomodes,dcg,hiord,doccomments,define_flag]).

%! \title  CGI programming
%  \author The Ciao Development Team
%
%  \module
%
%  This module implements the predicates for @concept{CGI}
%  processes. It is based on translating the CGI environment to HTTP
%  request terms (see @lib{http/http_server}).

% (see RFC 3875)
% TODO: move cookies to other place (e.g., http_serve, or a new http_common, etc.)

:- use_module(library(strings), [whitespace0/2]).
:- use_module(engine(io_basic)).
:- use_module(library(stream_utils), [get_line/1, write_string/1]).
:- use_module(library(lists), [append/3]).
:- use_module(library(system), [getenvstr/2]).
:- use_module(library(http/http_grammar), [http_media_type/5, http_crlf/2, http_lines/3, http_type_params/3]).
:- use_module(library(http/http_forms)).
:- use_module(library(http/multipart_form_data)).

:- use_module(engine(runtime_control), [current_prolog_flag/2]). % TODO: do not use flags for raw_form_values

% ---------------------------------------------------------------------------
% HTTP request from CGI environment

:- use_module(library(http/http_messages), [name_to_method/2]).

:- export(cgi_read_request/1).
:- pred cgi_read_request(Request) # "Obtain a HTTP request from the
   CGI environment variables (for headers) and input stream data (for
   contents). This request can be processed with other predicates in
   the @lib{http_server} libraries.".

cgi_read_request(Request) :-
    cgi_read_request_(Request, []).

cgi_read_request_ -->
    { cgi_request_method(Method) },
    [method(Method)],
    ( { cgi_request_uri(URIStr) } ->
        [uri(URIStr)]
    ; []
    ),
    ( { getenvatm('REMOTE_ADDR', RemoteAddr) } ->
        [remote_addr(RemoteAddr)]
    ; []
    ),
    ( { getenvatm('HTTP_USER_AGENT', UserAgent) } ->
        [user_agent(UserAgent)]
    ; []
    ),
    ( { Method = post } -> cgi_read_post ; [] ).

cgi_read_post -->
    { cgi_content_type(Type, Subtype, Params) },
    [content_type(Type, Subtype, Params)],
    %
    { getenvstr('CONTENT_LENGTH', Len0) },
    { number_codes(Len,Len0) },
    [content_length(Len)],
    %
    { read_all(Len, Cs) },
    [content(Cs)],
    !.
cgi_read_post --> [].

cgi_request_method(Method) :-
    getenvatm('REQUEST_METHOD', M),
    name_to_method(M, Method).

cgi_request_uri(URIStr) :-
    getenvstr('SCRIPT_NAME', ScriptName),
    getenvstr('PATH_INFO', PathInfo),
    getenvstr('QUERY_STRING', QueryString),
    ( QueryString = "" -> Q = "" ; Q = "?"||QueryString ),
    mappend([ScriptName, PathInfo, Q], URIStr).

cgi_content_type(Type, Subtype, Params) :-
    getenvstr('CONTENT_TYPE', Str),
    http_media_type(Type, Subtype, Params, Str, []).

getenvatm(Name, Atm) :-
    getenvstr(Name, Str),
    atom_codes(Atm, Str).

% TODO: move to stream_utils.pl
% read N chars from input (N>=0)
read_all(0, []) :- !.
read_all(N, [C|Cs]) :-
    get_code(C),
    N1 is N - 1,
    read_all(N1, Cs).

% ---------------------------------------------------------------------------

% TODO: it should use 'https' when needed
% TODO: obtain from HTTP request headers instead

:- export(my_url/1).
:- doc(my_url(URL), "Unifies @var{URL} with the Uniform
   Resource Locator (WWW address) of this cgi executable.").

:- pred my_url(?string).

my_url(URL) :-
    getenvstr('SERVER_NAME', Server),
    getenvstr('SCRIPT_NAME', File),
    getenvstr('SERVER_PORT', Port),
    ( Port = "80" ->
        mappend(["http://",Server,File], URL)
    ; mappend(["http://",Server,[0':|Port],File], URL)
    ).

% Concatenates a list of lists
mappend([], []).
mappend([S|Ss], R) :-
    append(S, R0, R),
    mappend(Ss, R0).

% ---------------------------------------------------------------------------
% Cookies (see [RFC6265](http://tools.ietf.org/html/6265))
%
% (first version contributed by Samir Genaim)

% TODO: Port to http!

% TODO: Add support for Set-Cookie directives 
%   (see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie).
%   The full list is:
%
%     Expires=<date> -- maximum lifetime of the cookie
%     Max-Age=<non-zero-digit> -- seconds until the cookie expires
%     Domain=<domain-value> -- hosts to which the cookie will be sent (defaults to the host portion of the current document location)
%     Path=<path-value> -- URL path that must exist in the requested resource
%       before sending the Cookie header. The %x2F ("/") character is
%       interpreted as a directory separator and sub directories will be
%       matched as well (e.g. path=/docs, "/docs", "/docs/Web/", or
%       "/docs/Web/HTTP" will all be matched).
%     Secure -- cookie sent to the server when a request is made using SSL and the HTTPS protocol
%     HttpOnly -- prevent access through Document.cookie property (mitigate attacks against cross-site scripting (XSS)).
%     SameSite=Strict -- cookie not to be sent along with cross-site requests (mitigate cross-site request forgery attacks (CSRF)).
%     SameSite=Lax

% sending a cookie is done by printing
%
%  Set-Cookie: var=value
%
% before sending Content-Type

:- export(set_cookie/2).
:- doc(set_cookie(Name,Value), "Sets a cookie of name @var{Name} and
   value @var{Value} (it must be called before @pred{cgi_write_response/1})").

:- pred set_cookie(+atm,+constant).

set_cookie(Name,Value) :-
    name(Value, String),
    form_encode_value(String, EValue, []),
    display('Set-Cookie: '),
    display(Name),
    display('='),
    write_string(EValue),
    nl.

:- doc(get_cookies(Cookies), "Unifies @var{Cookies} with a
   dictionary of @em{attribute}=@em{value} pairs of the active cookies
   for this URL.  If the flag @tt{raw_form_values} is @tt{on},
   @em{value}s are always atoms even if they could be interpreted as
   numbers.").

:- export(get_cookies/1).
:- pred get_cookies(-value_dict).

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
      form_decode_value(StrV,UStrV,[]),
      % TODO: merge with lines_to_value
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

% ---------------------------------------------------------------------------

:- export(cgi_write_response/1).
:- pred cgi_write_response(Response) # "Writes the CGI response
   (similar to a HTTP response but sent through stdout)".

% TODO: This supports only some cases of http_server:html_write_response/2

cgi_write_response(html_string(Cs)) :- !,
    write_string("Content-type: text/html\n\n"),
    write_string(Cs).
cgi_write_response(json_string(Cs)) :- !,
    write_string("Content-type: application/json\n\n"),
    write_string(Cs).
cgi_write_response(_Response) :- !,
    throw(error(unsupported_response, cgi_write_response/1)).

