:- module(http_messages, [], [assertions, regtypes, isomodes, dcg, hiord, doccomments]).

%! \title  HTTP messages (response and request)
%  \author The Ciao Development Team
%
%  \module
%
%  Parser/printer for HTTP response and request messages.
%
%  See [RFC2616](https://www.w3.org/Protocols/rfc2616/rfc2616.html) 
%  for a reference of HTTP/1.1 protocol.
%
%  The request and response objects are represented as list of terms,
%  which represent HTTP header fields and other elements (such as the
%  status, and contents).
%
%  The current list of **request** headers is:
%
%  @begin{itemize}
%  @item @bf{method(}@em{Method}@bf{):} @em{Method} is the HTTP method
%  in lowercase (@tt{head}, @tt{get}, @tt{post}, etc.).
%
%  @item @bf{timeout(}@em{T}@bf{):} @em{T} specifies the time in seconds
%  to wait for the response.  Default is 300 seconds.
%
%  @item @bf{if_modified_since(}@em{Date}@bf{):} Get document only if
%  newer than @em{Date}.  @em{Date} has the format defined by
%  @pred{http_date/1}.
%
%  @item @bf{user_agent(}@em{Agent}@bf{):} Provides a user-agent field,
%  @em{Agent} is an atom.
%
%  @item @bf{authorization(}@em{Scheme},@em{Params}@bf{):} To provide
%  credentials.  See RFC 1945 for details.
%
%  @item @bf{@em{option}(}@em{Value}@bf{):} Any unary term, being
%  @em{Value} an atom, can be used to provide another valid option (e.g.
%  @tt{from('user@@machine')}). @comment{}
%
%  @end{itemize}
%
%  The current list of **response** headers is:
%  @begin{itemize}
%
%  @item @bf{status(}@em{Type,Code,Reason}@bf{):} @em{Type} is an atom
%  denoting the response type, @em{Code} is the status code (an integer),
%  and @em{Reason} is a string holding the reason phrase.
%
%  @item @bf{date(}@em{Date}@bf{):} @em{Date} is the date of the
%  response, with format defined by @pred{http_date/1}.
%
%  @item @bf{location(}@em{Loc}@bf{):} This parameter appears when the
%  document has moved, @em{Loc} is an atom holding the new location.
%
%  @item @bf{server(}@em{Server}@bf{):} @em{Server} is the server
%  responding, as an atom.
%
%  @item @bf{www_authenticate(}@em{Params}@bf{):} Returned if document is
%  protected, @em{Params} is a list of chagenges.  See RFC 1945 for details.
%
%  @item @bf{allow(}@em{Methods}@bf{):} @em{Methods} are the methods
%  allowed by the server, as a list of atoms.
%
%  @item @bf{content_encoding(}@em{Encoding}@bf{):} @em{Encoding} is an
%  atom defining the encoding.
%
%  @item @bf{expires(}@em{Date}@bf{):} @em{Date} is the date after which
%  the entity should be considered stale.  Format defined by
%  @pred{http_date/1}.
%
%  @item @bf{last_modified(}@em{Date}@bf{):} @em{Date} is the date at
%  which the sender believes the resource was last modified.  Format
%  defined by @pred{http_date/1}.
%
%  @item @bf{pragma(}@em{String}@bf{):} Miscellaneous data.
%
%  @item @bf{@em{header}(}@em{String}@bf{):} Any other functor
%  @em{header}/1 is an extension header.
%  @end{itemize}
%
%  The current list of some common headers is:
%  @begin{itemize}
%
%  @item @bf{content_length(}@em{Length}@bf{):} @em{Length} is the
%  length of the document (an integer). @comment{}
%
%  @item @bf{content_type(}@em{Type,Subtype,Params}@bf{):} Specifies the
%  document content type, @em{Type} and @em{Subtype} are atoms, @em{Params}
%  a list of parameters (e.g.  @tt{content_type(text,html,[])}). @comment{}
%
%  @item @bf{content(}@em{Bytes}@bf{):} @em{Bytes} is the document
%  content (list of bytes).  If @tt{method(head)} of the HTTP
%  request is used, an empty list is get here.
%
%  @end{itemize}
%

:- use_module(library(lists), [select/3]).
:- use_module(library(strings), [string/3]).
:- use_module(library(http/http_grammar)).
:- use_module(library(http/http_date)).

:- doc(bug, "Add support for more request headers: current date,
   pragma, referer, etc.?").

% ---------------------------------------------------------------------------

:- export(http_request_param/1).
:- regtype http_request_param(Request) # "@var{Request} is a
   parameter of an HTTP request.".

% TODO: fill or leave open?
http_request_param(_).

% ---------------------------------------------------------------------------

:- export(http_request_str/4).
:- pred http_request_str(-RequestURI, -Request, +RequestBytes, +RequestBytesTail)
   # "Parse a string into an HTTP request".
:- pred http_request_str(+RequestURI, +Request, -RequestBytes, -RequestBytesTail)
   # "Generate an HTTP request from a list of parameters".

http_request_str(RequestURI, Options) -->
    http_request_line(RequestURI, Options, Options1),
    % TODO: specify request vs response?
    ( 'PRINTING' ->
        print_headers(Options1)
    ; parse_headers(Options1, [])
    ).

http_request_line(RequestURI, Options, Options1) -->
    http_request_method(Options,Options1),
    " ",
    string(RequestURI), % TODO: UTF8 encode?
    " ",
    !,
    ( 'PRINTING' -> http_http(1,0) % TODO: change version?
    ; http_http(_Major,_Minor) % TODO: do something with version?
    ),
    http_crlf.

http_request_method(Options, Options1) --> 'PRINTING', !,
    { select(method(Method), Options, Options1) -> true ; fail },
    { method_to_name(Method, Name) },
    http_token(Name).
http_request_method(Options,Options1) -->
    http_token(Name),
    { name_to_method(Name, Method) },
    { Options = [method(Method)|Options1] }.

method_to_name(head, 'HEAD').
method_to_name(post, 'POST').
method_to_name(get, 'GET').

:- export(name_to_method/2).
name_to_method('HEAD', head).
name_to_method('POST', post).
name_to_method('GET', get).

% ---------------------------------------------------------------------------

:- export(http_response_param/1).
:- regtype http_response_param(Response) # "@var{Response}
   is a parameter of an HTTP response.".

% TODO: fill or leave open?
http_response_param(_).

% ---------------------------------------------------------------------------

:- export(http_response_str/3).
http_response_str(Response) -->
    http_full_response(Response), !.
% TODO: if full_response fails, it try the simple_response; is it OK?
http_response_str(Response) -->
    http_simple_response(Response).

http_full_response(Response) --> 'PRINTING', !,
    % only the first one:
    { Status = status(_,_,_), select(Status,Response,Response2) }, !,
    http_status_line(Status),
    { ( select(content(Body),Response2,Response3) -> true
      ; Response3=Response2,
        Body=[]
      )
    },
    print_headers(Response3),
    http_entity_body(Body).
http_full_response(Response) -->
    { Response = [Status|Response1] },
    http_status_line(Status),
    parse_headers(Response1,Response2),
    { Response2 = [content(Body)] },
    http_entity_body(Body).

http_simple_response([content(Body)]) -->
    http_entity_body(Body).

http_entity_body(B,B,[]).

% ===========================================================================
% Header fields
% (see headers definition below)

% TODO: specify request vs response?

parse_headers(Options, Options) --> http_crlf, !.
parse_headers([Option|Options], Options0) -->
    http_header(Option), !,
    parse_headers(Options, Options0).

print_headers([]) --> http_crlf.
print_headers([Option|Options]) -->
    http_header(Option), !,
    print_headers(Options).

http_header(Option) --> 'PRINTING', !,
    ( { header_field(Option, Field) } ->
        % Use field name for output
        { header_shown(Field, FieldShown) },
        http_token(FieldShown), ":", http_lws,
        % Print values
        header_value(Field, Option)
    ; print_unknown_header(Option)
    ).
http_header(Option) -->
    % Parse normalized field name
    http_lo_up_token(Field), ":", http_lws,
    ( { header_shown(Field, _) } ->
        % Parse values
        header_value(Field, Option)
    ; parse_unknown_header(Field, Option)
    ).

print_unknown_header(O) -->
    { functor(O,F,1), arg(1,O,A) },
    http_field(F),
    http_line(A).

parse_unknown_header(F, Option) -->
    http_line(A),
    { functor(Option,F,1), arg(1,Option,A) }.

% ---------------------------------------------------------------------------
% Hooks for headers definition

% For each header field, we must define:
%  - (indexed by the normalized name for parsing)
%    - its term representation (Opt)
%    - the rule to parse/print their values (header_value/4)
%    - the field name for printing (header_shown/2)
%  - (indexed by the term representation):
%    - the normalized name for parsing (header_field/2)

% TODO: create indexing rules below automatically?

% header_field(Opt, 'my-opt')
% header_shown('my-opt', 'My-Opt')
% header_value('my-opt', Opt, Cs, Cs0)
:- discontiguous(header_field/2).
:- discontiguous(header_shown/2).
:- discontiguous(header_value/4).

% ---------------------------------------------------------------------------
% Headers (see https://www.w3.org/Protocols/rfc2616/rfc2616.html for a
% description of each one)

header_field(pragma(_), 'pragma').
header_shown('pragma', 'Pragma').
header_value('pragma', pragma(P)) --> !,
    http_line(P).

header_field(date(_), 'date').
header_shown('date', 'Date').
header_value('date', date(D)) --> !,
    http_date_str(D),
    http_crlf.

header_field(location(_), 'location').
header_shown('location', 'Location').
header_value('location', location(URL)) --> !,
    http_line_atm(URL).

header_field(server(_), 'server').
header_shown('server', 'Server').
header_value('server', server(S)) --> !,
    http_line_atm(S).

header_field(www_authenticate(_), 'www-authenticate').
header_shown('www-authenticate', 'WWW-Authenticate').
header_value('www-authenticate', www_authenticate(C)) --> !,
    http_challenges(C),
    http_lws0,
    http_crlf.

header_field(allow(_), 'allow').
header_shown('allow', 'Allow').
header_value('allow', allow(Methods)) --> !,
    http_token_list(Methods),
    http_crlf.

header_field(content_encoding(_), 'content-encoding').
header_shown('content-encoding', 'Content-Encoding').
header_value('content-encoding', content_encoding(E)) --> !,
    http_lo_up_token(E),
    http_lws0,
    http_crlf.

header_field(content_length(_), 'content-length').
header_shown('content-length', 'Content-Length').
header_value('content-length', content_length(L)) --> !,
    integer_str(L),
    http_lws0,
    http_crlf.

header_field(content_type(_,_,_), 'content-type').
header_shown('content-type', 'Content-Type').
header_value('content-type', content_type(Type,Subtype,Params)) --> !,
    http_media_type(Type,Subtype,Params),
    http_crlf.

header_field(expires(_), 'expires').
header_shown('expires', 'Expires').
header_value('expires', expires(D)) --> !,
    http_date_str(D),
    http_crlf.

header_field(last_modified(_), 'last-modified').
header_shown('last-modified', 'Last-Modified').
header_value('last-modified', last_modified(D)) --> !,
    http_date_str(D),
    http_crlf.

% --

header_field(user_agent(_), 'user-agent').
header_shown('user-agent', 'User-Agent').
header_value('user-agent', user_agent(A)) --> !,
    http_line_atm(A).

header_field(if_modified_since(_), 'if-modified-since').
header_shown('if-modified-since', 'If-Modified-Since').
header_value('if-modified-since', if_modified_since(Date)) --> !,
    http_date_str(Date),
    http_crlf.

header_field(authorization(_,_), 'authorization').
header_shown('authorization', 'Authorization').
header_value('authorization', authorization(Scheme, Params)) --> !,
    http_credentials(Scheme, Params),
    http_crlf.

% ---------------------------------------------------------------------------

% TODO: "Basic" auth-scheme is not supported (need token68 - see https://tools.ietf.org/html/rfc7235#page-12) 
http_credentials(Scheme,Params) -->
    http_lo_up_token(Scheme),
    http_sp,
    http_auth_params(Params).

% ----------------------------------------------------------------------------

http_challenges([C|CS]) -->
    http_maybe_commas,
    http_challenge(C),
    http_more_challenges(CS).

http_more_challenges([C|CS]) -->
    http_commas,
    http_challenge(C),
    http_more_challenges(CS).
http_more_challenges([]) --> "".

http_challenge(challenge(Scheme,Realm,Params)) -->
    http_lo_up_token(Scheme),
    http_sp,
    http_auth_params([realm=Realm|Params]).

% ----------------------------------------------------------------------------

http_token_list([T|Ts]) -->
    http_maybe_commas,
    http_token(T),
    http_token_list0(Ts).

http_token_list0([T|Ts]) -->
    http_commas,
    http_token(T),
    http_token_list0(Ts).
http_token_list0([]) -->
    http_maybe_commas.

http_commas -->
    http_lws0,",",http_lws0,
    http_maybe_commas.

http_maybe_commas --> 'PRINTING', !.
http_maybe_commas -->
    ",", !, http_lws0,
    http_maybe_commas.
http_maybe_commas --> "".

