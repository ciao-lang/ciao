:- module(http_client, [], [assertions, isomodes, regtypes, dcg, hiord, doccomments]).

%! \title HTTP client
%  \author Daniel Cabeza
%  \author Jose F. Morales (fixes, improvements)
%
%  \module
%
%  This module implements a client the @concept{HTTP} protocol.

:- use_module(library(strings), [string/3]).
:- use_module(library(lists), [member/2, select/3]).
:- use_module(library(http/url), [url_term/1]).

% Grammar definitions
:- use_module(library(http/http_messages)).

:- reexport(library(http/http_messages), [http_request_param/1, http_response_param/1]).

% ---------------------------------------------------------------------------

:- export(fetch_url/3).
:- doc(fetch_url(URL, Request, Response), "Fetches the document
   pointed to by @var{URL} from Internet, using request parameters
   @var{Request} (@tt{get} method by default), and unifies
   @var{Response} with the parameters of the response.  Fails on
   timeout.  Note that redirections are not handled automatically,
   that is, if @var{Response} contains terms of the form
   @tt{status(redirection,301,_)} and @tt{location(NewURL)}, the
   program should in most cases access location @tt{NewURL}.").

:- pred fetch_url(URL, Request, Response)
    : (url_term(URL), list(http_request_param, Request))
       => list(http_response_param, Response).

fetch_url(http(Host, Port, URIStr), Request, Response) :-
    timeout_option(Request, Timeout, Request0),
    http_request_content(Request0, Request1, Content),
    http_request_str(URIStr, Request1, Cs, Cs1),
    string(Content, Cs1, []), % TODO: Do some encoding? Support multipart on client?
    !,
    http_transaction(Host, Port, Cs, Timeout, ResponseChars),
    http_response(Response, ResponseChars, []).

:- pred timeout_option(+Options, -Timeout, -RestOptions)
   # "Returns timeout option, by default 5 min. (300s).".

timeout_option(Options, Timeout, RestOptions) :-
    select(timeout(Timeout), Options, RestOptions), !.
timeout_option(Options, 300, Options).

http_request_content(Options, Options2, Content) :-
    ( member(method(Method), Options) -> Options1=Options
    ; Options1=[method(Method)|Options], Method = get
    ),
    ( Method = post ->
        ( select(content(Content), Options1, Options2) -> true ; fail )
    ; Content = [], Options1 = Options2
    ).

% ---------------------------------------------------------------------------

:- use_module(engine(stream_basic)).
:- use_module(library(sockets)).
:- use_module(library(stream_utils), [write_string/2, read_string_to_end/2]).

:- pred http_transaction(+Host, +Port, +Request, +Timeout, -Response)
   :: atm * int * string * int * string
   # "Sends an HTTP Request to an HTTP server and returns the resultant
      message.  Fails on timeout (@var{Timeout} in seconds).".

http_transaction(Host, Port, Request, Timeout, Response) :-
    connect_to_socket(Host, Port, Stream),
    write_string(Stream, Request),
    flush_output(Stream),
    Timeout_ms is Timeout*1000,
    select_socket(_,_,Timeout_ms,[Stream],R),
    R \== [],  % Fail if timeout
    read_string_to_end(Stream,Response), % TODO: read_bytes_to_end/2?
    close(Stream).
