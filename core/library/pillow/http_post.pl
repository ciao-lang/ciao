:- module(http_post, [
        http_request_param/1, http_response_param/1,
        http_date/1, weekday/1, month/1, hms_time/1,
	%
        fetch_url/3
        ], [assertions,isomodes,dcg]).

% TODO: Differences w.r.t. http.pl?

:- doc(title, "HTTP conectivity").
:- doc(author, "Daniel Cabeza").

:- doc(module, "This module implements the @concept{HTTP} protocol, which
   allows retrieving data from HTTP servers.").

:- use_module(library(strings), [string/3]).
:- use_module(library(lists), [select/3]).
:- use_module(library(pillow/pillow_aux)).
:- use_module(library(pillow/http_ll)).
:- use_module(library(pillow/url), [url_term/1]).

% ---------------------------------------------------------------------------

:- doc(http_request_param/1, "A parameter of an HTTP request:
   @begin{itemize}
   @item @bf{head:} Specify that the document content is not wanted.

   @item @bf{timeout(}@em{T}@bf{):} @em{T} specifies the time in seconds
   to wait for the response.  Default is 300 seconds.

   @item @bf{if_modified_since(}@em{Date}@bf{):} Get document only if
   newer than @em{Date}.  @em{Date} has the format defined by
   @pred{http_date/1}.

   @item @bf{user_agent(}@em{Agent}@bf{):} Provides a user-agent field,
   @em{Agent} is an atom.  The string @tt{\"PiLLoW/1.1\"} (or whatever
   version of PiLLoW is used) is appended.

   @item @bf{authorization(}@em{Scheme},@em{Params}@bf{):} To provide
   credentials.  See RFC 1945 for details.

   @item @bf{@em{option}(}@em{Value}@bf{):} Any unary term, being
   @em{Value} an atom, can be used to provide another valid option (e.g.
   @tt{from('user@@machine')}). @comment{}
   @end{itemize}
").

:- true prop http_request_param(Request) + regtype # "@var{Request} is a
   parameter of an HTTP request.".

http_request_param(_).

:- doc(http_response_param/1, "A parameter of an HTTP response:
   @begin{itemize}

   @item @bf{content(}@em{String}@bf{):} @em{String} is the document
   content (list of bytes).  If the @tt{head} parameter of the HTTP
   request is used, an empty list is get here.

   @item @bf{status(}@em{Type,Code,Reason}@bf{):} @em{Type} is an atom
   denoting the response type, @em{Code} is the status code (an integer),
   and @em{Reason} is a string holding the reason phrase.

   @item @bf{message_date(}@em{Date}@bf{):} @em{Date} is the date of the
   response, with format defined by @pred{http_date/1}.

   @item @bf{location(}@em{Loc}@bf{):} This parameter appears when the
   document has moved, @em{Loc} is an atom holding the new location.

   @item @bf{http_server(}@em{Server}@bf{):} @em{Server} is the server
   responding, as a string.

   @item @bf{authenticate(}@em{Params}@bf{):} Returned if document is
   protected, @em{Params} is a list of chagenges.  See RFC 1945 for details.

   @item @bf{allow(}@em{Methods}@bf{):} @em{Methods} are the methods
   allowed by the server, as a list of atoms.

   @item @bf{content_encoding(}@em{Encoding}@bf{):} @em{Encoding} is an
   atom defining the encoding.

   @item @bf{content_length(}@em{Length}@bf{):} @em{Length} is the
   length of the document (an integer). @comment{}

   @item @bf{content_type(}@em{Type,Subtype,Params}@bf{):} Specifies the
   document content type, @em{Type} and @em{Subtype} are atoms, @em{Params}
   a list of parameters (e.g.  @tt{content_type(text,html,[])}). @comment{}

   @item @bf{expires(}@em{Date}@bf{):} @em{Date} is the date after which
   the entity should be considered stale.  Format defined by
   @pred{http_date/1}.

   @item @bf{last_modified(}@em{Date}@bf{):} @em{Date} is the date at
   which the sender believes the resource was last modified.  Format
   defined by @pred{http_date/1}.

   @item @bf{pragma(}@em{String}@bf{):} Miscellaneous data.

   @item @bf{@em{header}(}@em{String}@bf{):} Any other functor
   @em{header}/1 is an extension header.
   @end{itemize}
").

:- true prop http_response_param(Response) + regtype # "@var{Response}
   is a parameter of an HTTP response.".

http_response_param(_).

:- doc(http_date(Date), "@var{Date} is a term defined as
   @includedef{http_date/1}.").
:- true prop http_date(Date) + regtype # "@var{Date} is a term denoting
   a date.".

http_date(date(WeekDay,Day,Month,Year,Time)) :-
        weekday(WeekDay),
        int(Day),
        month(Month),
        int(Year),
        hms_time(Time).

:- true prop weekday(WeekDay) + regtype # "@var{WeekDay} is a term
   denoting a weekday.".

weekday('Monday').
weekday('Tuesday').
weekday('Wednesday').
weekday('Thursday').
weekday('Friday').
weekday('Saturday').
weekday('Sunday').

:- true prop month(Month) + regtype # "@var{Month} is a term denoting
   a month.".

month('January').
month('February').
month('March').
month('April').
month('May').
month('June').
month('July').
month('August').
month('September').
month('October').
month('November').
month('December').

:- true prop hms_time(Time) + regtype # "@var{Time} is an atom of the form
   @tt{hh:mm:ss}".

hms_time(T) :- atm(T).

% ---------------------------------------------------------------------------

pillow_version("1.1").

:- doc(fetch_url(URL, Request, Response), "Fetches the document
   pointed to by @var{URL} from Internet, using request parameters
   @var{Request}, and unifies @var{Response} with the parameters of the
   response.  Fails on timeout.  Note that redirections are not handled
   automatically, that is, if @var{Response} contains terms of the form
   @tt{status(redirection,301,_)} and @tt{location(NewURL)}, the program
   should in most cases access location @tt{NewURL}.").

:- true pred fetch_url(URL, Request, Response)
        : (url_term(URL), list(Request, http_request_param))
       => list(Response, http_response_param).

fetch_url(http(Host, Port, Document), Request, Response) :-
    timeout_option(Request, Timeout, Request1),
    http_request(Document, Request1, RequestChars, []), !,
    http_transaction(Host, Port, RequestChars, Timeout, ResponseChars),
    http_response(Response, ResponseChars, []).

:- pred timeout_option(+Options, -Timeout, -RestOptions)
   # "Returns timeout option, by default 5 min. (300s).".

timeout_option(Options, Timeout, RestOptions) :-
        select(timeout(Timeout), Options, RestOptions), !.
timeout_option(Options, 300, Options).

:- pred http_request(+Document, +Request, -RequestChars, -RequestCharsTail)
   # "Generate an HTTP request from a list of parameters, conforming to
      the RFC 1945 guidelines.  Does not use the headers: current date,
      pragma, referer, and entity body (this will have to change if the
      implementation extends beyond the GET and HEAD methods.  cf
      RFC1945 section 7.2)".

http_request(Document,Options) -->
        http_request_method(Options,Options1,Content),
        " ",
        string(Document),
        " HTTP/1.0",
        http_crlf,
        http_req(Options1),
	string(Content),
	!.

http_request_method(Options,Options1,[]) -->
        {
            select(head, Options, Options1)
        }, !,
        "HEAD".
http_request_method(Options,Options1,Content) -->
        {
            select(post, Options, Options0), !,
	    select(content(Content),Options0,Options1)
        },
        "POST".
http_request_method(Options, Options,[]) -->
        "GET".

http_req([]) -->  http_crlf.
http_req([Option|Options]) -->
        http_request_option(Option), !,
        http_req(Options).

http_request_option(user_agent(A)) --> !,
        {
            atom_codes(A,AStr),
            pillow_version(Ver)
        },
        "User-Agent: ",
        string(AStr),
        " PiLLoW/",
        string(Ver),
        http_crlf.
http_request_option(if_modified_since(date(WkDay,Day,Month,Year,Time))) --> !,
        "If-Modified-Since: ",
        http_internet_date(WkDay,Day,Month,Year,Time),
        http_crlf.
http_request_option(authorization(Scheme, Params)) --> !,
        "Authorization: ",
        http_credentials(Scheme, Params),
        http_crlf.
http_request_option(O) -->
        {
            functor(O,F,1),
            atom_codes(F,FS),
            arg(1,O,A),
            ( number(A) -> number_codes(A,AS) ; atom_codes(A,AS) )
        }, !,
        string(FS),
        ": ",
        string(AS),
        http_crlf.
http_request_option(O) --> "",
        {warning(['Invalid http_request_param ',O])}.

http_credentials(basic, Cookie) --> !,
        "Basic ",
        string(Cookie).
http_credentials(Scheme,Params) --> !,
        {
            atom_codes(Scheme, S)
        },
        string(S), " ",
        http_credential_params(Params).

http_credential_params([]) --> "".
http_credential_params([P|Ps]) -->
        http_credential_param(P),
        http_credential_params_rest(Ps).
http_credential_params_rest([]) --> "".
http_credential_params_rest([P|Ps]) -->
        ", ",
        http_credential_param(P),
        http_credential_params_rest(Ps).

http_credential_param(P=V) -->
        {
            atom_codes(P,PS)
        },
        string(PS), "=""", string(V), """".

% ============================================================================
% PROLOG BNF GRAMMAR FOR HTTP RESPONSES
%  Based on RFC 1945
%
% ============================================================================


http_response(R) -->
	http_full_response(R), !.
http_response(R) -->
	http_simple_response(R).

http_full_response([Status|Head_Body]) -->
        http_status_line(Status),
        http_response_headers(Head_Body,Body),
        http_crlf,
        http_entity_body(Body).

http_simple_response(Body) -->
        http_entity_body(Body).

http_response_headers([H|Hs], Hs_) -->
        http_response_header(H), !,
        http_response_headers(Hs, Hs_).
http_response_headers(Hs, Hs) --> "".

http_entity_body([content(B)],B,[]).

% ----------------------------------------------------------------------------

http_status_line(status(Ty,SC,RP)) -->
        "HTTP/", parse_integer(_Major), ".", parse_integer(_Minor),
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
type_of_status_code(_, extension_code).

% ----------------------------------------------------------------------------

% General header
http_response_header(P) --> http_pragma(P), !.
http_response_header(D) --> http_message_date(D), !.
% Response header
http_response_header(L) --> http_location(L), !.
http_response_header(S) --> http_server(S), !.
http_response_header(A) --> http_authenticate(A), !.
% Entity header
http_response_header(A) --> http_allow(A), !.
http_response_header(E) --> http_content_encoding(E), !.
http_response_header(L) --> http_content_length(L), !.
http_response_header(T) --> http_content_type(T), !.
http_response_header(X) --> http_expires(X), !.
http_response_header(M) --> http_last_modified(M), !.
http_response_header(E) --> http_extension_header(E), !.

% ----------------------------------------------------------------------------

http_pragma(pragma(P)) -->
        http_field("pragma"),
        http_line(P).

http_message_date(message_date(D)) -->
        http_field("date"),
        parse_http_date(D),
        http_crlf.

http_location(location(URL)) -->
        http_field("location"),
        http_line(URLStr),
        {
            atom_codes(URL,URLStr)
        }.

http_server(http_server(S)) -->
        http_field("server"),
        http_line(S).

http_authenticate(authenticate(C)) -->
        http_field("www-authenticate"),
        http_challenges(C).

http_allow(allow(Methods)) -->
        http_field("allow"),
        http_token_list(Methods),
        http_crlf.

http_content_encoding(content_encoding(E)) -->
        http_field("content-encoding"),
        http_lo_up_token(E),
        http_lws0,
        http_crlf.

http_content_length(content_length(L)) -->
        http_field("content-length"),
        parse_integer(L),
        http_lws0,
        http_crlf.

http_content_type(content_type(Type,SubType,Params)) -->
        http_field("content-type"),
        http_media_type(Type,SubType,Params),
        http_crlf.

http_expires(expires(D)) -->
        http_field("expires"),
        parse_http_date(D),
        http_crlf.

http_last_modified(last_modified(D)) -->
        http_field("last-modified"),
        parse_http_date(D),
        http_crlf.

http_extension_header(T) -->
        http_field(F),
        http_line(A),
        {
            atom_codes(Fu,F),
            functor(T,Fu,1),
            arg(1,T,A)
        }.

% ----------------------------------------------------------------------------

% TODO: Refactor parse_http_date/3 definitions
parse_http_date(date(WeekDay,Day,Month,Year,Time)) -->
        ( http_internet_date(WeekDay,Day,Month,Year,Time), !
	; http_asctime_date(WeekDay,Day,Month,Year,Time)
	).

http_internet_date(WeekDay,Day,Month,Year,Time) -->
        http_weekday(WeekDay),
        ",",
        http_sp,
        http_day(Day),
        http_sp_or_minus,
        http_month(Month),
        http_sp_or_minus,
        http_year(Year),
        http_sp,
        http_time(Time),
        http_sp,
        "GMT".

http_sp_or_minus --> "-", !.
http_sp_or_minus --> http_sp.

http_asctime_date(WeekDay,Day,Month,Year,Time) -->
        http_weekday(WeekDay),
        http_sp,
        http_month(Month),
        http_sp,
        http_day(Day),
        http_sp,
        http_time(Time),
        http_sp,
        http_year(Year).

http_weekday('Monday') --> "Monday", !.
http_weekday('Tuesday') --> "Tuesday", !.
http_weekday('Wednesday') --> "Wednesday", !.
http_weekday('Thursday') --> "Thursday", !.
http_weekday('Friday') --> "Friday", !.
http_weekday('Saturday') --> "Saturday", !.
http_weekday('Sunday') --> "Sunday", !.
http_weekday('Monday') --> "Mon", !.
http_weekday('Tuesday') --> "Tue", !.
http_weekday('Wednesday') --> "Wed", !.
http_weekday('Thursday') --> "Thu", !.
http_weekday('Friday') --> "Fri", !.
http_weekday('Saturday') --> "Sat", !.
http_weekday('Sunday') --> "Sun", !.

http_day(Day) -->
        [D1,D2],
        {
            number_codes(Day,[D1,D2])
        }, !.
http_day(Day) -->
        http_sp,
        [D],
        { 
            number_codes(Day,[D])
        }.

http_month('January') --> "Jan".
http_month('February') --> "Feb".
http_month('March') --> "Mar".
http_month('April') --> "Apr".
http_month('May') --> "May".
http_month('June') --> "Jun".
http_month('July') --> "Jul".
http_month('August') --> "Aug".
http_month('September') --> "Sep".
http_month('October') --> "Oct".
http_month('November') --> "Nov".
http_month('December') --> "Dec".

% Assumes Year > 999
http_year(Year) -->
        [Y1,Y2,Y3,Y4],
        {
            number_codes(Year,[Y1,Y2,Y3,Y4])
        }, !.
http_year(Year) -->
        [Y1,Y2],
        {
            number_codes(Y,[Y1,Y2]),
            ( Y >= 90 -> Year is 1900+Y ; Year is 2000+Y )
        }.

http_time(Time) -->
        [H1,H2,0':,M1,M2,0':,S1,S2],
        {
            atom_codes(Time,[H1,H2,0':,M1,M2,0':,S1,S2])
        }.


% ----------------------------------------------------------------------------

http_challenges([C|CS]) -->
        http_maybe_commas,
        http_challenge(C),
        http_more_challenges(CS).

http_more_challenges([C|CS]) -->
        http_commas,
        http_challenge(C),
        http_more_challenges(CS).
http_more_challenges([]) --> http_lws0, http_crlf.

http_challenge(challenge(Scheme,Realm,Params)) -->
        http_lo_up_token(Scheme),
        http_sp,
        http_lo_up_token(realm), "=", http_quoted_string(Realm),
        http_lws0,
        http_auth_params(Params).

http_auth_params([P|Ps]) -->
        ",", http_lws0,
        http_auth_param(P), http_lws0,
        http_auth_params(Ps).
http_auth_params([]) --> "".

http_auth_param(P=V) -->
        http_lo_up_token(P),
        "=",
        http_quoted_string(V).

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

http_maybe_commas -->
        ",", !, http_lws0,
        http_maybe_commas.
http_maybe_commas --> "".



% ----------------------------------------------------------------------------

http_field([C|Cs]) -->
        http_lo_up_token_char(C),
        http_lo_up_token_rest(Cs),
        ":", http_lws.
