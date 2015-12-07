:- module(http_server,[http_request/4,http_serve_fetch/2],
	[assertions, isomodes, dcg,hiord]).

%:- use_module(library(dcg/dcg_tr),[phrase/3]).
:- use_module(library(lists), [append/3, select/3]).
:- use_module(library(pillow/pillow_aux), 
	[ http_crlf/2, http_line/3, http_quoted_string/3,
	  http_sp/2, parse_integer/3
	]).
:- use_module(library(strings), [string/3, write_string/2]).
:- use_module(library(pillow/http_server_ll)).

:- doc(bug,"Have to check syntax of HTTP attributes for upper and 
	lowercase.").
:- doc(bug,"Have to check whether numbers are numbers or atoms.").
:- doc(bug,"POST is still under development.").
:- doc(bug,"http_request should not be here!").

http_request(http(Host,Port,Req),Host,Port,Req):-
	atom(Host),
	number(Port),
	Port>0.

:- meta_predicate http_serve_fetch(?,pred(2)).
% Stream: socket stream
% Server: predicate name (without arguments) for processing 
%         requests.
http_serve_fetch(Stream,Server):-
	http_receive_header(Stream,RequestChars,Tail),
	http_parse_request(Doc,Opt,RequestChars,_),
	(member(post,Opt) ->
	 member('Content-length'(Length),Opt),
	 atom_codes(Length,Ls),
	 number_codes(Ln,Ls),
	 http_receive_content(Stream,Ln,Tail,Content),
	 Opt1 = [content(Content)|Opt]
	;
	 Opt1 = Opt
	),
	Server([document(Doc)|Opt1],Response),
	http_response_string(Response,ResponseChars,[]),
	write_string(Stream,ResponseChars),
	flush_output(Stream),
	close(Stream).
	
:- pred http_parse_request(-Document,-Request,+RequestChars,+RequestCharsTail)
   # "Parse a string into an HTTP request, conforming to
      the RFC 1945 guidelines.  Does not use the headers: current date,
      pragma, referer, and entity body (this will have to change if the
      implementation extends beyond the GET and HEAD methods.  cf
      RFC1945 section 7.2)".

http_parse_request(Document,Options) -->
        http_request_method(Options,Options1),
        " ",
        string(Document),
%        " HTTP/1.0",
        " HTTP/", parse_integer(_Major), ".", parse_integer(_Minor),
        http_crlf,
        http_req(Options1), !.

http_request_method(Options,Options1) -->
        "HEAD", !,
        { Options = [head|Options1] }.
http_request_method(Options,Options1) -->
        "POST", !,
        { Options = [post|Options1] }.
http_request_method(Options, Options) -->
        "GET".

http_req([]) -->  http_crlf.
http_req([Option|Options]) -->
        http_request_option(Option), !,
        http_req(Options).

http_request_option(Option) -->
        "User-Agent: ",  !,
        string(AStr),
        http_crlf,
        { Option=user_agent(A),
          atom_codes(A,AStr)
        }.
http_request_option(Option) -->
        "If-Modified-Since: ", !,
        http_date(Date),
        http_crlf,
	{ Option=if_modified_since(Date) }.
http_request_option(Option) -->
        "Authorization: ", !,
        http_credentials(Scheme, Params),
        http_crlf,
	{ Option=authorization(Scheme, Params) }.
http_request_option(Option) -->
        string(FS),
        ": ", !,
        string(AS),
        http_crlf,
        { atom_codes(F,FS),
          functor(Option,F,1),
          arg(1,Option,A),
          atom_codes(A,AS)
        }.
%% Simply fail!
%% http_request_option(O) --> "",
%%         {warning(['Invalid http_request_param ',O])}.

http_credentials(Scheme, Cookie) -->
        "Basic ", !,
        string(Cookie),
	{ Scheme=basic }.
http_credentials(Scheme,Params) -->
        string(S), " ",
        http_credential_params(Params),
        { atom_codes(Scheme, S) }.

http_credential_params([]) --> "".
http_credential_params([P|Ps]) -->
        http_credential_param(P),
        http_credential_params_rest(Ps).

http_credential_params_rest([]) --> "".
http_credential_params_rest([P|Ps]) -->
        ", ",
        http_credential_param(P),
        http_credential_params_rest(Ps).

http_credential_param(Param) -->
        string(PS), "=""", string(V), """",
	!,
        { atom_codes(P, PS),
	  Param=(P=V)
	}.

% ============================================================================
% PROLOG BNF GRAMMAR FOR HTTP RESPONSES
%  Based on RFC 1945
%
% ============================================================================

http_response_string(R) -->
	http_full_response(R), !.
http_response_string(R) -->
	http_simple_response(R).

http_full_response(Response) -->
	% only the first one:
	{ select(status(Ty,SC,RP),Response,Head_Body) }, !,
        http_status_line(Ty,SC,RP),
	{ ( select(content(Body),Head_Body,Head)
	  ; Head=Head_Body, Body=[] )
	},
        http_response_headers(Head),
        http_crlf,
        http_entity_body(Body).

http_simple_response(Body) -->
        http_entity_body(Body).

http_response_headers([H|Hs]) -->
        http_response_header(H),
        http_response_headers(Hs).
http_response_headers([]) --> "".

http_entity_body(B,B,[]).

% ----------------------------------------------------------------------------

http_status_line(Ty,SC,RP) -->
        "HTTP/1.0 ",
        http_status_code(Ty,SC),
        " ",
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
        http_date(D),
        http_crlf.

http_location(location(URL)) -->
        http_field("location"),
        { atom_codes(URL,URLStr) },
        http_line(URLStr).

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
        http_crlf.

http_content_length(content_length(L)) -->
        http_field("content-length"),
	number_string(L),
        http_crlf.

http_content_type(content_type(Type,SubType,Params)) -->
        http_field("content-type"),
        http_media_type(Type,SubType,Params),
        http_crlf.

http_expires(expires(D)) -->
        http_field("expires"),
        http_date(D),
        http_crlf.

http_last_modified(last_modified(D)) -->
        http_field("last-modified"),
        http_date(D),
        http_crlf.

http_extension_header(T) -->
        { functor(T,Fu,1),
	  atom_codes(Fu,F),
	  arg(1,T,A)
        },
        http_field(F),
        http_line(A).

% ----------------------------------------------------------------------------

http_media_type(Type,SubType,Params) -->
        http_lo_up_token(Type),
        "/",
        http_lo_up_token(SubType),
        http_type_params(Params).

http_type_params([P|Ps]) -->
        ";",
        http_type_param(P),
        http_type_params(Ps).
http_type_params([]) --> "".

http_type_param(A = V) -->
        http_lo_up_token(A),
        "=",
        http_token(V).

% ----------------------------------------------------------------------------

% (http://www.w3.org/Protocols/rfc2616)
%
%        HTTP-date    = rfc1123-date | rfc850-date | asctime-date
%        rfc1123-date = wkday "," SP date1 SP time SP "GMT"
%        rfc850-date  = weekday "," SP date2 SP time SP "GMT"
%        asctime-date = wkday SP date3 SP time SP 4DIGIT
%        date1        = 2DIGIT SP month SP 4DIGIT
%                       ; day month year (e.g., 02 Jun 1982)
%        date2        = 2DIGIT "-" month "-" 2DIGIT
%                       ; day-month-year (e.g., 02-Jun-82)
%        date3        = month SP ( 2DIGIT | ( SP 1DIGIT ))
%                       ; month day (e.g., Jun  2)
%        time         = 2DIGIT ":" 2DIGIT ":" 2DIGIT
%                       ; 00:00:00 - 23:59:59
%        wkday        = "Mon" | "Tue" | "Wed"
%                     | "Thu" | "Fri" | "Sat" | "Sun"
%        weekday      = "Monday" | "Tuesday" | "Wednesday"
%                     | "Thursday" | "Friday" | "Saturday" | "Sunday"
%        month        = "Jan" | "Feb" | "Mar" | "Apr"
%                     | "May" | "Jun" | "Jul" | "Aug"
%                     | "Sep" | "Oct" | "Nov" | "Dec"

http_date(Date) --> { nonvar(Date) }, !,
	{ Date = date(WeekDay,Day,Month,Year,Time) },
	% (HTTP 1.1 requires writing only in rfc1123 format)
	% TODO: unfolding def, http_sp is not reversible
        http_wkday(WeekDay), " ",
        http_day(Day, 0'0), " ",
        http_month(Month), " ",
        http_year(Year), " ",
	http_time(Time), " ",
	"GMT".
http_date(Date) --> rfc1123_date(Date), !.
http_date(Date) --> rfc850_date(Date), !.
http_date(Date) --> asctime_date(Date), !.

rfc1123_date(date(WeekDay,Day,Month,Year,Time)) -->
        http_wkday(WeekDay),
        http_sp,
	http_date1(Day, Month, Year),
        http_sp,
	http_time(Time),
        http_sp,
	"GMT".

rfc850_date(date(WeekDay,Day,Month,Year,Time)) -->
        http_weekday(WeekDay),
	",",
        http_sp,
	http_date2(Day, Month, Year),
        http_sp,
	http_time(Time),
        http_sp,
	"GMT".

asctime_date(date(WeekDay,Day,Month,Year,Time)) -->
        http_wkday(WeekDay),
        http_sp,
	http_date3(Day, Month),
        http_sp,
	http_time(Time),
        http_sp,
	http_year(Year).

http_date1(Day, Month, Year) -->
        http_day(Day, 0'0),
        http_sp,
        http_month(Month),
        http_sp,
        http_year(Year).

http_date2(Day, Month, Year) -->
        http_day(Day, 0'0),
        "-",
        http_month(Month),
        "-",
        http_year2(Year).

http_date3(Day, Month) -->
        http_month(Month),
	http_sp,
        http_day(Day, 0' ).

http_wkday('Monday') --> "Mon", !.
http_wkday('Tuesday') --> "Tue", !.
http_wkday('Wednesday') --> "Wed", !.
http_wkday('Thursday') --> "Thu", !.
http_wkday('Friday') --> "Fri", !.
http_wkday('Saturday') --> "Sat", !.
http_wkday('Sunday') --> "Sun", !.

http_weekday('Monday') --> "Monday", !.
http_weekday('Tuesday') --> "Tuesday", !.
http_weekday('Wednesday') --> "Wednesday", !.
http_weekday('Thursday') --> "Thursday", !.
http_weekday('Friday') --> "Friday", !.
http_weekday('Saturday') --> "Saturday", !.
http_weekday('Sunday') --> "Sunday", !.

% Parse/unparse 2DIGIT day. Left zeroes are represented with LeftZ char.
%   E.g., "02", "12", etc. (LeftZ = 0'0)
%   E.g., " 2", "12", etc. (LeftZ = 0' )
http_day(Day, LeftZ) --> { nonvar(Day) }, !,
	{ number_codes(Day, Ds) },
        { Ds = [D1,D2] -> true ; Ds = [D2], D1 = LeftZ },
	[D1,D2].
http_day(Day, LeftZ) --> [D1,D2], !,
	{ D1 = LeftZ -> number_codes(Day, [D2])
	; number_codes(Day, [D1,D2])
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
http_year(Year) --> { nonvar(Year) }, !,
        { number_codes(Year,[Y1,Y2,Y3,Y4]) },
        [Y1,Y2,Y3,Y4].
http_year(Year) -->
        [Y1,Y2,Y3,Y4],
        { number_codes(Year,[Y1,Y2,Y3,Y4]) }.

% 2DIGIT year
% On parsing, converts to 4DIGIT using POSIX conventions:
%   - >=70 -> 19xx
%   - =<69 -> 20xx
% (70->1970, ..., 99->1999, 00->2000, ..., 69->2069)

http_year2(Year) --> { nonvar(Year) }, !,
        { number_codes(Year,[_,_,Y3,Y4]) },
        [Y3,Y4].
http_year2(Year) -->
        [Y3,Y4],
        { number_codes(Year0,[Y3,Y4]) },
	{ Year0 =< 69 -> Year is 2000 + Year0
	; Year is 1900 + Year0
	}.

http_time(Time) --> { nonvar(Time) }, !,
        { atom_codes(Time,Time0),
	  time_field(Time0,[H1,H2,0':|Time1]),
          time_field(Time1,[M1,M2,0':|Time2]),
          time_field(Time2,[S1,S2])
	},
        [H1,H2,0':,M1,M2,0':,S1,S2].
http_time(Time) -->
	% (some clients generate this kind of time format)
	http_2dig_or_1dig(H1,H2), [0':],
	http_2dig_or_1dig(M1,M2), [0':],
	http_2dig_or_1dig(S1,S2),
        { Time0 = [H1,H2,0':,M1,M2,0':,S1,S2],
	  atom_codes(Time,Time0)
	}.

http_2dig_or_1dig(H1,H2) -->
        ( [H1,H2], { digit(H2) } -> []
	; [H2], { H1 = 0'0 }
	).

digit(X) :- X >= 0'0, X =< 0'9.

time_field(Pattern,Pattern):- !.
time_field(Pattern,[0'0|Pattern]).

% ----------------------------------------------------------------------------

http_challenges([C|CS]) -->
        http_challenge(C),
        http_more_challenges(CS).

http_more_challenges([C|CS]) -->
        ", ",
        http_challenge(C),
        http_more_challenges(CS).
http_more_challenges([]) --> http_crlf.

http_challenge(challenge(Scheme,Realm,Params)) -->
        http_lo_up_token(Scheme),
        " ",
        http_lo_up_token(realm), "=", http_quoted_string(Realm),
        http_auth_params(Params).

http_auth_params([P|Ps]) -->
        ",",
        http_auth_param(P),
        http_auth_params(Ps).
http_auth_params([]) --> "".

http_auth_param(P=V) -->
        http_lo_up_token(P),
        "=",
        http_quoted_string(V).

% ----------------------------------------------------------------------------

http_token_list([T|Ts]) -->
        http_token(T),
        http_token_list0(Ts).

http_token_list0([T|Ts]) -->
        ", ",
        http_token(T),
        http_token_list0(Ts).
http_token_list0([]) --> "".

http_token(T) --> token_string(T).

http_lo_up_token(T) --> token_string(T).

token_string(T,String,More):-
	atom_codes(T,Codes),
	append(Codes,More,String).

number_string(L,String,More):-
	number_codes(L,Codes),
	append(Codes,More,String).

% ----------------------------------------------------------------------------

http_field(T,Field,More):- append(T,": "||More,Field).
