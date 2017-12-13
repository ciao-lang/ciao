:- module(http_server,[], [assertions, isomodes, dcg, hiord, doccomments]).

%! \title HTTP server
%  \author Ciao Development Team
%  \author Jose F. Morales (multifile-based HTTP simple server)
%
%  \module
%
%  This module implements a simple HTTP server.
% 
%  It can be used to handle individual HTTP requests
%  (@pred{http_serve_fetch/2}) or for implementing a simple HTTP
%  server (see @pred{http_bind/1}, @pred{http_loop/1},
%  @pred{http_shutdown/1}).
%

:- use_module(library(lists), [append/3, select/3, length/2]).
:- use_module(library(strings), [string/3, write_string/2]).
:- use_module(library(pathnames), [path_concat/3]).

% Grammar definitions
:- use_module(library(http/http_messages)).

% ===========================================================================
:- doc(section, "Read HTTP requests").

:- use_module(library(sockets), [socket_recv_code/3, socket_getpeername/2]).

% Note: Request = [] if the stream has been orderly closed on the peer
http_read_request(Stream, Request) :-
	http_read_header(Stream, RequestChars, Tail), % (Tail are chars that should by in the content)
	% format("~s~n", RequestChars), nl,
	( RequestChars = [] ->
	    % Stream was orderly closed (socket_recv_code/3 returned empty data)
	    Request = []
	; http_request_str(URIStr,Request1,RequestChars,_) ->
	    ( member(method(post),Request1) ->
	        http_read_content(Stream, Request1, Request2, Tail)
	    ; Request2 = Request1
	    ),
	    socket_getpeername(Stream, RemoteAddr),
	    Request = [uri(URIStr), remote_addr(RemoteAddr)|Request2]
	; log(error, failed(http_request_str/4)),
	  fail % TODO: exception?
	).

% Receive header and content
http_read_header(Stream,Str,Tail):-
	http_read_header_(Stream,Data,[],Tail),
	flatten(Data,Str),
	!.
http_read_header(_,_,_):-
	log(error, failed(http_read_header/3)).

http_read_header_(S,[Chunk|Cont],LastThree,Tail):-
	socket_recv_code(S,Chunk,ChunkLen),
	( ChunkLen = 0 -> % Peer closed, no more data (for TCP)
	    Cont = []
	; append(LastThree,Chunk,Ch1),
	  miniparse(Ch1,Tail,StopReading,LastThree2),
	  ( StopReading = yes ->
	      Cont = []
	  ; http_read_header_(S,Cont,LastThree2,Tail)
	  )
	).

%% traverses the chunk just read looking for cr lf cr lf.
miniparse([A],_,no,[A]):- !.
miniparse([10,10|Tail],Tail,yes,[10,10]):- !.
miniparse([A,B],_,no,[A,B]):- !.
miniparse([A,B,C],_,no,[A,B,C]):- !.
miniparse([13,10,13,10|Tail],Tail,yes,[10,13,10]):- !.
miniparse([_|Xs],Tail,Stop,LastThree):-
	miniparse(Xs,Tail,Stop,LastThree).
	
% depth 2 list flattening.
flatten([],[]).
flatten([[]|Xs],Ys):- !,
	flatten(Xs,Ys).
flatten([[X|Xs]|Ys],Zs):-
	flatten(Ys,Z1s),
	append([X|Xs],Z1s,Zs).

% TODO: Add a command that replaces content/1 by its multipart split; other that extracts the query as a dict?

% Receive content (only for POST requests)
% (Note: Tail are pending read characters from parsing the header)
http_read_content(Stream, Request, Request1, Tail) :-
	member(content_length(Length),Request),
	http_read_content_n(Stream,Length,Tail,Content),
	Request1 = [content(Content)|Request],
	!.
http_read_content(_Stream, Request, _, _) :-
	log(error, failed(http_read_content/4, Request)).

http_read_content_n(Stream,Length,AlreadyRead,Data):-
	length(AlreadyRead,N),
	Length1 is Length - N,
	http_read_content_(Stream,Length1,Data1),
	append(AlreadyRead,Data1,Data2),
	trim(Data2,Length,Data).

http_read_content_(_,N,[]):-
	N =< 0, !.
http_read_content_(Stream,Length,Data):-
	socket_recv_code(Stream,Chunk,ChunkLen),
	( ChunkLen = 0 -> % Peer closed, no more data (for TCP)
	    Data = []
	; length(Chunk,N), % TODO: trust ChunkLen?
	  Length1 is Length - N,
	  append(Chunk,Data1,Data),
	  http_read_content_(Stream,Length1,Data1)
	).
	
trim(_,0,[]) :- !.
trim([X|Xs],N,[X|Ys]):-
	N1 is N-1,
	trim(Xs,N1,Ys).

% ===========================================================================
:- doc(section, "Write HTTP responses").

:- doc(bug, "Serve file without reading it as a string"). % TODO: write header and then contents

http_write_response(Stream, Response) :-
	expand_response(Response, Response2),
	http_send_response(Stream, Response2).

http_send_response(Stream, Response) :-
	http_response(Response,ResponseChars,[]),
	write_string(Stream,ResponseChars),
%	write_string(user_output,"{{{RESPONSE IS:\n"),
%	write_string(user_output,ResponseChars),
%	write_string(user_output,"}}}\n"),
	flush_output(Stream),
	close(Stream). % TODO: could be keep Stream open? (see socket_select leak problem)
	
% Expand some common HTTP responses:
%   - not_found(P): P is not found
%   - file(P): serve file P
%   - file_if_newer(OldModifDate, P): serve file P if it has been modified since OldModifDate
%   - file_(S,C,OldModifDate,P): serve file P with specific status S and content type C
%       (if modified since OldModifDate) -- otherwise send 304 status
%   - string_(S,C,X): serve string X with specific status S and content type C
%   - html_string(Str): serve string as HTML (200 status)
%   - html_string(S,Str): serve string as HTML with status S
%   - json_string(Str): serve string as JSON (200 status)

expand_response(Response0, Response) :- Response0 = [_|_], !, % Low-level response
	Response = Response0.
%
expand_response(not_found(_Path), Response) :-
	Status = status(request_error,404,"Not Found"),
	not_found_html(String),
	expand_response(html_string(Status, String), Response).
%
expand_response(file(Path), Response) :-
	Status = status(success,200,"OK"),
	file_content_type(Path, ContentType),
	expand_response(file_(Status, ContentType, none, Path), Response).
expand_response(file_if_newer(OldModifDate, Path), Response) :-
	Status = status(success,200,"OK"),
	file_content_type(Path, ContentType),
	expand_response(file_(Status, ContentType, OldModifDate, Path), Response).
expand_response(html_string(Content), Response) :- !,
	Status = status(success,200,"OK"),
	expand_response(html_string(Status, Content), Response).
expand_response(html_string(Status, Content), Response) :- !,
	ContentType = content_type(text,html,[charset='UTF-8']),
	expand_response(string_(Status, ContentType, Content), Response).
expand_response(json_string(Content), Response) :- !,
	Status = status(success,200,"OK"),
%	ContentType = content_type(application,json,[charset='UTF-8']),
	ContentType = content_type(application,json,[]), % TODO: no charset OK?
	expand_response(string_(Status, ContentType, Content), Response).
% Contents of file at Path
expand_response(file_(Status, ContentType, OldModifDate, Path), Response) :-
	% TODO: add special 'contents_from_file' response, serve without going through terms
	modif_time(Path, ModifTime),
	date_time(ModifTime, ModifDate),
	( needs_update(OldModifDate, ModifDate) ->
	    file_to_string(Path,Content),
	    expand_response_(Status, ModifDate, ContentType, Content, Response, [])
	; not_modified_response(ModifDate, Response, [])
	).
% String
expand_response(string_(Status, ContentType, Content), Response) :-
	ModifDate = none,
	expand_response_(Status, ModifDate, ContentType, Content, Response, []).

expand_response_(Status, ModifDate, ContentType, Content) -->
	% Contents from a string
	{ length(Content, ContentLength) },
	%
	{ server_name(ServerName) },
	{ current_host(Host) },
	{ date_time(_, CurrDate) },
	[Status],
	[date(CurrDate)],
	[server(ServerName)],
	( { ModifDate = none } -> [] ; [last_modified(ModifDate)] ),
%       [etag("""2d41cf-4a7-69344c00""")],
%       [accept-ranges("bytes")],
	[location(Host)],
	[content_length(ContentLength)],
	( { ContentType = none } -> [] ; [ContentType] ),
	[content(Content)].

not_modified_response(ModifDate) -->
	{ Status = status(redirection,304,"OK") },
	{ date_time(_, CurrDate) },
	[Status],
	[date(CurrDate)],
	( { ModifDate = none } -> [] ; [last_modified(ModifDate)] ).

needs_update(none, _ModifDate) :- !.
needs_update(OldModifDate, ModifDate) :-
	% It is OK to use exact date comparison here (the client
	% should not have a cached version more recent than that 
	% we have!)
	\+ OldModifDate = ModifDate.

% ===========================================================================
:- doc(section, "Receive and parse one request").

:- export(http_serve_fetch/2).
:- meta_predicate http_serve_fetch(?,pred(2)).
:- pred http_serve_fetch(Stream, Server) # "Read a HTTP
   request from @var{Stream}, obtain the response calling @var{Server}
   predicate, and write the response the socket stream.".

http_serve_fetch(Stream,Server):-
	( http_read_request(Stream, Request) ->
	    ( Request = [] ->
	        % TODO: peer orderly closed; do something? (e.g., in multi-worker setting)
	        true
	    ; Server(Request, Response) ->
	        http_write_response(Stream, Response)
	    ; % Request (without Content) 
	      log(error, failed(http_serve_fetch/2, Request))
	    )
	; log(error, failed(http_read_request/2))
	).

% ===========================================================================
:- doc(section, "Simple server").
	
:- use_module(library(sockets)).
:- use_module(library(sockets/sockets_io), [serve_socket/3]).
:- use_module(library(file_utils), [file_to_string/2]).
:- use_module(library(system), [file_exists/1, current_host/1, modif_time/2]).
:- use_module(library(lists), [length/2, list_concat/2]).
:- use_module(library(pathnames), [path_splitext/3]).
:- use_module(library(terms), [atom_concat/2]).

:- include(library(http/http_server_hooks)).

:- data curr_socket/1.

server_name('Ciao HTTP Server 0.1').

not_found_html(
    "<html>"||
    "<body>"||
    "<pre>File not found</pre>"||
    "</body>"||
    "</html>").

:- export(http_bind/1).
:- pred http_bind(Port) # "Bind socket to the port @var{Port} (use
   with @pred{http_loop/1})".

http_bind(Port) :-
	retractall_fact(shutdown(_)),
	retractall_fact(curr_socket(_)),
	bind_socket(Port, 5, Socket),
	assertz_fact(curr_socket(Socket)).

:- export(http_loop/1).
:- pred http_loop(ExitCode)
   # "Listen and handle HTTP requests in a loop. You can terminate
      this loop by @pred{http_shutdown/1} predicate. Requests are
      handled by multifile @pred{http_handle/3} and
      @pred{http_file_path/2} predicates (declared in
      @lib{http_server_hooks} file).".

http_loop(ExitCode) :-
	curr_socket(Socket),
	catch(serve_socket_loop(Socket),
	      err_shutdown(Code),
	      ExitCode=Code).

% TODO: I need to handle 'connection reset by peer'
% TODO: Why is loop needed? Maybe due to interrupts
serve_socket_loop(Socket) :-
	( serve_socket(Socket, socket_serve, catcher) ->
	    true
	; log(note, serve_socket_loop), % try again!
	  serve_socket_loop(Socket)
	).

:- data shutdown/1.

:- export(http_shutdown/1).
:- pred http_shutdown(ExitCode) # "@var{ExitCode} mark that we are not
   going to process further requests".

http_shutdown(ExitCode) :-
	assertz_fact(shutdown(ExitCode)).

socket_serve(Stream) :-
	% TODO: Are Stream objects here leaking? (see C code)
	http_serve_fetch(Stream,http_serve(Stream)),
	( current_fact(shutdown(Code)) ->
	    throw(err_shutdown(Code)) % TODO: better way?
	; true
	).

http_serve(Request, _Stream, Response) :-
	log(note, received_message(Request)),
	( handle(Request,Response0) ->
	    Response = Response0
	; log(error, failed(http_serve/3, Request))
	).

handle(Request, Response) :-
	( member(uri(URIStr),Request) -> true
	; member(method(post),Request) -> URIStr = "/" % TODO: Why? not really correct
	),
	handle_(URIStr, Request, Response).

handle_(Path, Request, Response) :-
	http_handle(Path, Request, Response0),
	!,
	Response = Response0.
handle_(File0, Request, Response) :-
	atom_codes(File, File0),
	( locate_file(File, LocalFile) ->
	    ( member(if_modified_since(OldModifDate), Request) ->
	        Response = file_if_newer(OldModifDate, LocalFile)
	    ; Response = file(LocalFile)
	    )
	; % log(error, not_found(File)),
	  Response = not_found(File)
	),
	% TODO: add a 'not_logged' or 'logged' part to responses; we only log requests not captured by http_handle/3
	% TODO: log Response after expand_response
	log_response(Request, Response). % TODO: make it optional? 

catcher(E) :- E = err_shutdown(_), !, throw(E).
catcher(Error) :- 
	log(error, Error).

locate_file(File, LocalFile) :-
	http_file_path(Dir, LocalPath),
	% TODO: use pathnames? (safer)
	atom_concat(Dir, Rel, File), % File is under Dir
	atom_concat(LocalPath, Rel, LocalFile0),
	( is_dir(LocalFile0) -> path_concat(LocalFile0, 'index.html', LocalFile)
	; LocalFile = LocalFile0
	),
	file_exists(LocalFile).

:- use_module(library(system), [file_properties/6]).

% TODO: duplicated
is_dir(Path) :-
        prolog_flag(fileerrors, OldFE, off),
        file_properties(Path, directory, [], [], [], []),
        set_prolog_flag(fileerrors, OldFE).

% ---------------------------------------------------------------------------

:- use_module(library(http/mimetypes)).

% Detect content type of a file (based on extension).
%
% Do not send ContentType if it is not known (RFC-7231)
file_content_type(Path, ContentType) :-
	path_splitext(Path, _, Ext),
	mimetype(Ext, Type, SubType),
	!,
	( Type = text -> Params = [charset='UTF-8'] ; Params = [] ),
	ContentType = content_type(Type, SubType, Params).
file_content_type(_Path, none).

% ---------------------------------------------------------------------------
% Timestamp to http_date conversion

% TODO: Move somewhere else? use http_date.pl?

:- use_module(library(system), [datime/9]).

% date_time(Timestamp, Date): @var{Date} is the http_date
%   corresponding to the given timestamp. If timestamp is free, get
%   the current time (see @pred{datime/9}).
date_time(Timestamp, Date) :-
	datime(Timestamp,Year,MonthNum,Day,HH,MM,SS,WeekDayNum,_),
	http_weekday_atm(WeekDayNum,WeekDay),
	http_month_atm(MonthNum,Month),
	num_codes2(HH,H),
	num_codes2(MM,M),
	num_codes2(SS,S),
	list_concat([H,":",M,":",S],TimeStr),
	atom_codes(Time,TimeStr),
	Date = date(WeekDay,Day,Month,Year,Time).

http_weekday_atm(0,'Sunday').
http_weekday_atm(1,'Monday').
http_weekday_atm(2,'Tuesday').
http_weekday_atm(3,'Wednesday').
http_weekday_atm(4,'Thursday').
http_weekday_atm(5,'Friday').
http_weekday_atm(6,'Saturday').

http_month_atm( 1,'January').
http_month_atm( 2,'February').
http_month_atm( 3,'March').
http_month_atm( 4,'April').
http_month_atm( 5,'May').
http_month_atm( 6,'June').
http_month_atm( 7,'July').
http_month_atm( 8,'August').
http_month_atm( 9,'September').
http_month_atm(10,'October').
http_month_atm(11,'November').
http_month_atm(12,'December').

% (force two digits)
num_codes2(N, Cs) :-
	number_codes(N, Xs),
	( Xs = [_] -> Cs = "0"||Xs
	; Xs = Cs
	).

% ---------------------------------------------------------------------------
% Replace response on error (exception)

:- export(http_protect/4).
:- pred http_protect(Handler, ErrHandler, Request, Response)
   # "Execute @var{Handler} on @var{Request} to obtain @var{Response}.
      If @var{Handler} fails or raises some exception @var{E}, a
      response is obtained calling @var{ErrHandler} on @var{E}.
      This is useful, for example, to produce an HTML page
      informing about the incident".

:- meta_predicate http_protect(pred(2), pred(2), ?, ?).

http_protect(Handler, ErrHandler, Request, Response) :-
        catch(Handler(Request, Response0), E, err_handler(ErrHandler, E, Response0)),
	!,
	Response = Response0.
http_protect(_, ErrHandler, _Request, Response) :-
        err_handler(ErrHandler, unexpected_failure, Response).

:- meta_predicate err_handler(pred(2), ?, ?).
err_handler(ErrHandler, Err, Response) :- ErrHandler(Err, Response).

% ---------------------------------------------------------------------------
% Logging

:- doc(bug, "make logs configurable").

:- use_module(library(write), [writeq/1]).
:- use_module(library(format), [format/2]).
:- use_module(library(http/http_date), [http_date_str/3]).

logging :- fail.
%logging.

log(note, X) :- logging, !,
	writeq(X),nl.
log(error, X) :- !,
	writeq(X),nl.
log(_, _).

% TODO: Use some standarized log format, e.g.:
%   http://httpd.apache.org/docs/current/logs.html#combined
% similar to the combined log format but with user agent and referrer:
%   RemoteAddr - - [Time] "Method URI Protocol ResponseStatus ResponseBytes" EllapsedTime

% TODO: Log the full response/request instead?
log_response(Request, Response) :-
	date_time(_, CurrDate),
	http_date_str(CurrDate, DateStr, []),
	( member(uri(URIStr), Request) -> true ; URIStr = "?" ),
	( member(remote_addr(RemoteAddr), Request) -> true ; RemoteAddr = '?' ),
	( member(user_agent(UserAgent), Request) -> true ; UserAgent = '?' ),
	display(RemoteAddr),
	display(' - - ['),
	write_string(user_output, DateStr),
	display('] "'),
	write_string(user_output, URIStr),
	% display(Method),
	% display(Protocol),
	% display(Response ...),
	display('" "'),
	display(UserAgent),
	display('"'),
	( Response = not_found(_) ->
	    display(' code 404, not found')
	; true
	),
	nl.
