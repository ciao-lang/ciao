:- module(stream_watchdog, [], [assertions]).

:- doc(title, "Stream watchdog (Non-Blocking IO)").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module implements a @em{stream watchdog} module,
   which allows attaching (@em{user}-defined) attributes to streams,
   add or remove streams from a @em{watch} list, and monitoring
   watched streams for available data.").

:- use_module(library(sockets), [select_socket/5]).
:- use_module(library(aggregates), [findall/3]).

% ---------------------------------------------------------------------------
% Stream watchdog state

:- doc(bug, "Indexing on streams (for data preds) is inefficient; improve it").

:- pred watched_stream(Stream, StreamAttr) # "Watch @var{Stream}
   (annotated with attribute @var{StreamAttr} for new data".
:- data watched_stream/2.

:- pred ready_stream(Stream) # "@var{Stream} contains data to be read".
:- data ready_stream/1.

:- pred watched_socket(Socket, NewStreamAttr) #
   "Watch a socket @var{Socket} for new streams. Each new connection
   is automatically watched using @var{NewStreamAttr} as attribute.".
:- data watched_socket/2.

% ---------------------------------------------------------------------------

:- export(init_stream_watch/0).
:- pred init_stream_watch # "Initialize the stream watchdog module".

init_stream_watch :-
	retractall_fact(ready_stream(_)),
	retractall_fact(watched_stream(_,_)).

% ---------------------------------------------------------------------------

:- export(watch_stream/2).
:- pred watch_stream(Stream, StreamAttr) # "Add stream @var{Stream} to
   the watch list (with attribute @var{StreamAttr})".

watch_stream(Stream, StreamAttr) :-
	( current_fact(watched_stream(Stream, _)) ->
	    throw(already_watched(Stream))
	; assertz_fact(watched_stream(Stream, StreamAttr))
	).

:- export(unwatch_stream/1).
:- pred unwatch_stream(Stream) # "Remove @var{Stream} from the watch
   list".

unwatch_stream(Stream) :-
	( retract_fact(watched_stream(Stream, _)) -> true
	; true 
	).

:- doc(bug, "@pred{watch_socket/2}: only one socket can be watched").

:- export(watch_socket/2).
:- pred watch_socket(Socket, NewStreamAttr) # "Add socket @var{Socket}
   to the watch list (with attribute @var{NewStreamAttr} for new
   incomming streams)".

watch_socket(Socket, NewStreamAttr) :-
	retractall_fact(watched_socket(_, _)),
	assertz_fact(watched_socket(Socket, NewStreamAttr)).

:- export(unwatch_socket/1).
:- pred unwatch_socket(Socket) # "Remove @var{Socket} from the watch
   list".

unwatch_socket(Socket) :-
	retractall_fact(watched_socket(Socket, _)).

% ---------------------------------------------------------------------------

:- doc(bug, "@pred{wait_streams/1}: It must use pselect() instead of
   select() to avoid race conditions for signals. See example at
   @href{https://linux.die.net/man/2/select_tut}").

:- doc(bug, "@pred{wait_streams/1}: return ellapsed time, needed for
   schedulers. Note that select() C function does not update the
   timeout once we have received a message with the remaining time,
   which would be necessary for multiple timeouts (use
   gettimeofday).").

:- export(wait_streams/1).
:- pred wait_streams(Timeout) # "Wait until some watched streams
   change to ready status or @var{Timeout} has expired.".

wait_streams(Timeout) :-
	( watched_socket(Socket,NewStreamAttr) -> true ; Socket = no ),
	findall(X, other_stream(X), InStreams),
	( Socket = no, \+ watched_stream(_,_) ->
	    ReadableStreams = dried_streams % no new messages can arrive
	; select_socket(Socket,NewStream,Timeout,InStreams,ReadableStreams),
	  % Watch a new socket connection if needed
	  ( var(NewStream) -> true
	  ; watch_stream(NewStream, NewStreamAttr)
	  ),
	  % TODO: OS signals may interrupt select() with no ready
	  %   stream (ReadableStream=[] after select_socket/5). Repeat?
	  ( % (failure-driven loop)
	    member(S, ReadableStreams),
	      assertz_fact(ready_stream(S)), % TODO: unwatch and watch again?
	      fail
	  ; true
	  )
	).

% A watched stream that is not marked as ready
other_stream(S) :- watched_stream(S, _), \+ ready_stream(S).

% ---------------------------------------------------------------------------

:- export(dried_streams/0).
:- pred dried_streams # "Streams are dried: there are no socket to
   accept new connections, nor streams to watch.".

dried_streams :-
	\+ watched_socket(_,_),
	\+ watched_stream(_,_).

% ---------------------------------------------------------------------------

:- export(current_ready_stream/3).
:- pred current_ready_stream(Stream, StreamAttr, Ref) # "Enumerate watched
   streams with ready data. @var{StreamAttr} corresponds to the attribute
   assigned to @var{Stream}. Use @tt{erase(Ref)} to remove them from
   the ready queue before processing.".

current_ready_stream(Stream, StreamAttr, Ref) :-
	current_fact(ready_stream(Stream), Ref),
	( current_fact(watched_stream(Stream, StreamAttr0)) ->
	    StreamAttr = StreamAttr0
	; throw(bug(no_watched_stream(Stream), current_ready_stream/3)) % (buf if reachable)
	).
