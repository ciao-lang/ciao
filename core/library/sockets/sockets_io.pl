:- module(sockets_io, [
    serve_socket/3,
    socket_send_term/2,
    socket_recv_term/2,
    socket_send_fastrw/2,
    socket_recv_fastrw/2
   ], [assertions, hiord, regtypes]).

:- doc(title, "Sockets I/O").
:- doc(author, "Francisco Bueno").
:- doc(author, "Jose F. Morales (minor changes, queue version)").

:- doc(module, "This module implements a socket-based message reading
   loop.").

:- use_module(library(lists), [delete/3]).
:- use_module(library(sockets), [select_socket/5]). 

% ---------------------------------------------------------------------------

:- regtype socket(S) # "@var{S} is a socket id.".
socket(S):- int(S).

% ---------------------------------------------------------------------------

:- pred serve_socket(Socket,Serve,Handler) :: socket * callable * callable
   # "Handles the streams associated to @var{Socket} calling
      @var{Serve} on one request of each stream (as
      @var{Serve(Stream,Unwatch)}), and @var{Handler(Stream)} if the
      stream is empty (connection closed). It is expected that
      @var{Unwatch} is unified with @tt{yes} or @tt{no} after calls to
      @var{Serve}.  If @tt{Unwatch=yes} then the @var{Stream} is
      removed from the watch list (e.g., useful when the application
      will perform IO on that stream from a separate thread)".

:- meta_predicate serve_socket(?,pred(2),pred(1)).
serve_socket(Socket,Serve,Handler) :-
    serve_socket_(Socket,Serve,Handler,[]).

:- meta_predicate serve_socket_(?,pred(2),pred(1),?).
serve_socket_(Socket,Serve,Handler,Streams0) :-
    wait_for_arrival(Socket,Streams0,ReadableStreams,Streams1),
    serve_streams(ReadableStreams,Serve,Handler,Streams1,Streams2),
    serve_socket_(Socket,Serve,Handler,Streams2).

% Waits for either one or all of the following:
%   1) A connection is done to 'Socket'
%   2) It is possible to read from a stream

wait_for_arrival(Socket,Streams0,ReadableStreams,Streams) :-
    % TODO: use pselect() -- avoids race conditions?
    select_socket(Socket,NewStream,off,Streams0,ReadableStreams),
    new_stream_in_list(NewStream,Streams0,Streams).

new_stream_in_list(NewStream,Streams,Streams) :-
    var(NewStream),!.
new_stream_in_list(NewStream,Streams0,[NewStream|Streams0]).

:- meta_predicate serve_streams(?,pred(2),pred(1),?,?).
serve_streams([],_Serve,_H,SS,SS).
serve_streams([Stream|Streams],Serve,Handler,SS0,SS) :-
    serve_one_stream(Stream,Serve,Handler,SS0,SS1),
    serve_streams(Streams,Serve,Handler,SS1,SS).

:- meta_predicate serve_one_stream(?,pred(2),pred(1),?,?).
serve_one_stream(Stream,Serve,Handler,SS0,SS) :-
    catch(Serve(Stream,Unwatch),Error,Handler(Error)),
    ( Unwatch = yes ->
        delete(SS0,Stream,SS)
    ; SS0 = SS
    ).

% ---------------------------------------------------------------------------
% Serialize terms using term_write/1, read/2

:- use_module(engine(stream_basic)).
:- use_module(library(terms_io), [term_write/1]).
:- use_module(library(read), [read/2]).

:- pred socket_send_term(Stream,Term) :: stream * term
    # "Writes @var{Term} to @var{Stream} in a way that it is safe
       for a socket connection on @var{Stream}.".

socket_send_term(Stream,Term):-
    current_output(Stream0),
    set_output(Stream),
    term_write(Term),
    flush_output,
    set_output(Stream0).

socket_recv_term(Stream, Out) :-
    read(Stream, Out).

% ---------------------------------------------------------------------------
% Serialize terms using fastrw

:- use_module(library(fastrw), [fast_write/1, fast_read/1]).

socket_send_fastrw(Stream, T) :-
    current_output(CU),
    set_output(Stream),
    fast_write(T),
    flush_output(Stream),
    set_output(CU).

socket_recv_fastrw(Stream, T) :-
    current_input(CU),
    set_input(Stream),
    fast_read(T),
    set_input(CU).
