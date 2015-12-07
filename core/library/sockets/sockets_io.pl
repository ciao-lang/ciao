
:- module(sockets_io,
	[ serve_socket/3, safe_write/2 ],
	[ assertions, hiord, regtypes ]
	 ).
:- use_module(library(lists), [delete/3]).
%:- use_module(library(read),[read/2]).
:- use_module(library(sockets), [select_socket/5]). 

:- doc(title,"Sockets I/O").

:- doc(author,"Francisco Bueno").

:- doc(module,"This module provides two useful predicates for
   programming with sockets.").

:- true pred serve_socket(Socket,Server,Handler)
	: socket * callable * callable
        # "Handles the streams associated to @var{Socket} calling
           @var{Server} on one request of each stream 
           (as @var{Server(Stream)}),
           and @var{Handler(Stream)} if the stream is empty
           (connection closed).".
:- meta_predicate serve_socket(?,pred(1),pred(1)).

serve_socket(Socket,Server,Handler) :-
	serve_socket_1(Socket,Server,Handler,[]).

serve_socket_1(Socket,Server,Handler,Streams0) :-
	wait_for_arrival(Socket,Streams0,ReadableStreams,Streams1),
	serve_streams(ReadableStreams,Server,Handler,Streams1,Streams2),
	serve_socket_1(Socket,Server,Handler,Streams2).

/* Waits for either one or all of the following:
	1) A connection is done to 'Socket'
        2) It is possible to read from a stream
*/
wait_for_arrival(Socket,Streams0,ReadableStreams,Streams) :-
	select_socket(Socket,NewStream,off,Streams0,ReadableStreams),
	new_stream_in_list(NewStream,Streams0,Streams).

new_stream_in_list(NewStream,Streams,Streams) :-
	var(NewStream),!.
new_stream_in_list(NewStream,Streams0,[NewStream|Streams0]).

serve_streams([],_S,_H,SS,SS).
serve_streams([Stream|Streams],Server,Handler,SS0,SS) :-
%jcf% This predicate cannot be used here because reads to
%jcf% end_of_file...
%jcf%	stream_to_string(Stream,Request),
%	socket_recv(Stream,Request),
%	read(Stream,Request),
	serve_one_stream(Stream,Server,Handler,SS0,SS1),
	serve_streams(Streams,Server,Handler,SS1,SS).

%jcf%serve_one_stream(S,_Server,Handler,SS0,SS) :- !,
%jcf%        % end of file, that is, a broken connection
%jcf%	Handler(S),
%jcf%	close(S),
%jcf%	delete(SS0,S,SS).
serve_one_stream(Stream,Server,Handler,SS0,SS) :-
	catch(Server(Stream),Error,Handler(Error)),
	delete(SS0,Stream,SS).

:- true pred safe_write(Stream,Term)
	: stream * term
        # "Writes @var{Term} to @var{Stream} in a way that it is safe
           for a socket connection on @var{Stream}.".

safe_write(Stream,Term):-
	current_output(Stream0),
	set_output(Stream),
	display_term(Term),
	flush_output,
	set_output(Stream0).

:- regtype socket(S) # "@var{S} is a socket id.".
socket(S):- int(S).
