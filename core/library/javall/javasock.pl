:- module(javasock, [
	bind_socket_interface/1,
	start_socket_interface/2,
	stop_socket_interface/0,
	join_socket_interface/0,
	java_query/2,
	java_response/2,
	prolog_query/2,
	prolog_response/2,
	is_connected_to_java/0,
	java_debug/1,
	java_debug_redo/1,
	start_threads/0
	],
	[assertions,regtypes,isomodes]).

:- doc(title,"Low-level Prolog to Java socket connection").

:- doc(author,"Jes@'{u}s Correas").

:- doc(module,"

@cindex{Socket implementation} This module defines a low-level socket
interface, to be used by javart and jtopl. Includes all the code
related directly to the handling of sockets. This library should not
be used by any user program, because is a very low-level connection to
Java. Use @lib{javart} (Prolog to Java interface) or @lib{jtopl} (Java
to Prolog interface) libraries instead.

").

:- use_module(library(fastrw), [fast_read/1, fast_write/1]).
:- use_module(library(sockets)).
:- use_module(library(format)). 
:- use_module(library(concurrency)). 
:- use_module(library(javall/jtopl)).

:- regtype machine_name(?Name) # "@var{Name} is a valid host name.".
machine_name(X) :- atm(X).

:- pred java_stream(PJStream, JPStream, Address,Stream)
	:: struct * struct * machine_name * stream # "Stores the identifiers
        of the streams used. A fact is asserted when the connection 
        to the Java process is established. It Contains 
        prolog-to-java and java-to-prolog streams,
	and the network	address where the Java process is running.
        Last argument represents the Java process standard input stream.".
:- concurrent java_stream/4.

:- pred java_threads(PJIn,PJOut,JPIn,JPOut,PLServer)
	:: int * int * int * int * int # "Stores the threads used to
        handle the sockets and the goal server.".
:- data java_threads/5.

%% -----------------------------------------------------------------------
%% MESSAGE QUEUES
%% Dynamic predicates used for communication between user threads and 
%% socket handling threads.
%% -----------------------------------------------------------------------
%% -----------------------------------------------------------------------
:- pred java_query(ThreadId,Query)
	:: atm * term # "Data predicate containing the queries to be sent
        to Java. First argument is the Prolog thread Id, and second
        argument is the query to send to Java.".
%% -----------------------------------------------------------------------
:- concurrent java_query/2.

%% -----------------------------------------------------------------------
:- pred java_response(Id,Response)
	:: atm * term # "Data predicate that stores the responses to
        requests received from Java. First argument corresponds to
        the Prolog thread Id; second argument corresponds to the
        response itself.".
%% -----------------------------------------------------------------------
:- concurrent java_response/2.

%% -----------------------------------------------------------------------
:- pred prolog_query(Id, Query)
	:: int * term # "Data predicate that keeps a queue of the queries
        requested to Prolog side from Java side.".
%% -----------------------------------------------------------------------
:- concurrent prolog_query/2.

%% -----------------------------------------------------------------------
:- pred prolog_response(Id, Response)
	:: int * term # "Data predicate that keeps a queue of the responses
        to queries requested to Prolog side from Java side.".
%% -----------------------------------------------------------------------
:- concurrent prolog_response/2.

%% -----------------------------------------------------------------------
:- pred start_socket_interface(+Address,+Stream) 
	:: term * stream
        # "Given an address in format 'node:port', creates the sockets
        to connect to the java process, and starts the threads needed
        to handle the connection.". 
%% -----------------------------------------------------------------------

start_socket_interface(Node:SocketId,Stream):-
        java_client(Node:SocketId,Stream),
	!,
	start_threads.

%% -----------------------------------------------------------------------
:- pred start_threads

        # "Starts the threads that will handle the connection to
          Java. This predicate is declared public for internal
          purposes, and it is not intended to be used by a user
          program.".
%% -----------------------------------------------------------------------
start_threads:-
	eng_call(pj_socket_reader, create, create, PjIn),
	eng_call(pj_socket_writer, create, create, PjOut),
	eng_call(jp_socket_reader, create, create, JpIn),
	eng_call(jp_socket_writer, create, create, JpOut),
	eng_call(shell_s,create,create,PS),
	set_fact(java_threads(PjIn,PjOut,JpIn,JpOut,PS)).

%% -----------------------------------------------------------------------
:- pred stop_socket_interface # "Closes the sockets to disconnect from 
        the java process, and waits until the threads that handle the
        connection terminate.".
%% -----------------------------------------------------------------------
stop_socket_interface :-
	assertz_fact(java_query(0,'$terminate')),
	assertz_fact(prolog_response(0,'$terminate')),
	join_socket_interface,
	retract_fact(java_threads(PjIn,PjOut,JpIn,JpOut,PlServer)),
	eng_release(PjIn),
	eng_release(PjOut),
	eng_release(JpIn),
	eng_release(JpOut),
	eng_release(PlServer),
%        retract_fact(java_stream(DataStream,EventStream,_,StdStream)),
%        close(DataStream),
%        close(EventStream),
	retractall_fact(java_query(_,_)),
	retractall_fact(java_response(_,_)),
	retractall_fact(prolog_query(_,_)),
	retractall_fact(prolog_response(_,_)),
	(var(StdStream) ->
	 true
	;
	 close(StdStream)
	),
	!.

%% -----------------------------------------------------------------------
:- pred join_socket_interface # "Waits until the threads that handle the
        connection terminate.".
%% -----------------------------------------------------------------------
join_socket_interface:-
	java_threads(PjIn,PjOut,JpIn,JpOut,PlServer),
	eng_wait(PjIn),
	eng_wait(PjOut),
	eng_wait(JpIn),
	eng_wait(JpOut),
	eng_wait(PlServer),
%	!,
	retract_fact_nb(java_stream(DataStream,EventStream,_,_)),
        close(DataStream),
        close(EventStream).

join_socket_interface.

%% -----------------------------------------------------------------------
:- pred pj_socket_reader/0 # "Predicate that runs in a separate thread
	reading from the prolog-to-java socket. If receives a disconnect
        or terminate request, just finish the thread.".
%% -----------------------------------------------------------------------
pj_socket_reader :-
	repeat,
	  java_fast_read(pj,pj(Id,R)),
	  java_debug('pj_socket_reader: pj'(Id,R)),
	  (termination_check(Id,R) -> 
	   assertz_fact(java_query(Id,R)),
	   true
	  ;
	   assertz_fact(java_response(Id,R)),
	   fail
	  ),
	  !.

%% -----------------------------------------------------------------------
:- pred pj_socket_writer/0 # "Predicate that runs in a separate thread
	writing to the prolog-to-java socket. If receives a disconnect
        or terminate request, just finish the thread.".
%% -----------------------------------------------------------------------
pj_socket_writer :-
	retract_fact(java_query(Id,Q)),
	java_debug('pj_socket_writer: pj'(Id,Q)),
	java_fast_write(pj,pj(Id,Q)),
	(termination_check(Id,Q) -> 
	 true
	;
	 fail
	),
	!.

%% -----------------------------------------------------------------------
:- pred jp_socket_reader/0 # "Predicate that runs in a separate thread
	reading from the java-to-prolog socket. If receives a disconnect
        or terminate request, sends the termination message to the peer
        thread (that handles jp socket writing), and to the Prolog goal
        handler. Finally, terminates itself.
        IMPORTANT: a special query internal_use_module/1 is asserted
        from javart in prolog_query/2 predicate.".
%% -----------------------------------------------------------------------
jp_socket_reader :-
        repeat,
          java_fast_read(jp,jp(Id,Q)),
	  java_debug('jp_socket_reader: jp'(Id,Q)),
	  (termination_check(Id,Q) -> 
	   assertz_fact(prolog_query(0,Q)),
	   assertz_fact(prolog_response(0,Q))
	  ;
	   assertz_fact(prolog_query(Id,Q)),
	   fail
	  ),
	  !.

%% -----------------------------------------------------------------------
:- pred jp_socket_writer/0 # "Predicate that runs in a separate thread
	writing to the java-to-prolog socket. If receives a disconnect
        or terminate request, just finish the thread.".
%% -----------------------------------------------------------------------
jp_socket_writer :-
	retract_fact(prolog_response(Id,R)),
	java_debug('jp_socket_writer:jp'(Id,R)),
	java_fast_write(jp,jp(Id,R)),
	(termination_check(Id,R) -> 
	 true
	;
	 fail
	),
	!.

%% -----------------------------------------------------------------------
:- pred termination_check(+Term,+Term)
	:: term * term # "Checks if the termination atom is received.".
%% -----------------------------------------------------------------------

termination_check(0,'$terminate').
termination_check(0,'$disconnect').

%% -----------------------------------------------------------------------
:- pred java_client(+Address,+Stream) 
	:: term * stream # "Opens a connection at an address, and asserts the 
        @tt{java_stream} corresponding fact.".
%% -----------------------------------------------------------------------

java_client(Address,Stream) :-
        open_client(Address,DataStream,EventStream),
	set_fact(java_stream(DataStream,EventStream,Address,Stream)).

%% -----------------------------------------------------------------------
:- pred open_client(+Address, -Stream, -Stream)
	:: term * stream * stream
        # "Given an address (@tt{Host:Port} format), creates and synchronizes
	  the sockets to the java process.".
%% -----------------------------------------------------------------------

open_client(Host:Port,PJStream,JPStream) :-
	open_pj(Host, Port, PJStream),
	open_jp(Host, Port, JPStream).

open_pj(Host, Port, PJStream) :-
        connect_to_socket(Host, Port, PJStream),
	java_fast_write0(PJStream,pj(0,data)),
        java_fast_read0(PJStream, pj(0,data)).

open_jp(Host, Port, JPStream) :-
        connect_to_socket(Host, Port, JPStream),
	java_fast_write0(JPStream,jp(0,event)),
        java_fast_read0(JPStream, jp(0,event)).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Prolog server code.
%% -----------------------------------------------------------------------
:- pred bind_socket_interface(+Port) 
	:: int
        # "Given an port number, waits for a connection request from
        the Java side, creates the sockets to connect to the java
        process, and starts the threads needed to handle the
        connection.".
%% -----------------------------------------------------------------------
bind_socket_interface(Port):-
        java_server(Port),
	!,
	start_threads.

%% -----------------------------------------------------------------------
:- pred java_server(+Port)
	:: int
        # "Given a @var{Port}, waits for a connection request from a
	  Java client and synchronizes the sockets to the java
	  process.".
%% -----------------------------------------------------------------------
:- use_module(library(sockets/sockets_io), [serve_socket/3]).

java_server(Port) :-
	java_debug('inside open_server'),
	set_fact(java_stream(_,_,_,_)),
        bind_socket(Port,1, Sock),
	eng_call(serve_socket(Sock,binding,sock_error),create,create),
	wait_connection,
	java_debug('after open_server').

binding(Stream):-
        java_fast_read0(Stream, Term),
	(Term = pj(0,data) ->
	 bind_pj(Stream)
	;(Term = jp(0,event) ->
	  bind_jp(Stream)
	 ;
	     format(user_error,
	     '{ERROR: Socket error accepting connections from java!~n}',[]),
	     !,
	     fail
	 )).

bind_pj(PJStream) :-
%        java_fast_read0(PJStream, pj(0,data)),
	java_fast_write0(PJStream,pj(0,data)),
	java_stream(_,JPStream,Address,Stream),
	set_fact(java_stream(PJStream,JPStream,Address,Stream)).
	

bind_jp(JPStream) :-
%        java_fast_read0(JPStream, jp(0,event)),
	java_fast_write0(JPStream,jp(0,event)),
	java_stream(PJStream,_,Address,Stream),
	set_fact(java_stream(PJStream,JPStream,Address,Stream)).

sock_error(Error):-
	format(user_error,
	'{ERROR: Socket error while trying to connect to java!~n~w}',[Error]),
	fail.

wait_connection:-
	java_debug('waiting for connection established'),
	java_stream(PJStream,JPStream,_,_),
	(var(PJStream) ; var(JPStream)),
	!,
	wait_connection.
wait_connection.
	
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% -----------------------------------------------------------------------
:- pred java_fast_write(+Type, +Term) 
	:: atom * term # " It writes on the given stream type the term
        received as second argument. This is the basic predicate used
        to send data to the Java side. The first argument
	reflects the socket (prolog-to-java or java-to-prolog).
        The second argument is the term to be sent to the Java side.".
%% -----------------------------------------------------------------------
java_fast_write(pj,T) :-
        current_fact(java_stream(PJStream,_,_,_)),
        java_fast_write0(PJStream,T),
        !.  %% Avoid choicepoints (none should have been pushed---just in case)

java_fast_write(jp,T) :-
        current_fact(java_stream(_,JPStream,_,_)),
        java_fast_write0(JPStream,T),
        !.  %% Avoid choicepoints (none should have been pushed---just in case)

java_fast_write0(Stream,T) :-
        (current_stream(_N,socket,Stream) ->
	 current_output(CU),
	 set_output(Stream),
	 fast_write(T),
	 flush_output(Stream),
	 set_output(CU)
	;
	 format(user_error,
	 '{ERROR: the connection with java has been shut down!}',[]),
	 fail
        ).

%% -----------------------------------------------------------------------
:- pred java_fast_read(+Type, -Term) :: atom * term # "It reads from the
	given stream type one term and unifies it with the term received as
	second argument. This is the basic predicate used to receive data
	from the Java side. The first argument reflects the socket type
	(prolog-to-java or java-to-prolog). The second argument is
        unified with the data received from the socket.".
%% -----------------------------------------------------------------------

java_fast_read(pj, T) :-
        current_fact(java_stream(PJStream,_,_,_)),
        java_fast_read0(PJStream,T),
        !.  %% Avoid choicepoints (none should have been pushed---just in case)

java_fast_read(jp, T) :-
        current_fact(java_stream(_,JPStream,_,_)),
        java_fast_read0(JPStream,T),
        !.  %% Avoid choicepoints (none should have been pushed---just in case)

java_fast_read0(Stream,T) :-
        (current_stream(_N,socket,Stream) ->
	 current_input(CU),
	 set_input(Stream),
	 fast_read(T),
	 set_input(CU)
        ;
	 format(user_error,
	 '{ERROR: the connection with java has been shut down!}',[]),
	 fail
        ).

%% -----------------------------------------------------------------------
:- pred is_connected_to_java/0
	# "Checks if the connection to Java is established.".
%% -----------------------------------------------------------------------
is_connected_to_java :-
	current_fact_nb(java_stream(_,_,_,_)).

%%------------------------------------------------------------------
%% ONLY FOR DEBUGGING
%%------------------------------------------------------------------

:- data debugging/0.
% Comment/uncomment next line to set debugging off/on.
%debugging.

java_debug(T) :-
	debugging,
	open('javasock.log',append,S),
	display(S,T),nl(S),
	close(S),
	!.

java_debug(_) :- !.

java_debug_redo(X):-
	java_debug(do(X)).
java_debug_redo(X):-
	java_debug(redo(X)), fail.
