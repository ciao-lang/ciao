:- module(server, [main/1], []).

:- use_module(library(system)).
:- use_module(library(sockets)).
:- use_module(library(strings)).
:- use_module(library(iso_char)).
:- use_module(library(format)).
:- use_module(library(concurrency)).

:- use_module(socket_number).

%% Listens to a socket, reads in two integers (each in a line),
%% and writes the result of adding them.

%% 

main(['--help']):- main([]).
main(['-help']):- main([]).
main(['help']):- main([]).

main([]):-
       format("
Usage: server <num_threads>, where num_threads is the number of threads
to start.
", []).


main([NumberThreads]):-
        atom_codes(NumberThreads, Codes),
        number_codes(Number, Codes),
        create_threads(Number),
        get_socket(Socket),
        wait_for_connections(Socket).

main(_):-
        format("Error in invocation!~n", []),
        main([]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fixed number of threads

:- concurrent connection/1.

wait_for_connections(Socket):-
        repeat,
        socket_accept(Socket, Stream),
%        socket_buffering(Stream, read, _Old, unbuf),
        assertz_fact(connection(Stream)),
        fail.

create_threads(0).
create_threads(N):-
        N > 0,
        eng_call(handle_connection, create, create),
        N1 is N - 1,
        create_threads(N1).

handle_connection:-
        retract_fact(connection(Stream)),
        handle_stream(Stream),
        fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common predicates


%% Build a socket and tell us which port it is bound to.

num_of_connections(100).

get_socket(Socket):-
        num_of_connections(Queue),
        current_host(Host),
        socket_port(Port),
        bind_socket(Port, Queue, Socket),  %% Already in "listen" state
        display('Bound to port '),
        display(Port),
        display(' in host '),
        display(Host),
        nl.


%% Receive a stream, and read two nuber from it, add them, and write
%% the result to the standard output.  A negative number will stop
%% the daemon.

handle_stream(Stream):-
        get_line(Stream, FirstNumber),
        number_codes(N1, FirstNumber),
        get_line(Stream, SecondNumber),
        number_codes(N2, SecondNumber),
        N is N1 + N2,
        display(Stream, N), 
        nl(Stream),
        close(Stream).
