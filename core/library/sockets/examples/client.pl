:- module(client, [main/1], []).

:- use_module(library(format), [format/2]).
:- use_module(library(random), [random/3]).
:- use_module(library(strings), [get_line/2]).
:- use_module(library(sockets), [connect_to_socket/3]).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(concurrency)).

:- use_module(socket_number).


%% Write two numbers a socket whose host and port number are passed in
%% as argument. 

main(['--help']):- main([]).
main(['-help']):- main([]).
main(['help']):- main([]).
main([]):-
        format("
Usage: client <how_many> <host>: write to the server running
in the machine <host> an amount of <how_many> random pairs of numbers.  In
the designated <host> an instance of the client program should be running;
it will read these pairs, add them, and return the result, which is locally 
checked.  The port number to connect is fixed at compile time.  
", []).

main([HowMany, Host]):-
        atom_and_number(HowMany, N),
        socket_port(Port),
        statistics(walltime, _),
        write_lots_of_data(N, Host, Port).

write_lots_of_data(0, _Host, _Port).
write_lots_of_data(N, Host, Port):-
        N > 0,
        write_progress(N),
        random(1,1000,R1),
        random(1,1000,R2),
        connect_to_socket(Host, Port, Stream), 
        display(Stream, R1), 
        nl(Stream),
        display(Stream, R2),
        nl(Stream),
        get_line(Stream, NumberStr),
        number_codes(Number, NumberStr),
 %% It is very important to close the stream at each end, or the
 %% process itself will simply eventually die!
        close(Stream),  
        check_correction(R1, R2, Number),
        N1 is N - 1, 
        write_lots_of_data(N1, Host, Port).

atom_and_number(A, N):-
        atom_codes(A, Codes),
        number_codes(N, Codes).


write_progress(N):-
        N mod 100 =:= 0 ->
        statistics(walltime, [Total, Relative]),
        display(N), display(':'), display(Total),
        display(':'), display(Relative), nl
 ;
        true
 .


check_correction(R1, R2, Number):-
        Number =:= R1 + R2 ->
        true
 ;
        display('Error in computation!!!!'),
        nl
 .
