:- module(actmod_server, [actmodmain/0], [assertions]).

:- use_module(user, [exe/2]).
:- use_module(library(system)).
:- use_module(library(sockets)).
:- use_module(library(lists)).
:- use_module(library(read)).
:- use_module(library(aggregates), [findall/3]).

:- multifile save_addr_actmod/1.

actmodmain :-
        current_prolog_flag(argv, Args),
        bootargs(Args).

bootargs([PORT|More]) :-
        atom_codes(PORT, Chs),
        number_codes(Port, Chs), !,
        bind_socket(Port, 5, Socket),
        save_addr(Port,More),
	serve_socket(Socket,[]).
bootargs(More) :-
        bind_socket(Port, 5, Socket),
        inform_user(['Active module connected at port ',Port]),
        save_addr(Port,More),
	serve_socket(Socket,[]).

save_addr(Port,More) :-
        current_host(Host),
        save_addr_actmod([a(Host,Port)|More]).

serve_socket(Socket, Streams0) :-
	wait_for_arrival(Socket,Streams0,ReadableStreams,Streams1),
	serve_streams(ReadableStreams,Streams1,Streams2),
	serve_socket(Socket,Streams2).

/* Waits for either one or all of the following:
	1) A connection is done to 'Socket'
        2) It is possible to read from a stream
*/
wait_for_arrival(Socket, Streams0, ReadableStreams, Streams) :-
	select_socket(Socket, NewStream, off, Streams0, ReadableStreams),
	new_stream_in_list(NewStream, Streams0, Streams).

new_stream_in_list(NewStream, Streams, Streams) :-
	var(NewStream), !.
new_stream_in_list(NewStream, Streams0, [NewStream|Streams0]).

serve_streams([],SS,SS).
serve_streams([Stream|Streams],SS0,SS) :-
	read(Stream, Goal),
	serve_one_stream(Goal,Stream,SS0,SS1),
	serve_streams(Streams,SS1,SS).

serve_one_stream(end_of_file,S,SS0,SS) :- !,
        % end of file, that is, a broken connection
	close(S),
	delete(SS0,S,SS).
serve_one_stream(Goal,Stream,SS,SS) :-
        ( user:exe(Goal, Meta) -> true
        ; inform_user(['Goal not recognized: ',Goal]),
          Meta = fail
        ),
        findall(Goal,Meta,Answers),
	set_output(Stream),
        display_term(Answers), flush_output,
        set_output(user_output).
