:- module(actmodrt, [remote_call/2], [assertions]).

:- use_module(library(sockets)).
:- use_module(library(read)).

remote_call(a(Host,Port), Goal) :-
        remote_call_stream(Host, Port, Stream),
        current_output(OldOut),
        set_output(Stream),
        display_term(Goal),
        flush_output,
        set_output(OldOut),
        read(Stream, Answers),
        member(Goal, Answers).

:- data remote_stream/3.

remote_call_stream(Host, Port, Stream) :-
        current_fact(remote_stream(Port, Host, Stream)),
        current_stream(_N,socket,Stream), !.
remote_call_stream(Host, Port, Stream) :-
	connect_to_socket(Host, Port, Stream),
        retractall_fact(remote_stream(_, _, _)),
        assertz_fact(remote_stream(Port, Host, Stream)).
