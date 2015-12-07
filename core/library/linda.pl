% ----------------------------------------------------------------------
%
% Linda communication for Distributed CIAO
% Loosely based on library file linda/client.pl of SICStus 2.1
% (C) UPM-CLIP 1997
%
% ----------------------------------------------------------------------

:- module(linda,[
	linda_client/1,
	close_client/0,
	in/1,
	in/2,
	in_noblock/1,
	out/1,
	rd/1,
	rd/2,
	rd_noblock/1,
        rd_findall/3,
	linda_timeout/2,
        halt_server/0,
        open_client/2,
        in_stream/2,
        out_stream/2],
	[assertions]).

:- use_module(library(read)).
:- use_module(library(fastrw), [fast_read/1, fast_write/1]).
% :- use_module(library(lists), [member/2]).
:- use_module(library(sockets/sockets)).

:- doc(module,"This is a SICStus-like linda package. Note that
   this is essentially quite obsolete, and provided mostly in case it
   is needed for compatibility, since Ciao now supports all Linda
   functionality (and more) through the concurrent fact database.").

:- data linda_stream/2, time_out/1.

time_out(30000). % 30 seconds

protocol(0'p).   % 0'f = special format (cannot be used by now), 0'p = displayq

linda_client(NewAddress) :-
	linda_stream(Stream,OldAddress),
	current_stream(_N,socket,Stream), !,
	(   NewAddress = OldAddress ->
	    true    % Seems ok, just ignore it
	;   inform_user(['{ERROR: linda_client/1: Already client to ',
                         OldAddress,'}'])
	).
linda_client(Address) :-
	retractall_fact(linda_stream(_,_)), %Might be a try to reestablish a broken conn.
	open_client(Address,Stream),
	asserta_fact(linda_stream(Stream,Address)),
	ping(Answer),
	ping_answer_ok(Answer).

ping_answer_ok(pong) :- !.
ping_answer_ok(A) :-
	inform_user(['{ERROR: linda_client/1: strange answer from server:',
                     A,'}']).	

open_client(Host:Port,Stream) :-
	atom(Host),
	integer(Port), !,
	connect_to_socket(Host, Port, Stream).
open_client(Addr,_Stream) :-
	inform_user(['{ERROR: open_client/3: Illegal network address: ',
                     Addr,'}']),
	fail.

%-----------------------------------------------------------------------------
close_client :-
	retract_fact(linda_stream(Stream,_Address)),
	close(Stream).

ping(Answer) :-	
	to_linda(0'p, ping),
	time_out_select,
	from_linda(Answer).

out(T) :-
	to_linda(0'o, T).

in(T) :-
        to_linda(0'i, T),
	from_linda(T).

in(Tuples,Tuple) :-
	Tuples = [_|_],
        to_linda(0'I, Tuples),
	from_linda(Tuple), !,
	member(Tuple,Tuples).  % for unification of Tuples with answer

in_noblock(T) :-
        to_linda(0'j, T),
	time_out_select,
	from_linda_code(0's, T).
	
rd_noblock(T) :-
	to_linda(0's, T),
	time_out_select,
	from_linda_code(0's, T).

rd(T) :-
	to_linda(0'r, T),
	from_linda(T).

rd(Tuples,Tuple) :-
	Tuples = [_|_],
        to_linda(0'R, Tuples),
	from_linda(Tuple), !,
	member(Tuple,Tuples).  % for unification of Tuples with answer

rd_findall(Template,Tuple,Bag) :- 
	to_linda(0'f, f(Template,Tuple,Bag)),
	time_out_select,
	from_linda(Bag).

halt_server :- % makes also close_client
	linda_stream(Stream,Address),
	(   current_stream(_N,socket,Stream) ->
	    current_output(CU),
	    set_output(Stream),
	    protocol(P),
            put_code(P),
            put_code(0'h),
	    flush_output(Stream),
	    set_output(CU),
	    current_input(CI),
	    set_input(Stream),
	    get_code(_),
	    set_input(CI),
            close_client
        ;   linda_client(Address) -> % Connection broken; could reestablish it
	    halt_server
        ;   inform_user(['{ERROR: the connection with linda has been shut down, can''t reopen it!}']),
	    fail
        ).

in_stream(Stream,T) :-
        to_linda_stream(Stream, 0'i, T),
	from_linda_stream(Stream, T).

out_stream(Stream, T) :-
        to_linda_stream(Stream, 0'o, T).

% linda_trace(OnOff) :-
% 	(var(OnOff) ; OnOff=on ; OnOff=off), !,
% 	to_linda(0't, OnOff),
% 	time_out_select,
% 	from_linda(OnOff).
% 
% linda_call(Goal) :-
% 	to_linda(0'c, Goal),
% 	from_linda(0's, Goal).
        
%-----------------------------------------------------------------------------
to_linda(Code, Item) :-
	linda_stream(Stream,Address),
	(   current_stream(_N,socket,Stream) ->
	    current_output(CU),
	    set_output(Stream),
	    protocol(P),
	    write_out(P,Code,Item),
	    flush_output(Stream),
	    set_output(CU)
        ;   linda_client(Address) -> % Connection broken; could reestablish it
	    to_linda(Code, Item)
        ;   inform_user(['{ERROR: the connection with linda has been shut down, can''t reopen it!}']),
	    fail
        ).

to_linda_stream(Stream, Code, Item) :-
	(   current_stream(_N,socket,Stream) ->
	    current_output(CU),
	    set_output(Stream),
	    protocol(P),
	    write_out(P,Code,Item),
	    flush_output(Stream),
	    set_output(CU)
        ;   inform_user(['{ERROR: the connection has been shut down!}']),
	    fail
        ).

%-----------------------------------------------------------------------------
write_out(0'p,Code,Item) :- 
	put_code(0'p),
	put_code(Code), 
	displayq(Item), put_code(0'.), nl.
write_out(0'f,Code,Item) :-
	put_code(0'f), 
	put_code(Code), 
	fast_write(Item).

read_in(0'p,Item) :- read(Item).
read_in(0'f,Item) :- fast_read(Item).
%-----------------------------------------------------------------------------
from_linda(Item) :-
	linda_stream(Stream,_Address),
	current_input(S),
	set_input(Stream),
	protocol(P),
	read_in(P,Item),
	set_input(S).

from_linda_stream(Stream, Item) :-
	current_input(S),
	set_input(Stream),
	protocol(P),
	read_in(P,Item),
	set_input(S).

from_linda_code(Code, Item) :-
	linda_stream(Stream,_Address),
	current_input(S),
	set_input(Stream),
	get_code(Cl),
	(   Cl=Code ->
	    protocol(P),
	    read_in(P,Item),
	    set_input(S)
	;   set_input(S),
	    fail
	).

%-----------------------------------------------------------------------------
time_out_select :- time_out(TimeOut), time_out_select1(TimeOut).

time_out_select1(off) :- !.
time_out_select1(TimeOut) :-
	linda_stream(Stream,_Address),
	select_socket(_,_,TimeOut,[Stream],_), !.
time_out_select1(_) :-
	inform_user(['{ERROR: wait for linda timed out}']),
	fail.
%-----------------------------------------------------------------------------

linda_timeout(Old, New) :-
	linda_timeout_arg(Old, New),
	time_out(Old),
	retract_fact(time_out(Old)),
	asserta_fact(time_out(New)).

linda_timeout_arg(O, N) :- var(N), !, O==N.
linda_timeout_arg(_, MS) :- number(MS), MS>=0, !.
linda_timeout_arg(_, off).
