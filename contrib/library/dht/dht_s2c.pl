% This library is meant to be a prolog DHT implementation
% Copyright (C) 2006 "Arsen Kostenko" <kosty@clip.dia.fi.upm.es>
%
% This library is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
% Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this library; if not, write to the Free Software
% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
:- module(dht_s2c, 
         [
           dht_s2c_main/0
         ], 
         [
          assertions,regtypes,isomodes
         ]).


:- doc(title, "Server to client communication module").
:- doc(subtitle, "Ciao DHT implementation reference manual").
:- doc(copyright, "This library is developed by Arsen Kostenko as part
           of the Ciao system and is distributed under the GNU Lesser 
           General Public License. See the COPYING file in source package
           of visit the following Internet address: 
           @href{http://www.gnu.org/licenses/lgpl.html} for 
           the complete text of the license.").

:- doc(summary, "This is just a single module of the whole Ciao DHT 
           subsystem, and should be used mostly by developers (not
           casual users) of the system.").

:- doc(author, "Arsen Kostenko").

:- doc(module, "This module describes the server-2-client side behavior 
           of any node. Since the behavior is not very different this module 
           shares a number of fearures with @file{dht_s2c.pl}:
           @begin{itemize}
           @item{} extensive usage of the Ciao threading mechanism;
           @item{} interface made as simple as possible;
           @item{} @file{dht_server.pl} module is the only usage point;
           @item{} parameters are passed through the 
                   @file{dht_config.pl} module.
           @end{itemize}").

:- use_module(library(sockets)).
:- use_module(library(concurrency)).

:- use_module(dht_config).
:- use_module(dht_misc).
:- use_module(dht_logic).
:- use_module(dht_logic_misc, [consistent_hash/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Concurrent predicates.
:- concurrent conn/1.


:- regtype dht_directive_cons_b(DHTDirective) #
   "@var{DHTDirective} is a predicate of type @pred{dht_consult_b/2}, where
    the first argument is bound to an atom.".

dht_directive_cons_b(DHTDirective):-
      DHTDirective = dht_consult_b(Key, _Value),
      atm(Key).

:- regtype dht_directive_cons_nb(DHTDirective) #
   "@var{DHTDirective} is a predicate of type @pred{dht_consult_nb/2}, where
    the first argument is bound to an atom.".

dht_directive_cons_nb(DHTDirective):-
      DHTDirective = dht_consult_nb(Key, _Value),
      atm(Key).

:- regtype dht_directive_ext_b(DHTDirective) #
   "@var{DHTDirective} is a predicate of type @pred{dht_extract_b/2}, where
    the first argument is bound to an atom.".

dht_directive_ext_b(DHTDirective):-
      DHTDirective = dht_extract_b(Key, _Value),
      atm(Key).

:- regtype dht_directive_ext_nb(DHTDirective) #
   "@var{DHTDirective} is a predicate of type @pred{dht_extract_nb/2}, where
    the first argument is bound to an atom.".

dht_directive_ext_nb(DHTDirective):-
      DHTDirective = dht_extract_nb(Key, _Value),
      atm(Key).

:- regtype dht_directive_store(DHTDirective) #
   "@var{DHTDirective} is a predicate of type @pred{dht_store/2}, where
    the first argument is bound to an atom.".

dht_directive_store(DHTDirective):-
      DHTDirective = dht_store(Key, _Value),
      atm(Key).

:- regtype dht_directive_hash(DHTDirective) #
   "@var{DHTDirective} is a predicate of type @pred{dht_hash/2}, where
    the first argument is bound to an atom.".

dht_directive_hash(DHTDirective):-
      DHTDirective = dht_hash(Key),
      atm(Key).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_s2c_main:-
        dht_s2c_threads(Threads),
        create_threads(Threads, _),
        get_socket(Socket),
        eng_call(wait_for_connections(Socket), create, create).

:- doc(dht_s2c_main/0,
           "Generally speaking, this predicate must perform some common tasks
            like:@begin{itemize}
              @item listen to a port for incoming connections from clients, 
              @item and process requests from clients.
            @end{itemize} All the parameters needed are received from initial
            configuration, which is stored in @file{dht_config.pl} module.").

wait_for_connections(Socket):-
        repeat,
            socket_accept(Socket, Stream),
	    assertz_fact(conn(Stream)),
        fail.
        
:- doc(wait_for_connections/1,
           "Eternally accept incoming connection and store the for further
            processing.").

:- pred wait_for_connections(Socket): gnd => gnd #
    "Accept and store incoming connections.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_threads(0, []).
create_threads(N, [GoalId| T]):-
        N > 0,
        eng_call(handle_connection, create, create, GoalId),
        N1 is N - 1,
        create_threads(N1, T).
        
:- doc(create_threads/2,
           "Create number of connections for processing user requests.").

:- pred create_threads(N, List): int * list => int * list #
    "Create @var{N} threads and store their handler into @var{List}.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(handle_connection/0,
           "Eternally (at least at the moment - see 
            @pred{check_for_stop_command/1} predicate documentation) process 
            incoming streams with user-requests.").

:- pred handle_connection #
    "Handle a single stream with user requests. Once it is done - pick up a new
     stream from database.".

handle_connection:-
        repeat,
            retract_fact(conn(Stream)),
            display('Got connection:'), display(Stream), nl,
	    handle_single_connection(Stream, ServerLevelCommand),
        check_for_stop_command(ServerLevelCommand).
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(handle_single_connection/2,
           "This predicates actually performs reading of the stream and 
            and calls @pred{operate_with_client/2} for their correct 
            precessing.").

:- pred handle_single_connection(Stream, _ServerLevelCommand): gnd * var =>
                                                               gnd * var #
    "Process user command until @pred{end_of_file} term is received. 
     @var{_ServerLevelCommand} is almost useless at the moment. ".

:- pred handle_single_connection(Stream, _ServerLevelCommand): gnd * var =>
                                                               gnd * var #

    "Process user command until @pred{end_of_file} term is received. 
     @var{_ServerLevelCommand} is almost useless at the moment. ".

handle_single_connection(Stream, _ServerLevelCommand):-
            repeat,
                read_pr(Stream, Term),
                display('Got command: '),display(Term),nl,
                operate_with_client(Stream, Term),
            Term = 'end_of_file',
	    !.
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
:- doc(check_for_stop_command/1,
           "Theoretically there might be a special command for stopping the 
            server, however with the current state of affairs, there is none.
            So this predicates defaults to @pred{fail/0}.").

:- pred check_for_stop_command(Command): var => var #
    "Command is omitted. Body consists on out of @pred{fail/0} predicate.".

:- pred check_for_stop_command(Command): gnd => gnd #
    "Command is omitted. Body consists on out of @pred{fail/0} predicate.".

check_for_stop_command(_ServerLevelCommand):- fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
:- doc(operate_with_client/2,
           "This predicate takes care of processing user requests. There five
            main usages at this time.").

:- pred operate_with_client(Stream, EndOfFile)
	: gnd(Stream) => (EndOfFile = end_of_file) #
    "Once @pred{end_of_file/0} is received software just takes no action.".

:- pred operate_with_client(Stream, DHTDirectiveConsult): 
    gnd * dht_directive_cons_b => gnd * {dht_directive_cons_b,gnd} # 
    "Check presence of a term matching given pattern (pattern is supplied via 
     @var{Value}) in DHT. The predicate blocks if none is found at the
     moment.".

:- pred operate_with_client(Stream, DHTDirectiveConsult):
    gnd * {dht_directive_cons_b,gnd} => gnd * {dht_directive_cons_b,gnd} # 
    "Check presence of exact term in DHT. The predicate blocks if none is 
     found at the moment.".

:- pred operate_with_client(Stream, DHTDirectiveConsult): 
    gnd * dht_directive_cons_nb => gnd * {dht_directive_cons_nb,gnd} # 
    "Check presence of a term matching given pattern (pattern is supplied via 
     @var{Value}) in DHT. The predicate does not block if no match is found.".

:- pred operate_with_client(Stream, DHTDirectiveConsult):
    gnd * {dht_directive_cons_nb,gnd} => gnd * {dht_directive_cons_nb,gnd} # 
    "Check presence of exact term in DHT. The predicate does not block if no 
     match is found.".

:- pred operate_with_client(Stream, DHTDirectiveExtractB):
    gnd * dht_directive_ext_b =>  gnd * {dht_directive_ext_b,gnd} # 
    "Extract any term matching given pattern (pattern is supplied via 
     @var{Value}) from DHT. The predicate blocks if none is found at the 
     moment.".

:- pred operate_with_client(Stream, DHTDirectiveExtractB): 
    gnd * {dht_directive_ext_b,gnd}  => gnd * {dht_directive_ext_b,gnd} # 
    "Extract exact term from DHT. The predicate blocks if none is found at the
     moment.".

:- pred operate_with_client(Stream, DHTDirectiveExtractNB):
    gnd * dht_directive_ext_nb =>  gnd * {dht_directive_ext_nb,gnd} # 
    "Extract any term matching given pattern (pattern is supplied via 
     @var{Value}) from DHT. ".

:- pred operate_with_client(Stream, DHTDirectiveExtractNB):
    gnd * {dht_directive_ext_nb,gnd}  => gnd * {dht_directive_ext_nb,gnd} # 
    "Extract exact term from DHT. The predicate does not block if no match
     is found.".

:- pred operate_with_client(Stream, DHTDirectiveStore):
    gnd * {dht_directive_store,gnd} => gnd * {dht_directive_store,gnd} # 
    "Store ground term to DHT. The predicate does not block if no match
     is found.".

:- pred operate_with_client(Stream, DHTDirectiveHash): 
    gnd * dht_directive_hash => gnd * {dht_directive_hash,gnd} # 
    "Get value of hash function for a given term. Result is written to 
     @var{Stream}. @var{DHTDirectiveHash} is a simple predicate of form 
     dht_hash(Term), where @var{Key} is always ground".

:- pred operate_with_client(Stream, DHTDirectiveHash):
    gnd * {dht_directive_hash,gnd} => gnd * {dht_directive_hash,gnd} # 
    "Check value of hash function for a given term.@var{DHTDirectiveHash} is 
     a simple predicate of form dht_hash(Term), where @var{Key} is always 
     ground".

operate_with_client(Stream, Term):-
        ground(Stream),
        ground(Term),
        Term = end_of_file,
        !.
operate_with_client(Stream, dht_consult_b(Key, Value)) :-
        ground(Stream),
        ground(Key),
        any_to_atom(Key, AtomKey),
        dht_logic:dht_find_and_consult_b(AtomKey, Value),
        write_pr(Stream, Value),
        !.
operate_with_client(Stream, dht_consult_nb(Key, Value)) :-
        ground(Stream),
        ground(Key),
        any_to_atom(Key, AtomKey),
        dht_logic:dht_find_and_consult_nb(AtomKey, Value),
        write_pr(Stream, Value),
        !.
operate_with_client(Stream, dht_extract_b(Key, Value)) :-
        ground(Stream),
        ground(Key),
        any_to_atom(Key, AtomKey),
        dht_logic:dht_find_and_extract_b(AtomKey, Value),
        write_pr(Stream, Value),
        !.
operate_with_client(Stream, dht_extract_nb(Key, Value)) :-
        ground(Stream),
        ground(Key),
        any_to_atom(Key, AtomKey),
        dht_logic:dht_find_and_extract_nb(AtomKey, Value),
        write_pr(Stream, Value),
        !.
operate_with_client(Stream, dht_store(Key, Value)) :-
        ground(Stream),
        ground(Key),
        %% ground Value?
        any_to_atom(Key, AtomKey),
        display('operate_with_client: dht_store'),nl,
        dht_logic:dht_find_and_store(AtomKey, Value),
        write_pr(Stream, dht_ok),
        !.
operate_with_client(Stream, dht_hash(Term)) :-
        ground(Stream),
        ground(Term),
        dht_logic_misc:consistent_hash(Term, TermHash),
        write_pr(Stream, TermHash), 
        nl(Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(num_of_connections/1,
           "Defined amount of possibly simultaneous connection on the socket.").

:- pred num_of_connections(QueueSize): var => int #
    "Get the size of waiting queue for the socket.".

:- pred num_of_connections(QueueSize): int => int #
    "Check whether number supplied corresponds to size of socket waiting 
     queue.".

num_of_connections(100).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_socket(Socket):-
        num_of_connections(Queue),
        dht_config:dht_s2c_port(Port),
        bind_socket(Port, Queue, Socket),  %% Already in "listen" state
        !. 


:- doc(get_socket/1,
           "Associates a free variable with newly opened listening socket.
            Quite standard operations for binding to socket.
            @begin{enumerate}
            @item{} get queue length.
            @item{} get socket port.
            @item{} bind to socket.
            @end{enumerate}").

:- pred get_socket(Socket): var => gnd #
    "Get listen-state socket for client-to-server communication.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(any_to_atom/2 ,
           "This predicate should convert virtually anything to an atom 
            value.").

:- pred any_to_atom(Key, Value): atm * var => atm * atm #
    "No conversion needed if @var{Key} is an atom already.".

:- pred any_to_atom(Key, Value): num * var => num * atm #
    "Convert number to atom.".

:- pred any_to_atom(Key, Value): string * var => string * atm #
    "Convert string to atom.".

any_to_atom(Key, Key):-
        atm(Key),
        !.
any_to_atom(Key, Atom):-
        num(Key),
        atom_number(Atom, Key),
        !.
any_to_atom(Key, Atom):-
        string(Key),
        atom_codes(Atom, Key),
        !.
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
