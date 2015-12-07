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
:- module(dht_s2s, 
         [
          dht_s2s_main/0
         ], 
         [
          assertions,regtypes,isomodes
         ]).

:- doc(title, "Server to server communication module").
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

:- doc(module, "This module describes server-2-server
           side behavior of any node. Since this includes 
           various communication and inspection tasks that 
           should be performed in parallel, this module 
           makes extensive usage of Ciao threading mechanism.
           On the other hand, module must be as simple as 
           possible in terms of usage. Therefore, only a single
           predicate @pred{dht_s2s_main/0} is exposed to outer 
           world and rest of complexity is hidden inside. This
           predicate is used from @file{dht_server.pl} module.
           All the parameters are passed through 
           @file{dht_config.pl} module.").

:- use_module(library(sockets)).
:- use_module(library(system), [pause/1]).
:- use_module(library(concurrency)).

%:- use_module(library(odd)).

:- use_module(dht_config).
:- use_module(dht_misc).
:- use_module(dht_logic).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Concurrent predicates
:- concurrent s2s_conn/1.

dht_s2s_main:-
        dht_config:dht_server_id(Self),
        dht_config:dht_s2s_threads(Threads),
        dht_logic:dht_init(Self),
        (
            dht_config:dht_join_host(JoinHost) ->
            dht_logic:dht_join(JoinHost)
            ;
            true
        ),
        create_threads(Threads),
        get_socket(Socket),
        wait_for_connections(Socket).

:- doc(dht_s2s_main/0,
           "Generally speaking, this predicate must perform some conventional
            tasks like: @begin{itemize}
               @item set local part of DHT to initial state, 
               @item contact a successor node if there is any, 
               @item and finally launch several threads that listen to streams, 
                     process incoming requests and inspect state of local 
                     finger-table.
            @end{itemize} 
            All parameters (like listen port, number of threads, host to join, 
            etc) are received from @file{dht_config.pl} module").


%:- pred dht_s2s_main([]): list #
%
%    "In case an empty list is used to initialize the server-2-server side of the node,
%     its behavior is driven by default values. Server identifier is set to
%     $0$ and the number of stream-listening threads is set to $20$. Two more threads
%     are created:@begin{itemize} 
%      @item{} one is dedicated to execution of @pred{dht_stabilize/0} predicate;
%      @item{} the other to execution of @pred{dht_fix_fingers/0}.
%     @end{itemize} Current node
%     is treated as the only node in DHT so no @pred{dht_join/2} is performed.".


%:- pred dht_s2s_main([ServerId]): list(int) #
%
%    "If a one-element list is used for initialization, this single element is
%     interpreted as the identifier of current node. Number of additional threads
%     is set to 20.Two more threads are created. One is dedicated to execution 
%     of @pred{dht_stabilize/0} predicate
%     and the other to execution of @pred{dht_fix_fingers/0}. Current node
%     is treated as the only node in DHT so no @pred{dht_join/2} is performed.".

%:- pred dht_s2s_main([ServerId, NodeId]): list #
%
%    "A two-element list is treated as information on identification number 
%     and on existing DHT node, that current node must join to. Number of 
%     additional threads is set to 20.Two more threads are created. One is 
%     dedicated to execution of @pred{dht_stabilize/0} predicate
%     and the other to execution of @pred{dht_fix_fingers/0}.".

%:- pred dht_s2s_main([ServerId, NodeId]): list #
%
%    "If previous case fails (for instance second element is of different
%     structure than @pred{node_id/2}), then two-element list is treated 
%     as information on identification number and on number of additional
%     threads. Actually number of threads is always a bit bigger since two 
%     more threads are created. One of them dedicated to execution of 
%     @pred{dht_stabilize/0} predicate and the other to execution of 
%     @pred{dht_fix_fingers/0}. Current node is treated as the only node 
%     in DHT so no @pred{dht_join/2} is performed.".

%:- pred dht_s2s_main([ServerId, NodeId]): list #
%
%    "Finally, if a three-element list is supplied first element is 
%     treated as information on existing DHT node, that current node
%     must join to, second as identifier of current node, and third 
%     one ass number of additional threads. As in previous cases,
%     actual number of threads is always a bit bigger since two 
%     more threads are created. One of them dedicated to execution of 
%     @pred{dht_stabilize/0} predicate and the other to execution of 
%     @pred{dht_fix_fingers/0}.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wait_for_connections(Socket):-
        repeat,
           socket_accept(Socket, Stream),
	   assertz_fact(s2s_conn(Stream)),
        fail.

:- doc(wait_for_connections/1, 
           "Predicate consists of eternal loop composed out of 'repeat' and 
            'fail' predicates, a socket reading predicate and a storage call.
            ").

:- pred wait_for_connections(Socket): int #
    "Get incoming connection and store them into local database.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_threads(0):-
        eng_call(handle_stabilize, create, create),
        eng_call(handle_fingers, create, create),
        !.
create_threads(N):-
        N > 0,
        eng_call(handle_connection, create, create),
        N1 is N - 1,
        create_threads(N1).

:- doc(create_threads/1,
           "perform call to @pred{eng_call/3} several times. Number of times
            is defined by value of single argument @var{N}.").

:- pred create_threads(N): int # 
    "Create auxiliary threads. @var{N} threads for handling connection. One 
     thread for running stabilization procedure on successor and one for 
     running stabilization on rest of the finger table.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(handle_stabilize/0, 
           "Second eternal loop. Executes @pred{dht_stabilize/0} predicate from
            @file{dht_logic.pl} with some pauses in between calls.").

:- pred handle_stabilize # 
    "Eternally check for correctness of successor link.".

handle_stabilize:-
        repeat,
            pause(5),
            display('Stabilizing...'),nl,
            dht_stabilize,
        fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(handle_fingers/0, 
           "Third eternal loop. Executes @pred{dht_fix_fingers/0} predicate from
            @file{dht_logic.pl} with some pauses as well.").

:- pred handle_fingers #
    "Eternally check correctness of other links in finger table.".

handle_fingers:-
        repeat,
            %undo(display('handle_fingers: undo: before repeat.')),
            pause(1),
            %undo(display('handle_fingers: undo: before pause.')),
            dht_fix_fingers,
            %undo(display('handle_fingers: undo: before dht_fix_fingers.')),
        fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(handle_connection/0, 
           "Performs following instructions:@begin{enumerate}
            @item{} read a dht_rpr_in form Stream,
            @item{}call argument to dht_rpr_in,
            @item{}write result as dht_rpr_out to Stream,
            @item{}close the Stream.@end{enumerate} ").

:- pred handle_connection# 
    "Eternally extract new requests from database ans process them via calling
     @pred{dht_pr_call/2}.".

handle_connection:-
        repeat,
           retract_fact(s2s_conn(Stream)),
           read_pr(Stream, Rpr),
           dht_pr_call(Stream, Rpr),
           close(Stream),
        fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(dht_pr_call/2, 
           "This predicate is called in @pred{handle_connection/0}.
            The only reason to have dedicated predicate here is to avoid 
            possible 'bad' inputs. And issue an error predicate if one 
            happens.").

:- pred dht_pr_call(Stream, Pred): gnd * gnd #
    "Check whether predicate supplied by @var{Pred} is satisfiable.".

:- pred dht_pr_call(Stream, Pred): gnd * var => gnd * gnd #
    "Check whether predicate supplied by @var{Pred} is satisfiable and
     bind those variables that are still free to the ones found in DHT.".

dht_pr_call(Stream, dht_rpr_in(Pr)):-
        call(Pr) ->
        write_pr(Stream, dht_rpr_out(Pr))
        ;
        write_pr(Stream, dht_rpr_out(dht_rpr_error)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

num_of_connections(100).

:- doc(num_of_connection/1,
           "Defined amount of possibly simultaneous connection on the socket.").

:- pred num_of_connections(QueueSize): var => int #
    "Get the size of waiting queue for the socket.".

:- pred num_of_connections(QueueSize): int => int #
    "Check whether number supplied corresponds to size of socket waiting 
     queue.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_socket(Socket):-
        num_of_connections(Queue),
        dht_config:dht_s2s_port(Port),
        bind_socket(Port, Queue, Socket),% Already in "listen" state
        !.

:- doc(get_socket/1,
           "Associates a free variable with newly opened listening socket.").

:- pred get_socket(Socket): var => gnd #
    "Get listen-state socket for server-to-server communication.".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
