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
% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

:- module(dht_logic,
         [
          dht_init/1,
          dht_finger/2,
          dht_successor/1,
          dht_check_predecessor/1,
          dht_closest_preceding_finger/2,
          dht_find_predecessor/2,
          dht_find_successor/2,
          dht_join/1,
          dht_notify/1,
          dht_stabilize/0,
          dht_fix_fingers/0,
          dht_id_by_node/2,

          dht_find_and_consult_b/2,
          dht_consult_server_b/3,

          dht_find_and_consult_nb/2,
          dht_consult_server_nb/3,

          dht_find_and_extract_b/2,
          dht_extract_from_server_b/3,

          dht_find_and_extract_nb/2,
          dht_extract_from_server_nb/3,

          dht_find_and_store/2,
          dht_store_to_server/4
         ],[
          assertions,regtypes,isomodes,fsyntax
         ]).

:- doc(title, "DHT-related logics").
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

:- doc(module, "This module implements a core DHT functionality,
   like finger-table manipulation and lookup search. Keep in mind that
   remote calls (known as remote predicate calls in Prolog) are
   extracted to a separate module as well as low-lever database
   handling.  In turn, the logic module is utilized by higher-lever
   modules, like the server-2-server and server-2-client communication
   modules.

   @var{Id} is treated as the value of the hash function used all over
   the DHT.").

%:- use_module(library(odd)).

:- use_module(dht_rpr).
:- use_module(dht_config).
:- use_module(dht_logic_misc).
:- use_module(dht_storage).
:- use_module(dht_routing).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:- dynamic node_id/2.
%
%dht_term_hash(Term, DHTHash):-
%        ground(Term),
%        hash_term(Term, TermHash),
%        hash_size(HashSize),
%        DHTHash is mod(integer(TermHash), HashSize).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
:- doc(dht_highest_hash_number/1, 
           "Wrapper around @pred{highest_hash_number/1} from 
            @file{dht_logic_misc.pl} See manual on that module for more 
            comprehensive documentation.").

:- pred dht_highest_hash_number(Number): int => int #
    "Check whether @var{Number} is equal to highest possible number in
     DHT ring.".

:- pred dht_highest_hash_number(Number): var => int #
    "Associate value of @var{Number} with biggest identifier.".

dht_highest_hash_number(Number):-
       highest_hash_number(Number).
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_init(Self):-
      dht_config:dht_server_id(Self),
      dht_config:dht_server_host(OwnIP),
      dht_rpr:dht_rpr_register_node(Self, OwnIP),
      dht_routing:dht_reset_predecessor,
      init_finger_table(Self).

:- doc(dht_init/1, 
           "Predicated performs the initialization of a single-node DHT.").

:- pred dht_init(OwnId): int  # 

    "The predicate is intended to perform various initialization
     functions like finger_table creation, hostname and
     IP address retrieval. The value supplied in @var{OwnId} 
     is treated as value of the hash function used all over
     the DHT.".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_finger(K, Node):-           
      hash_power(Power),
      K =< Power,
      K > 0,
      dht_routing:dht_finger_table(K, Node).

:- doc(dht_finger/2, 
           "Get the node identifier (composition of identifier and IP-address) 
            by using index in the finger table. Prolog synonym for 
            @tt{finger[k].node}.").

:- pred dht_finger(Idx, Finger): int * var => int * gnd #

    "@pred{dht_finger/2} implements the array looking up necessary to 
     retrive information from the finger table, where first argument 
     @var{Idx} if acting as array index and @var{Finger} as 
     corresponding array value.".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_successor(Successor):-
      dht_finger(1, Successor).

:- doc(dht_successor/1, 
           "Wrapper around @pred{dht_finger/1}, where first argument is 
            defaulted to '1'").

:- pred dht_successor(Successor): var => gnd #

    "@pred{dht_successor/1} a simple wrapper around 
     @pred{dht_finger/2}, that looks up the first 
     row of so-called finger-table".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_id_by_node(Node, NodeID):-
        dht_rpr_id_by_node(Node, NodeID).

:- doc(dht_id_by_node/2,
           "This predicate is entirely used by remote calls. Since there
            is @pred{dht_rpr_id_by_node/2} for current module. This is an effort
            to avoid module-re-exportation (@file{dht_routing.pl} and 
            @file{dht_rpr.pl}). ").

:- pred dht_id_by_node(Node, NodeID): int * var => int * gnd #
    "Get node identity by node number.".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_closest_preceding_finger(Id, NodeID):-
        ground(Id), dht_config:dht_server_id(Self),
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %% For future - why do we use 
        %% ?????????????????????????????????????
        %%   dht_finger_table(_, NodeId)
        %% In fact this is a prolog style of making 
        %% for-each loops. The effect is achieved
        %% with backtracking mechanism.
        dht_routing:dht_finger_table(_, Node),
        in_circle_oo(Node, Self, Id),
        dht_rpr_id_by_node(Node, NodeID),
        !.
dht_closest_preceding_finger(_Id, OwnID):-
	dht_config:dht_server_id(Self),
	dht_rpr_id_by_node(Self, OwnID).

:- doc(dht_closest_preceding_finger/2, 
           "Perform search over current finger table, in order to find entry
            pointing to node that is more closely located to value supplied 
            via @var{Id}. Here is original part of code from Chord paper.
            @begin{verbatim}
  n.closest_preceding_finger(id)@{
      for i = m downto 1 @{
         if (finger[i].node in (n, id))@{
            return finger[i].node;
         @}
      @}
      return n;
  @}
            @end{verbatim}.").

:- pred dht_closest_preceding_finger(Id, Finger): int * var => 
                                                  int * gnd #
    "@pred{dht_closest_preceding_finger/2} is searching local finger-table
     for the entry which points to a node, that is 'closer'
     (in terms of DHT distance) to the identifier specified at @var{Id}.
     It should hold that identifier of the node found is greater
     than identifier of current node and lesser than the identifier
     supplied in @var{Id}. No estimations concerning other nodes in
     between them are made. Furthermore, a cyclic structure of identifiers 
     is preserved: identifier '0' is lesser then identifier '1' but greater 
     then the highest identifier in DHT.".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_find_successor(Id, SuccID):-
        %undo((display('dht_find_successor: undo '),nl)),
        dht_config:dht_server_id(Self),
        %undo((display('dht_find_successor: undo: server_id'),nl)),
        dht_find_predecessor(Id, PredID),
        %undo((display('dht_find_successor: undo: find_predecessor'),nl)),
        Pred = ~dht_rpr_node_by_id(PredID),
        %undo((display('dht_find_successor: undo: node_by_id'),nl)),
        Self=Pred,
        %undo((display('dht_find_successor: undo: Self=Pred'),nl)),
        dht_successor(Successor),
        %undo((display('dht_find_successor: undo: dht_successor'),nl)),
        dht_rpr_id_by_node(Successor, SuccID),
        %undo((display('dht_find_successor: undo: dht_rpr_id_by_node'),nl)),
        !.
dht_find_successor(Id, SuccID):-      
        %undo((display('dht_find_successor: undo '),nl)),
        dht_config:dht_server_id(Self),
        %undo((display('dht_find_successor: undo: server_id'),nl)),
        dht_find_predecessor(Id, PredID),
        %undo((display('dht_find_successor: undo: find_predecessor'),nl)),
        Pred = ~dht_rpr_node_by_id(PredID),
        %undo((display('dht_find_successor: undo: node_by_id'),nl)),
        Self\=Pred,
        %undo((display('dht_find_successor: undo: Self\=Pred'),nl)),
        remote_node_call(PredID, dht_successor(Succ)),
        %undo((display('dht_find_successor: undo: remote_node_call: successor'),nl)),
        remote_node_call(PredID, dht_id_by_node(Succ, SuccID)).

%dht_find_successor(Id, SuccID):-
%        undo((display('dht_find_successor: undo '),nl)),
%        dht_config:dht_server_id(Self),
%        undo((display('dht_find_successor: undo: server_id'),nl)),
%        dht_find_predecessor(Id, PredID),
%        undo((display('dht_find_successor: undo: find_predecessor'),nl)),
%        Pred = ~dht_rpr_node_by_id(PredID),
%        undo((display('dht_find_successor: undo: node_by_id'),nl)),
%        (
%            Self=Pred ->
%            undo((display('dht_find_successor: undo: Self=Pred'),nl)),
%            dht_successor(Successor),
%            undo((display('dht_find_successor: undo: dht_successor'),nl)),
%            dht_rpr_id_by_node(Successor, SuccID),
%            undo((display('dht_find_successor: undo: dht_rpr_id_by_node'),nl))
%            ;
%            undo((display('dht_find_successor: undo: Self\=Pred'),nl)),
%            remote_node_call(PredID, dht_successor(Succ)),
%            undo((display('dht_find_successor: undo: remote_node_call: successor'),nl)),
%            remote_node_call(PredID, dht_id_by_node(Succ, SuccID))
%        ).

%dht_find_successor(Id, SuccID):-
%	display('dht_find_successor'),nl,
%	dht_find_predecessor(Id, PredID),
%       display('dht_find_successor: '),display(node_id(PredId)),nl,
%.

%DO WE REALLY NEED TO REGISTER EVERYTHING THAT GOES THROUGH THIS PREDICATE?
%	dht_rpr:dht_rpr_register_node(SuccID).

:- doc(dht_find_successor/1, 
           "Perform search over DHT structure. Resulting node is expected to 
            be responsible for identifier supplied via @var{Id}. Here is 
            corresponding part of code from Chord paper.
            @begin{verbatim}
 n.find_successor(id)@{
     n' = find_predecessor(id);
     return n'.successor;
 @}
            @end{verbatim}").

:- pred dht_find_successor(Id, Successor): int * var => int * gnd #

    "@pred{dht_find_successor/2} has behavior similar to
     @pred{dht_find_predecessor/2} except that it 
     gives a node that directly succeeds the identifier specifies.
     In other words, the identifier of the node found is greater than the
     identifier specified in @var{Id} and there are no other nodes
     between the node found and the node specified in @var{Id}".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_find_predecessor(Id, OwnID):-
%        display('dht_find_predecessor single node case'),nl,
        ground(Id),
        dht_config:dht_server_id(Self),
        dht_successor(Self),
        OwnID = ~dht_rpr_id_by_node(Self),
        !.
dht_find_predecessor(Id, OwnID):-
%        display('dht_find_predecessor self case'),nl,
	ground(Id),
	dht_config:dht_server_id(Self),
	dht_successor(Successor),
	in_circle_oc(Id, Self, Successor),
	dht_id_by_node(Self, OwnID),
	!.
dht_find_predecessor(Id, NodeID):-
%        display('dht_find_predecessor others case'),nl,
	ground(Id),
	dht_closest_preceding_finger(Id, PrecedingNodeID),
	iterate_preceding_fingers(Id, PrecedingNodeID, NodeID).

:- doc(dht_find_predecessor/2, 
           "Perform search over existing DHT structure. The resulting node is
            expected to precede the one identified by @var{Id}. Again, 
            a small quote from Chord-paper source code.
            @begin{verbatim}
  n.find_predecessor(id)@{
      n' is n,
      while(id not_in (n', n'.successor] )@{
         n' = n'.closest_preceding_finger(id);
      @}
      return n';
  @}
            @end{verbatim}").

:- pred dht_find_predecessor(Id, Predecessor): int * var => 
                                               int * gnd #

    "@pred{dht_find_predecessor/2} searches all over DHT for a
     node, which is 'the closest' (in terms of DHT distance) to the 
     index specified as @var{Id}. In other words, the searched node
     must have the index lesser than the identifier specified in @var{Id} 
     and there must not be any other nodes between 
     identifier specified in @var{Id} and the node found.".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_join(JoinHost):-
        ground(JoinHost),
        (
            dht_config:dht_server_host(JoinHost) ->
            true
            ;
            dht_config:dht_server_id(Self),
            display('About to call remotely'),nl,
            remote_node_call(JoinHost, dht_find_successor(Self, SuccID)),
            display('Found successor: '),display(SuccID),nl,
	    add_successor(SuccID)
        ).
%dht_join(Next, NextIP):-
        %this one was commented out,
        %since there is exactly the same
        %statement in dht_init, which
        %is more proper place for
        %a statement like this
        %asserta_fact(dht_predecessor(nil)),

:- doc(dht_join/1, 
           "It performs the join procedure on the node pointed by the argument. 
            Here is the corresponding quote:
            @begin{verbatim}
 n.join(Next)@{
     predecessor = nil;
     successor = Next.find_successor(n);
 @}
            @end{verbatim}
").

:- pred dht_join(NodeIP): atm #

    "@pred{dht_join/2} is executed every time some node decides to 
     join some DHT. The only information needed for execution of
     join is a valid ID / IP combination of any existing node,
     which must be supplied with @var{NodeId}".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_check_predecessor(nil):-
        dht_predecessor(nil),
        !.
dht_check_predecessor(PredecessorId):-
        dht_predecessor(PredId),
        PredId \= nil,
        catch(dht_rpr:dht_rpr_call(PredId,
                                   dht_successor(_X)),
              _Error,
              reset_predecessor),
        dht_predecessor(PredecessorId).

:- doc(dht_check_predecessor/1, 
           "@pred{dht_check_predecessor/1} predicate is called by external 
            nodes when they run @pred{dht_stabilize/0}. Since there is no other
            way to learn about a predecessor's failure, but checking explicitly 
            once predecessor is required. that is why behavior of 
            @pred{dht_check_predecessor} is following:@begin{enumerate}
            @item{} give 'nil' as predecessor if it is stored in database that 
                    way, which happens if node is unaware of any other nodes on 
                    the ring,
            @item{} otherwise, get current value of predecessor,
            @item{} try calling predecessor with any arbitrary predicate in 
                    our case @pred{dht_successor/1},
            @item{} if an exception is issued by remote call - reset 
                    predecessor to 'nil',
            @item{} get value of predecessor once again and bind this value to 
                    the answer. We also check that in case of finger-nodes 
                    failure, but this would work out only in poorly 
            populated Chord-ring
            @end{enumerate}").

:- pred dht_check_predecessor(PredecessorId): var => gnd # 
    "Get value of predecessor for current node.".

:- pred dht_check_predecessor(PredecessorId): gnd => gnd # 
    "Check whether value supplied is equal to id of predecessor.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reset_predecessor:-
        dht_reset_predecessor.

:- doc(reset_predecessor/0,
           "@pred{reset_predecessor/0} is used to set dynamic predicate 
            @pred{dht_predecessor/1} to 'nil' value. All previous values are 
            extracted form local DB. Currently there are two predicates that 
            call @pred{reset_predecessor/0}, namely 
            @pred{dht_check_predecessor/1} and @pred{verify_predecessor/1}.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(dht_stabilize, 
           "Perform stabilization on the successor node by asking successor's 
            predecessor value. If the value returned is equal to current node, 
            just stay calm, otherwise try to adopt the newly returned result 
            as new successor. Most of the 'dirty' work is performed by 
            @pred{stabilize_successor/2} predicate. The corresponding 
            Chord-paper quote is given below:
            @begin{verbatim}
 n.stabilize()@{
     x = successor.predecessor;
     if (x in (n, successor))@{
         successor = x;
     @}
     successor.notify(n);
 @}
            @end{verbatim}  There is also a small simplification scheme, 
            since before calling @pred{dht_join/1} each node assumes itself as the 
            only member of the circle, the very first case of 
            @pred{dht_stabilize/0} does nothing if successor is actually 
            equal to current node.").

:- pred dht_stabilize #
    "@pred{dht_stabilize/0} is a eternally repeated predicate,
     which aims at stabilizing first-level references of the finger-table,
     while multiple concurrent joins and failures can happen. Its main goal
     is to ask its own successor to report its predecessor. If the reported 
     node is different from the one that is calling @pred{dht_stabilize/0},
     there should be someone in between current node and its the successor.
     So a newly reported node should be registered as the successor 
     instead of an old one. Technically, once an 
     inconsistency between actual state of DHT and finger-table
     is found a newly found node is asked to perform 
     @pred{dht_notify/1}, with current node as an argument.".

dht_stabilize:-
        display('dht_stabilize: begin'),nl,
        dht_config:dht_server_id(Self),
        display('dht_stabilize: '),display(Self),nl,
        dht_successor(Self),
        display('dht_stabilize single node case'),nl,
        !.
dht_stabilize:-
        dht_config:dht_server_id(Self),
	dht_successor(Successor),
        Self \= Successor,
	remote_node_call(Successor, dht_check_predecessor(X)),
	display('dht_stabilize: '),display(X),nl,
	stabilize_successor(Successor, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_notify(OwnID):-
        display('dht_notify: 1'),nl,
        ground(OwnID),
        Self = ~dht_rpr_node_by_id(OwnID),
        dht_config:dht_server_id(Self),
        !.
dht_notify(NewNodeID):-
        display('dht_notify: 2 '),display(NewNodeID),nl,
	ground(NewNodeID),
	dht_predecessor(nil),
	add_predecessor(NewNodeID),
        display('About to check for successor'),nl,
	check_successor(NewNodeID),
        dht_predecessor(Debug1),dht_successor(Debug2),
        display('Debug. Pred: '),display(Debug1),display(' Succ: '),display(Debug2),nl,
        display('Successor check done.'),nl,
%        dht_config:dht_server_id(Self),
%        next_on_circle(Self, Next),
%        share_data(Next),
	!.
dht_notify(NewNodeID):-
        display('dht_notify: 3'),nl,
	ground(NewNodeID),
        NewNode = ~dht_rpr_node_by_id(NewNodeID),
	dht_predecessor(Pred),
	dht_config:dht_server_id(Self),
	in_circle_oo(NewNode, Pred, Self),
	add_predecessor(NewNodeID),
%        next_on_circle(Pred, Next),
%        share_data(Next),
        !.

:- doc(dht_notify/1, 
           "This predicate performs 'notification'. Notification is part of
            adaptation process launched by @pred{dht_stabilize/0}. A quote from
            Chord paper that refers to this section is given below:
            @begin{verbatim}
 n.notify(NewNodeId)@{
     if (predecessor is nil ||
           NewNodeId in (predecessor, n))@{
         predecessor = NewNodeId;
     @}
 @}
            @end{verbatim}").

:- pred dht_notify(NewNode): gnd #

    "@pred{dht_notify/1} is called on current node,
     once any other DHT node specified by @var{NewNode} 
     regards current node at it's successor. ".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
:- doc(share_data/1,
           "Sharing of data is performed once a new node joins an existing DHT
            circle. Usually there is some data, that is already in the circle,
            which should be, from now on, associated with new node. Therefore,
            all that data is to be moved form old node to a new one. It is 
            provable that only one node (except new one) is involved into
            this transaction.").

:- pred share_data(FromID) : int #
    "@pred{share_data/1} is called on current node, once a
     new predecessor was registered. Since there might be
     only one predecessor for each node at a time, there
     is no need to define more then one argument (@var{FromID}),
     which stands for beginning of sector to be shared.
     Generally speaking there might be two cases of 
     @pred{share_data/1} usage:
     @begin{enumerate}
     @item{} current node believes it is single node on the circle
     @item{} current node believes new predecessor is more close
     then the previous one.
     @end{enumerate}In both cases the sector of ID-s to share
     is estimated by receiving next ID on circle via @pred{next_on_circle}.
     A step-by-step explanation of predicate behavior follows:
     @begin{enumerate}
     @item{} check argument for groundness,
     @item{} if argument is equal to ID of current predecessor, 
     execute @pred{share_data_with_hash_id/1} and exit.
     @item{} if argument is less then ID of current predecessor:
        @begin{enumerate}
        @item{} execute @pred{share_data_with_hash_id/1},
        @item{} get next hash id on circle via @pred{next_on_circle/2},
        @item{} call itself recursively with value of next has id.
        @end{enumerate}
     @end{enumerate}".

share_data(PredID) :- 
        ground(PredID),int(PredID),
        dht_predecessor(PredID),
        share_data_with_hash_id(PredID),
        !.
share_data(HashID):-
        ground(HashID),int(HashID),
        dht_predecessor(PredID),
        HashID < PredID,
        share_data_with_hash_id(HashID),
        next_on_circle(HashID,NextID),
        share_data(NextID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(share_data_with_hash_id/1, 
           "This predicate is mostly author's twist to imitate loop behavior.
            The core function of it is retraction of key-value pairs that were
            mapped into certain hash value, and transmitting them to 
            predecessor.").

:- pred share_data_with_hash_id(HashId): int #
     "@pred{share_data_with_hash_id/1} is called entirely from
      @pred{share_data}, and performs ONLY mechanical actions on
      retraction of predicates and transportation them to predecessor.
      WARNING: predicate makes use of backtracking to emulate a
      common loop behavior. This is what @pred{fail/0} stands for
      at the end of first case.
     ".

share_data_with_hash_id(Hash):-
        integer(Hash),
        dht_predecessor(Pred),
        dht_config:dht_server_id(Self),
        Pred \= Self,
        dht_storage:dht_key_hash(Key, Hash),
        %%????
        dht_storage:dht_extract_nb(Key, Value),
        ServerID = ~dht_rpr_id_by_node(Pred),
        remote_node_call(ServerID, dht_store(Key, Hash, Value)),
        fail.
share_data_with_hash_id(_).
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(dht_fix_fingers, 
           "This predicate takes care of maintaining the finger table entries
            (except the first one) up to date. The first entry is taken care of
            by a specialized predicate @pred{dht_stabilize/0}, since there is 
            much more responsibility on first entry (a direct successor). Rest
            of the same task is performed here, by @pred{dht_fix_fingers/0}. 
            In the Chord paper notation this part look like:
            @begin{verbatim}
 n.fix_fingers()@{
     i = random index > 1 into finger[];
     finger[i].node = find_successor(finger[i].start);
 @}  
            @end{verbatim}.
").

:- pred dht_fix_fingers #
    "@pred{dht_fix_fingers/0} is yet another randomly repeated predicate,
     which aims at stabilizing high-level references of finger-table, 
     while multiple concurrent joins and failures happen. Once started,
     @pred{dht_fix_fingers/0} picks a random (but not first) entry 
     of finger table, and searches for a node responsible for the identifiers
     specified in the entry. In case, node that was found is different 
     from the one currently specified, current node is replaced by a new one.".

dht_fix_fingers:-
        %undo((display('dht_fix_fingers: undo: Fixing fingers...'),nl)),
        get_random_finger_index(Idx),
        %undo((display('dht_fix_fingers: undo: About to fix #'),display(Idx),nl)),
        dht_finger_start(Idx, NextId),
        %undo((display('dht_fix_fingers: undo: Corresponging section start:'),display(NextId),nl)),
        dht_find_successor(NextId, SuccessorID),
        %undo((display('dht_fix_fingers: undo: Found successor: '),display(SuccessorID),nl)),
        update_finger_node(Idx, SuccessorID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_server(Key, KeyHash, ServerID):-
        ground(Key),
        consistent_hash(Key, KeyHash),
        dht_find_successor(KeyHash, ServerID),
        display('Found server: '),display(ServerID),nl.

:- doc(find_server/3, 
           "@pred{find_server/3} is a first component of dht_find_and_* family 
            of predicates.").

:- pred find_server(Key, KeyHash, ServerId): gnd * var * var => 
                                             gnd * int * int #

    "if @pred{find_server/3} is used this way it associates value of second 
     argument @var{KeyHash} with value of hash-function applied to @var{Key}
     and third argument @var{ServerId} with ID of DHT-server responsible for
     holding predicated mapped into @var{KeyHash}.". 

:- pred find_server(Key, KeyHash, ServerId): gnd * int * var => 
                                             gnd * int * int #
    "if used with first argument @var{Key} and second argument @var{KeyHash} 
     ground predicate simply checks correspondence between these arguments,
     and if first is really mapped to second, then performs search on 
     actual data of DHT for a @var{ServerId}, that is responsible for holding
     @var{Key}.".

:- pred find_server(Key, KeyHash, ServerId): gnd * int * int => 
                                             gnd * int * int #
    "Third case leads to straight-forward check on actual DHT-data. In other
     words, @var{Key} must be mapped into @var{KeyHash} and @var{ServerId}
     must indicate ID of server responsible for storing @var{Key}.".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_find_and_consult_b(Key, Value):-
        find_server(Key, _KeyHash, ServerId),
        dht_consult_server_b(ServerId, Key, Value).

:- doc(dht_find_and_consult_b/2, 
           "@pred{dht_find_and_consult_b/2} is meant to perform two actions:
            @begin{itemize}
             @item search for the node responsible for value supplied as 
                   a @var{Key};
             @item perform @pred{dht_consult_server_b/3} on a node found.
            @end{itemize}
            This is the first predicate of the whole @pred{dht_find_and_*/2}
            family of predicates. Name of each member of the family gives
            a hint on implementation. And as it is presented by the name of
            the family, they share some part of implementation - all the
            members of the family perform search for particular DHT-node, 
            which is achieved by @pred{find_server/3} predicate. ").

:- pred dht_find_and_consult_b(Key, Value): int * var => int * gnd # 

    "An invocation of this type would bind all free variables in @var{Value} to 
     the values present in the local database of the node responsible for 
     @var{Key}.".

:- pred dht_find_and_consult_b(Key, Value): int * gnd #

    "Invocation with two ground arguments is equal to checking the presence of
     the term specified by @var{Value} inside the DHT. ".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_consult_server_b(ServerID, Key, Value):-
        ground(ServerID),ground(Key),
        Node = ~dht_rpr_node_by_id(ServerID),
        dht_config:dht_server_id(Self),
        ( 
            Self = Node ->
            dht_storage:dht_consult_b(Key, Value)
            ;
            remote_node_call(ServerID, dht_consult_server_b(ServerID, 
                                                            Key, 
                                                            Value))
        ).

:- doc(dht_consult_server_b/3, 
           "Basically forms the second half of @pred{dht_find_and_consult/2} 
            predicate. This predicate takes no care about search of 
            corresponding server. The only thing it does - given a server try to
            locate a predicate under given @var{Key} there. Of course there two 
            possible variants of application:").

:- pred dht_consult_server_b(NodeId, Key, Value): 
        gnd * gnd * var => gnd * gnd * gnd # 
        "First variant deals with real consulting, when the value is unknown.
         Therefore, after predicate is successfully executed, a real value, that
         is stored under @var{Key} is associated with third argument 
         (@var{Value})".

:- pred dht_consult_server_b(NodeId, Key, Value): 
        gnd * gnd * gnd => gnd * gnd * gnd # 
        "Second variant is more similar to check for existence, All arguments
         are ground by the time predicate is called, so the only use is 
         success/failure of predicate itself.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_find_and_consult_nb(Key, Value):-
        find_server(Key, _KeyHash, ServerId),
        dht_consult_server_nb(ServerId, Key, Value).

:- doc(dht_find_and_consult_nb/2, 
           "@pred{dht_find_and_consult_nb/2} is meant to perform two actions:
            @begin{itemize}
             @item search for the node responsible for value supplied as 
                   a @var{Key};
             @item perform @pred{dht_consult_server_nb/3} on a node found.
            @end{itemize}
            This is the first predicate of the whole @pred{dht_find_and_*/2}
            family of predicates. Name of each member of the family gives
            a hint on implementation. And as it is presented by the name of
            the family, they share some part of implementation - all the
            members of the family perform search for particular DHT-node, 
            which is achieved by @pred{find_server/3} predicate. ").

:- pred dht_find_and_consult_nb(Key, Value): int * var => int * gnd # 

    "An invocation of this type would bind all free variables in @var{Value} to 
     the values present in the local database of the node responsible for 
     @var{Key}.".

:- pred dht_find_and_consult_nb(Key, Value): int * gnd #

    "Invocation with two ground arguments is equal to checking the presence of
     the term specified by @var{Value} inside the DHT. ".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_consult_server_nb(ServerID, Key, Value):-
        ground(ServerID),ground(Key),
        Node = ~dht_rpr_node_by_id(ServerID),
        dht_config:dht_server_id(Self),
        ( 
            Self = Node ->
            dht_storage:dht_consult_nb(Key, Value)
            ;
            remote_node_call(ServerID, dht_consult_server_nb(ServerID, 
                                                             Key, 
                                                             Value))
        ).

:- doc(dht_consult_server_nb/3, 
           "Basically forms the second half of @pred{dht_find_and_consult/2} 
            predicate. This predicate takes no care about search of 
            corresponding server. The only thing it does - given a server try to
            locate a predicate under given @var{Key} there. Of course there two 
            possible variants of application:").

:- pred dht_consult_server_nb(NodeId, Key, Value): 
        gnd * gnd * var => gnd * gnd * gnd # 
        "First variant deals with real consulting, when the value is unknown.
         Therefore, after predicate is successfully executed, a real value, that
         is stored under @var{Key} is associated with third argument 
         (@var{Value})".

:- pred dht_consult_server_nb(NodeId, Key, Value): 
        gnd * gnd * gnd => gnd * gnd * gnd # 
        "Second variant is more similar to check for existence, All arguments
         are ground by the time predicate is called, so the only use is 
         success/failure of predicate itself.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_find_and_extract_b(Key, Value):-
        find_server(Key, _KeyHash, ServerId),
        dht_extract_from_server_b(ServerId, Key, Value).

:- doc(dht_find_and_extract_b/2, 
           "@pred{dht_find_and_extract_b/2} is meant to perform two actions:
            @begin{itemize}
             @item search for node responsible for value supplied as 
                   a @var{Key};
             @item perform @pred{dht_extract_from_server_b/3} on a node found.
            @end{itemize} Unlike @pred{dht_find_and_consult/2}, this predicate
            removes matching records form the local databases of 
            the corresponding nodes.").

:- pred dht_find_and_extract_b(Key, Value): int * var => int * gnd #

    "An invocation of this type would bind all free variables in @var{Value} to 
     values present in the local database of the corresponding node, and 
     the values against which the matching was performed are erased.".

:- pred dht_find_and_extract_b(Key, Value):int * gnd #

    "Invocation with two ground arguments is equal to checking presence of 
     term specified by @var{Value} inside the DHT, and removing it in case of 
     successful search.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_extract_from_server_b(ServerID, Key, Value):-
        ground(ServerID),
        Node = ~dht_rpr_node_by_id(ServerID),
	dht_config:dht_server_id(Self),
        (
            Self = Node ->
	    dht_storage:dht_extract_b(Key, Value)
            ;
            remote_node_call(ServerID, dht_extract_from_server_b(ServerID, 
                                                                 Key, 
                                                                 Value))
        ).

:- doc(dht_extract_from_server_b/3,
           "This predicate is also used entirely as a second part of 
            @pred{dht_find_and_extract/2} predicate. It's behavior differs
            from similar @pred{dht_consult_server_b/3} predicate, only in action
            that is performed over DHT. In this case 'extraction' of predicates,
            instead of simple 'consultation'").

:- pred dht_extract_from_server_b(NodeId, Key, Value): 
        gnd * gnd * var => gnd * gnd * gnd # 
    "First variant deals with blind 'extraction', when the value is unknown.
     Therefore, after predicate is successfully executed, a real value, that is 
     stored under @var{Key} is associated with third argument (@var{Value})".

:- pred dht_extract_from_server_b(NodeId, Key, Value): 
        gnd * gnd * gnd => gnd * gnd * gnd # 
    "Second variant is more similar to pointed elimination. All arguments are 
     ground by the time predicate is called, so the only use is success and 
     elimination of predicate itself or general failure on contrary.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_find_and_extract_nb(Key, Value):-
        find_server(Key, _KeyHash, ServerID),
        dht_extract_from_server_nb(ServerID, Key, Value).

:- doc(dht_find_and_extract_nb/2, 
           "@pred{dht_find_and_extract_nb/2} is meant to perform two actions:
            @begin{itemize}
             @item search for node responsible for value supplied as 
                   a @var{Key};
             @item perform @pred{dht_extract_from_server_nb/3} on a node found.
            @end{itemize} Unlike @pred{dht_find_and_consult/2}, this predicate
            removes matching records form the local databases of 
            the corresponding nodes.").

:- pred dht_find_and_extract_nb(Key, Value): int * var => int * gnd #

    "An invocation of this type would bind all free variables in @var{Value} to 
     values present in the local database of the corresponding node, and 
     the values against which the matching was performed are erased.".

:- pred dht_find_and_extract_nb(Key, Value):int * gnd #

    "Invocation with two ground arguments is equal to checking presence of 
     term specified by @var{Value} inside the DHT, and removing it in case of 
     successful search.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_extract_from_server_nb(ServerID, Key, Value):-
        ground(ServerID),
        Node = ~dht_rpr_node_by_id(ServerID),
	dht_config:dht_server_id(Self),
        (
            Self = Node ->
	    dht_storage:dht_extract_nb(Key, Value)
            ;
            remote_node_call(ServerID, dht_extract_from_server_nb(ServerID,
                                                                  Key, 
                                                                  Value))
        ).

:- doc(dht_extract_from_server_nb/3,
           "This predicate is also used entirely as a second part of 
            @pred{dht_find_and_extract/2} predicate. It's behavior differs
            from similar @pred{dht_consult_server_b/3} predicate, only in action
            that is performed over DHT. In this case 'extraction' of predicates,
            instead of simple 'consultation'").

:- pred dht_extract_from_server_nb(NodeId, Key, Value): 
        gnd * gnd * var => gnd * gnd * gnd # 
    "First variant deals with blind 'extraction', when the value is unknown.
     Therefore, after predicate is successfully executed, a real value, that is 
     stored under @var{Key} is associated with third argument (@var{Value})".

:- pred dht_extract_from_server_nb(NodeId, Key, Value): 
        gnd * gnd * gnd => gnd * gnd * gnd # 
    "Second variant is more similar to pointed elimination. All arguments are 
     ground by the time predicate is called, so the only use is success and 
     elimination of predicate itself or general failure on contrary.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_find_and_store(Key, Value):-
        find_server(Key, KeyHash, ServerID),
        display('About to store'),nl,
        dht_store_to_server(ServerID, Key, KeyHash, Value).

:- doc(dht_find_and_store/2, 
           "First part of this predicate is similar to rest of 
            @pred{dht_find_and_*/2} family - perform search over existing DHT 
            structure. After search - storage operation is performed.").

:- pred dht_find_and_store(Key, Value): int * gnd #

    "@pred{dht_find_and_store/2} is meant to perform two actions:@begin{itemize}
        @item search for a node responsible for the value supplied as @var{Key};
        @item perform @pred{dht_store_to_server/4} on node found.@end{itemize}
     Since the arguments of @pred{dht_store_to_server/4} must be ground
     both arguments of @pred{dht_find_and_store} are ground too.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_store_to_server(ServerID, Key, KeyHash, Value):-
        ground(ServerID),
        display('to server: '),display(ServerID),nl,
        Node = ~dht_rpr_node_by_id(ServerID),
        dht_config:dht_server_id(Self),
        display('Comparing '),display(Node),
        display(' to '),display(Self),nl,
        (
            Node = Self ->
            display('Storing '), display(Value), 
            display(' under key '),display(Key),
            display(' to: '),display(Self),nl,
            dht_storage:dht_store(Key, KeyHash, Value)
            ;
            remote_node_call(ServerID, dht_store_to_server(ServerID,
                                                           Key, 
                                                           KeyHash, 
                                                           Value))
        ).

:- doc(dht_store_to_server/4, 
           "Second part of @pred{dht_find_and_store/2} predicate.").

:- pred dht_store_to_server(NodeId, Key, KeyHash, Value): gnd * gnd * gnd * gnd # 
    "Store @var{Value} under given @var{Key} and also record relation between
     @var{Key} and @var{KeyHash}.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_successor(NodeID):-
        ground(NodeID),
        dht_rpr_node_id(NodeID),
        dht_config:dht_server_id(Self),
        (
            dht_successor(Self) ->
            add_successor(NodeID)
            ;
            true
        ).

:- doc(check_successor/1, 
           "@pred{check_successor/1} aims to check whether argument supplied (of
            type @pred{node_id/2}) is valid as a successor of current node. 
            A step-by-step behavior is following: @begin{enumerate}
            @item{} check for @pred{ground/1}-ness of the variables,
            @item{} get identifier of current node,
            @item{} get IP of current node
            @item{} check whether current node is deemed to be it's own 
                    successor,
            @item{} add new node as successor.
            @end{enumerate} This type of behavior, is useful in case DHT 
            previously consisted of a single node, that had all fingers pointing
            to itself. Since only place that can really make any difference is 
            4'th step, then if it does not hold, no modifications are needed and
            computation goes to default case, that has empty body.").

:- pred check_successor(NodeId): gnd # 
    "Check whether value supplied is equal to successor's identity.". 
:- pred check_successor(NodeId): var #
    "Default to true, and give up.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_finger_table(OwnId):-
        ground(OwnId),
        OwnIdInt is integer(OwnId),
        hash_power(Power),
        Nmin1 is Power - 1,
        put_on_circle(OwnIdInt+(2**Nmin1), Start),
        step_finger_table(Nmin1, Start, OwnIdInt, OwnIdInt).

:- doc(init_finger_table/1, 
           "This predicate is by convention called from @pred{dht_init/1} 
            predicate. It asserts entry with the biggest index number and 
            launches a recursive @pred{step_finger_table/4} predicate.").

:- pred init_finger_table(OwnId): int # 
    "Perform basic preparations for finger table initialization and start the 
     recursive call.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

step_finger_table(N, Start, End, NodeId):-
        ground(N),   ground(Start), 
        ground(End), ground(NodeId),
        (
            0 = N ->
            dht_routing:dht_set_finger(1, Start, End, NodeId),
            display('done with finger table'),nl
            ;
            Idx is N + 1,
            dht_routing:dht_set_finger(Idx, Start, End, NodeId),
            Nmin1 is N - 1,
            put_on_circle(NodeId+(2**Nmin1), NewStart),
            step_finger_table(Nmin1, NewStart, Start, NodeId)
        ).


%step_finger_table(N, Start, End, NodeId):-

%        ground(N),   ground(Start), 
%        ground(End), ground(NodeId),
%        Idx is N + 1,

%        dht_routing:dht_set_finger(Idx, Start, End, NodeId),



%        Nmin1 is N - 1,
%        put_on_circle(NodeId+(2**Nmin1), NewStart),
%        step_finger_table(Nmin1, NewStart, Start, NodeId).

:- doc(step_finger_table/4,
           "A recursive predicate launched by @pred{init_finger_table/1}.
            It's sole aim is to assert corresponding entries into finger-table.
            Since finger-table is entirely stored in @file{dht_routing.pl} 
            file/module, it's usage is quite extensive.").

:- pred step_finger_table(N, Start, End, NodeId): int * int * int * int #
    "Recursively populate finger table.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

put_on_circle(LineId, CircleNum):-
        hash_size(Size),
        LineNum is integer(LineId),
        CircleNum is mod(LineNum, Size).

:- doc(put_on_circle/2, 
           "Get integer part of division of @var{LineId} by size of current 
            hash.").
:- pred put_on_circle(LineId, CircleNum): int * var => int * int # 
    "Transform any positive integer to corresponding value of DHT ring.
     Arithmetic equivalent would look like: 
     @tt{CircleNum = mod(LineNum,hash_size())}".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_random_finger_index(2).
%get_random_finger_index(Idx):-
%        hash_power(Power),
%        random(2, Power, Idx).

:- doc(get_random_finger_index/1,
           "Get random integer number withing following bound [2,HashPower],
            where HashPower is power of 2, used to construct current DHT.").

:- pred get_random_finger_index(Idx): var => int  # 
    "Assign an integer value to free variable, with constraints mentioned
     above.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_finger_node(Idx, ServerID):-
        Node = ~dht_rpr_node_by_id(ServerID),
        dht_routing:dht_update_finger(Idx, Node),
        dht_rpr:dht_rpr_register_node(ServerID).

:- doc(update_finger_node/2,
           "This predicate relies on execution of 
            @pred{dht_update_finger/2} predicate from @file{dht_routing.pl},
            but also update values stored in @file{dht_rpr.pl}. ").

:- pred update_finger_node(Idx, Node): int * gnd # 
    "Replace existing finger information with given one.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stabilize_successor(Succ, nil):-
        ground(Succ),
        dht_config:dht_server_id(Self),
%        display('nil case'),nl,
        OwnID = ~dht_rpr_id_by_node(Self),
        display('About to notify '),display(Succ), display(' of own existance.'),nl,
        remote_node_call(Succ, dht_notify(OwnID)),
        display('Notification performed.'),nl,
        dht_predecessor(Debug1),dht_successor(Debug2),
        display('Debug. Pred: '),display(Debug1),display(' Succ: '),display(Debug2),nl,
        !.
stabilize_successor(Succ, Self):-
        display('received Self: '),display(Self),nl,
        ground(Succ), ground(Self),
%        Self = ~dht_rpr_node_by_id(OwnID),
        dht_config:dht_server_id(Self),
%        display('calm down case'),nl,
%        dht_rpr:dht_rpr_node(OwnID),
        !.      
stabilize_successor(Succ, Candidate):-
        ground(Succ), ground(Candidate),
        dht_config:dht_server_id(Self),
%        Candidate = ~dht_rpr_node_by_id(CandidateID),
        in_circle_oo(Candidate, Self, Succ),
        remote_node_calll(Succ, dht_id_by_node(CandidateID), debug),
        add_successor(CandidateID),
%        display('someone in between case'),nl,
% This CUT should help anyway. Am I right?  
        !,
        OwnID = ~dht_rpr_id_by_node(Self),
        remote_node_call(CandidateID, dht_notify(OwnID)),
        !.
%%%
%%%
%%%?????? Show me the case when you need this.
stabilize_successor(Succ, Candidate):-
        ground(Succ), ground(Candidate),
        dht_config:dht_server_id(Self),
%        display('i am a new one case'),nl,
        OwnID = ~dht_rpr_id_by_node(Self),
        remote_node_call(Succ, dht_notify(OwnID)).

:- doc(stabilize_successor/2,
           "Perform stabilization of successor-predecessor connection. If there
            is some node in between current node and it's successor, it would
            get integrated into structure here.").

:- pred stabilize_successor(SuccessorId, NodeId): gnd * gnd # 
    "Check whether currently mentioned successor is an actual one.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_successor(ServerID):-
        ground(ServerID),
        Node = ~dht_rpr_node_by_id(ServerID),
        dht_config:dht_server_id(Self),
        next_on_circle(Self, Next),
        % why not
        %    dht_routing:dht_update_finger().
        % ?
        dht_routing:dht_set_finger(1, Next, Node, Node),
        dht_rpr:dht_rpr_register_node(ServerID).

:- doc(add_successor/1, 
           "@pred{add_successor/1} is a straightforward implementation of 
            following behavior:@begin{enumerate}
            @item{} get identifier of current node,
            @item{} get next by applying formula 
                    next = current_id+1 mod 2**hash_power, 
            @item{} remove entrance from finger_table with index '1'. 
            @item{} add following entrance to finger table 
   @tt{finger_table(1, next, finger_interval(next, new_node_id), new_node_id)}.
            @item{} remove previous entrances from RPR-module DB, 
            @item{} add new node to RPR-module DB.
            @end{enumerate}").

:- pred add_successor(NodeId): gnd # 
    "Add/replace first entry from finger table with given one.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_predecessor(PredID):-
        ground(PredID),
        Pred = ~dht_rpr_node_by_id(PredID),
        dht_routing:dht_set_predecessor(Pred),
        dht_rpr:dht_rpr_register_node(PredID).

:- doc(add_predecessor/1, 
           "@pred{add_predecessor/1} is a straightforward implementation of 
            following bahavior:@bagin{enumerate}
            @item{} remove instance of @pred{dht_predecessor/1} from local DB,
            @item{} assert new node as predecessor,
            @item{} clear all references to node with same identifier from
                    RPR-module DB, 
            @item{} assert new node to RPR-module DB.
            @end{enumerate}").

:- pred add_predecessor(NodeId): gnd # 
    "Add/replace value of node's predecessor.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iterate_preceding_fingers(Id, CurrID, NodeID):-
        dht_rpr:dht_rpr_node_by_id(CurrID, Curr),
        remote_node_call(CurrID, dht_successor(Succ)),
        % why we need not_in_circle? Because otherwise we would write 
        %   remote_node_call(successor(SuccessorId), CurrentNode)
        % twice - in first and in second variant of iterate_preceding_fingers.
        % which is definitely waste of resources.
        not_in_circle_oc(Id, Curr, Succ),
        remote_node_call(CurrID, dht_closest_preceding_finger(Id, NextID)),
        iterate_preceding_fingers(Id, NextID, NodeID),
        !.
iterate_preceding_fingers(_Id, NodeID, NodeID).

:- doc(iterate_preceding_fingers/3, 
           "@pred{iterate_preceding_fingers/3} is called entirely from 
            @pred{dht_find_predecessor/2}. Technically speaking, it is just 
            another, imitation of a loop, that goes calling predicates remotely 
            through the DHT.").

:- pred iterate_preceding_fingers(Id, NextNode, Node): int * gnd * gnd # 
    "Iterate through finger tables of remote nodes. Terminate, once @var{Id}
     ca not be located between some node and its successor.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remote_node_call(Base, Term):-
        catch(dht_rpr:dht_rpr_call(Base, Term),
              error(node_failure(FailedId),_Err),
              node_failure_handler(FailedId)).

remote_node_calll(Base, Term, debug):- 
        catch(dht_rpr:dht_rpr_call(Base, Term, debug),
              error(node_failure(FailedId),_Err),
              node_failure_handler(FailedId)).

:- doc(remote_node_call/2, 
           "@pred{remote_node_call/2} is a straightforward wrapper around a 
            dht_rpr_call predicate, since it needs a catch statement which 
            appears to be quite long. So it will probably introduce much more
            mess then a one-line wrapper with sensible name.").

:- pred remote_node_call(Node, Term): gnd * var # 
    "Perform remote predicate call.".

:- pred remote_node_call(Node, Term): gnd * var # 
    "Perform remote predicate call.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

node_failure_handler(FailedId):-
        hash_power(Power),
        check_last_finger(Power, FailedId),
        PowerMinusOne is Power-1,
        eliminate_from_other_fingers(PowerMinusOne, FailedId),
        verify_predecessor(FailedId).

:- doc(node_failure_handler/1, 
           "@pred{node_failure_handler/1} is actually what it is supposed to 
            be - a handler that is executed once a node failure occurs. 
            Step-by-step behavior is following:@begin{enumerate}
            @item{} get power of DHT,
            @item{} check whether last entry of finger_table has any information
                    about failed node,
            @item{} start checking all other finger_table entrances (this item 
                    is more verbosely explained in
                    @pred{eliminate_from_other_fingers/2}).
            @end{enumerate}").

:- pred node_failure_handler(FailedId) : gnd # 
    "Eliminate given node (@var{FailedId}) from routing table.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_last_finger(Idx, FailedId):-
        ground(Idx), ground(FailedId),
        dht_finger_table(Idx, FailedId),
        dht_config:dht_server_id(OwnId),
        dht_update_finger(Idx, OwnId),
        !.
check_last_finger(_Idx, _FailedId):- !.

:- doc(check_last_finger/2, 
           "a straightforward descrition:@begin{enumerate}
            @item{} check both arguments for @pred{ground/1}-ness,
            @item{} get last entry of finger_table from local DB if node 
                    identifier corresponding to it is equal to failed one,
            @item{} get identifier of current node,
            @item{} remove last entry from local DB,
            @item{} save current node as the one corresponding to last entry of 
                    finger_table.
            @end{enumerate}. Since alternative is possible only at 2'nd step. 
            Alternative definition of @pred{check_last_finger/2} has empty 
            body.").

:- pred check_last_finger(Idx, FailedId): gnd * gnd # 
    "Check whether failed node is mentioned in last entry of finger table.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
eliminate_from_other_fingers(1, FailedId):-
        ground(FailedId),
        dht_finger_table(1, NodeId),
        dht_finger_table(2, NextNodeId),
        replace_node_if_failed(1, NodeId, FailedId, NextNodeId),
        !.
eliminate_from_other_fingers(Idx, FailedId):-
        ground(FailedId),
        ground(Idx), Idx \= 1,
        dht_finger_table(Idx, NodeId),
        IdxPlusOne is Idx+1,
        dht_finger_table(IdxPlusOne, NextNodeId),
        replace_node_if_failed(Idx, NodeId, FailedId, NextNodeId),
        IdxMinusOne is Idx-1,
        eliminate_from_other_fingers(IdxMinusOne, FailedId).

:- doc(eliminate_from_other_fingers/2, 
           "@pred{eliminate_from_other_fingers/2} has one sole purpose - walk
            down the finger_table starting from record that has index 
            hash_power-1. At each step make a call to 
            @pred{replace_node_if_failed/3}, which is actually replacing node 
            (if it failed) in the record with the next one in the 
            finger_table.").

:- pred eliminate_from_other_fingers(Idx, NodeId): gnd * gnd # 
    "Check whether failed node is mentioned in any other entry of 
     finger table.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

replace_node_if_failed(Idx, NodeId, FailedId, NextNodeId):-
        ground(Idx), int(Idx),
        ground(NodeId), int(NodeId),
        ground(FailedId), int(FailedId),
        ground(NextNodeId), int(NextNodeId),
        NodeId = FailedId,
        dht_update_finger(Idx,NextNodeId),
        !.
replace_node_if_failed(Idx, NodeId, FailedId, _NextNodeId):-
        ground(Idx), int(Idx),
        ground(NodeId), int(NodeId),
        ground(FailedId), int(FailedId),
        ground(_NextNodeId), int(_NextNodeId),
        NodeId \= FailedId.

:- doc(replace_node_if_failed/3, 
           "@pred{replace_node_if_failed/3} is used to define whether 
            finger table entry supplied as first argument contains a failed 
            node, which is supplied as second argument. If it is so, all entries
            of that type are removed from local DB. And a new one is inserted.
            New record contains different node (probably living one), which was 
            supplied as third argument. ").

:- pred replace_node_if_failed(Idx, NodeId, FailedId, NextNodeId): 
    int * int * int * int # 
    "If current entry of finger table contains failed node - replace it with
     next entry of finger table.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(verify_predecessor/1, 
           "@pred{verify_predecessor/1} is developed for cases of poorly 
            populate Chord ring. Sometimes it happens that failed node is direct
            predecessor of current one. So learning early about the failure is a
            nice idea. This predicate is making use of 
            @pred{reset_predecessor/0} which is also used in 
            @pred{dht_check_predecessor/1}.").

:- pred verify_predecessor(FailedId): gnd # 
    "Check whether failed node (@var{FailedId}), is mentioned as node's 
     predecessor.".

verify_predecessor(_FailedId):-
        dht_predecessor(nil),
        !.
verify_predecessor(FailedId):-
%        display('verify_predecessor nil case'),nl,
        ground(FailedId),
        dht_predecessor(PredId),
        ground(PredId),
        FailedId \= PredId,
%        display('no predecessor affected'),nl,
%        display(FailedId),display('//'),display(PredId),nl,
        !.
verify_predecessor(FailedId):-
        display('verify_predecessor reset case'),nl,
        ground(FailedId),
        dht_predecessor(FailedId),
%        display('reseting predecessor'),nl,
        reset_predecessor.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
print_fingers:-
        hash_power(Power),
        step_print_fingers(Power).

step_print_fingers(0).
step_print_fingers(N):-
        dht_finger_table(N,_A),
%        display(finger(N, A)),nl,
        NMinusOne is N - 1,
        step_print_fingers(NMinusOne).
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
