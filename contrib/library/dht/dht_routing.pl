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
:- module(dht_routing, 
         [
          dht_finger_table/2,
          dht_finger_start/2,
          dht_update_finger/2,
          dht_set_finger/4,
          dht_predecessor/1,
          dht_set_predecessor/1,
          dht_reset_predecessor/0    
         ],[
          assertions,regtypes,isomodes
         ]).

:- doc(title, "Finger table and routing information").
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

:- doc(module, "This module holds the basic operations over the
   finger table and other routing information. Developed for the sake
   of simplicity, it should be used only within the
   @file{dht_logic.pl} file.  Probably the most important and abstract
   part of the routing is the finger table.  Here is a finger table
   copy of the node representing identifier 0 (from the Chord paper):

@begin{verbatim} finger_table(1, 1, finger_interval(1,2), 1).
 finger_table(2, 2, finger_interval(2,4), 3).
 finger_table(3, 4, finger_interval(4,0), 0).
@end{verbatim}. 

   This table reflects the case where there are 8 
   identifiers in total in the identifier circle and three running nodes with  
   identifiers 0, 1, and 3. In general each entry can be represented as 
   @tt{finger_table(idx, start, interval, succ)}. Here is a 
   brief description of each argument:@begin{itemize}
   @item{} @tt{idx} - Since arrays are not native for Prolog, I put the 
                      index as the first argument to the
                      finger_table predicate to achieve indexing.
   @item{} @tt{start} - ID that starts the corresponding section of the table.
   @item{} @tt{interval} - IDs that fit into the corresponding section of 
                             table, represented in the form: 
               @tt{finger_interval(interval_start, interval_end).}, where:
                  @begin{itemize}
                  @item{interval_start} - ID that starts this interval.
                  @item{interval_end} - first ID of the next interval.
                  @end{itemize}
  @item{} @tt{succ} - ID of node responsible for the corresponding 
                        section of table, represented in 
               @tt{node_id(id, ip).} form, where:
                  @begin{itemize}
                  @item{id} - ID of a node.
                  @item{ip} - IP address of a node.
                  @end{itemize}
  @end{itemize}").

%:- use_module(library(concurrency)).

:- concurrent finger_table/4.
:- concurrent predecessor/1.

dht_finger_table(Idx, Node):-
      int(Idx),int(Node),
      current_fact_nb(finger_table(Idx, _A, _B, Node)).

:- doc(dht_finger_table/2, 
           "It serves as a read-only interface to the finger table 
            information as follows: @var{Idx} stands for an index 
            in an array, and @var{Node} is the number of the node which
            is recorded in the entry with that index.").

:- pred dht_finger_table(Idx, Node): var * var => int * int # 
    "This case is equal to getting any (possibly random) entry from
     the finger table.". 

:- pred dht_finger_table(Idx, Node): var * int =>
                                     int * int # 
    "Get some (one or several if backtracking is exploited) indexes that
     mention particular node numer, supplied as @var{Node}.". 

:- pred dht_finger_table(Idx, Node): int * var =>
                                     int * int # 
    "Get the identifier of the node, indexed by @var{Idx}. Theoretically,
     there should be no more than on entry for each value of index.". 

:- pred dht_finger_table(Idx, Node): int * int # 
    "Check whether a particular combination (index and node number) is
     present in finger table.".



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_finger_start(Idx, NextId):-
      display('finger_start: begin'),nl,
      int(Idx),
      current_fact_nb(finger_table(Idx,NextId, _, _)),
      display('finger_start: '),display(Idx),display(' '),display(NextId),nl.

:- doc(dht_finger_start/2, 
           "This predicate may be regarded as a read-only interface to map 
            between the index is a finger table and the beginning of 
            the section of the DHT circle.").

:- pred dht_finger_start(Idx, NextId): var * var =>
                                       int * int #
    "Get any entry, in other words and entry that has any index and
     points to any segment in the circle. Common constraints on finger tables,
     do hold anyway:@begin{itemize}
     @item{} result unifies with the content of finger table;
     @item{} predicate enumerates possible results on backtracking.
     @end{itemize}".

:- pred dht_finger_start(Idx, NextId): var * int =>
                                       int * int #
    "Ask for the index of entry that mentions a particular node as 
     the starting one.".

:- pred dht_finger_start(Idx, NextId): int * var => 
                                       int * int #
    "Get the starting node for  a certain entry of finger table.".

:- pred dht_finger_start(Idx, NextId): int * int #
    "Check for the presence of a correspondence between a certain entry and its
     starting node.".



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_set_finger(Idx, Start, End, Node):-
      int(Idx),int(Start),int(End),int(Node),
      (
          current_fact_nb(finger_table(Idx, _, _, _)) ->
          retractall_fact(finger_table(Idx, _, _, _))
          ;
          true
      ),
      assertz_fact(
         finger_table(Idx,Start,finger_interval(Start,End),Node)).

%      !.
%dht_set_finger(Idx, Start, End, Node):-
%      ground(Idx),int(Idx),
%      ground(Start),int(Start),
%      ground(End),int(End),
%      ground(Node),int(Node),
%      assertz_fact(
%         finger_table(Idx,Start,finger_interval(Start,End),Node)).

:- doc(dht_set_finger/4, 
           "This predicate sets the value of finger table entries. If any entry
            with the same index (@var{Idx})  exists, it is erased before
            new value is asserted.").

:- pred dht_set_finger(Idx, Start, End, Node): int * int *
                                               int * int #  
    "All the arguments are to be ground and of type integer.".



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_update_finger(Idx, NodeId):-
        int(Idx),int(NodeId),
        current_fact_nb(
          finger_table(Idx,IntStart,Interval,_A)),
        Entry = 
          finger_table(Idx,IntStart,Interval,NodeId),
        ground(Entry),

        retractall_fact(
          finger_table(Idx,IntStart,Interval,_A)),
        asserta_fact(Entry).

:- doc(dht_update_finger/2, 
           "@pred{dht_update_finger/2} takes care of changing node value to 
            @var{NodeId} for entry with index number equal to @var{Idx}. 
            This predicate has entirely imperative behavior. Both arguments
            are to be ground at the moment of call.").

:- pred dht_update_finger(Idx, NodeId): int * int # 
    "Stores supplied information, previously erasing all old information.".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%dht_own_id(OwnId):-
%      own_id(OwnId).
%
%:- doc(dht_own_id/1,
%           "@pred{dht_own_id/1} is a read-only interface to consult the number
%            of the current node.").
%
%:- pred dht_own_id(OwnId): var => int # 
%     "Return the identifier of the calling node.".
%    "First of all, given a free variable, this predicate would assign it value
%     of current node id.".
%
%:- pred dht_own_id(OwnId): int => int # 
%     "Check that @var{OwnId} is the identifier of the local node.".
%    "Second, if argument is already ground a simple check is performed.
%     Predicate may be used as a 'guard' in this case.".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%dht_set_own_id(OwnId):-
%      ground(OwnId),int(OwnId),
%	integer(OwnId),
%      assertz_fact(own_id(OwnId)).
%
%:- doc(dht_set_own_id/1,
%           "Store internally the current node ID.").
%
%:- pred dht_set_own_id(OwnId): int => int # 
%    "Store internally the current node ID data.".
      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_predecessor(PredId):-
      current_fact_nb(predecessor(PredId)).

:- doc(dht_predecessor/1,
           "A common read-only interface to get or check the 
            ID of the predecessor node.").

:- pred dht_predecessor(PredId): var => int # 
    "Get number of predecessor node.".

:- pred dht_predecessor(PredId): int # 
    "Check whether particular number is associated with 
     current predecessor.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_set_predecessor(PredId):-
      ground(PredId),int(PredId),
      current_fact_nb(predecessor(_A)),
      retractall_fact(predecessor(_A)),
      assertz_fact(predecessor(PredId)),
      !.
dht_set_predecessor(PredId):-
      ground(PredId),int(PredId),
      assertz_fact(predecessor(PredId)).

:- doc(dht_set_predecessor/1,
           "Write interface for predecessor ID. It checks the type of
            its argument and saves argument locally.").

:- pred dht_set_predecessor(PredId): int # 
    "Save @var{PredId} as the ID of predecessor for current node.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_reset_predecessor:-
      current_fact_nb(predecessor(nil)),
      !.
dht_reset_predecessor:-
      current_fact_nb(predecessor(A)),
      retractall_fact(predecessor(A)),
      assertz_fact(predecessor(nil)),
      !.
dht_reset_predecessor:-
      assertz_fact(predecessor(nil)).

:- doc(dht_reset_predecessor/0,
           "Set value of preceding node index to 'nil' whatever it's prior
            value is.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      
