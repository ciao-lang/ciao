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
:- module(dht_rpr, 
         [dht_rpr_register_node/1,
          dht_rpr_register_node/2,
          dht_rpr_node_by_id/2,
          dht_rpr_id_by_node/2,
          dht_rpr_node_id/1,
          dht_rpr_compose_id/3,
          dht_rpr_clear_by_node/1,
          dht_rpr_node/1,
          dht_rpr_call/2,
          dht_rpr_call/3,
	  node_id/2], 
         [
          assertions,regtypes,isomodes
         ]).

:- doc(title, "Remote predicate calling utilities").
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

:- doc(module, "RPR stands for Remote PRedicate calling.  The
   basic functionality of a remote procedure (or predicate, in Prolog
   terms of Prolog) call is gathered here. Thus module contains
   predicates for the execution of remote calls and manipulation
   routines of the module-specific database.").

:- use_module(library(format), [format/2]).
:- use_module(library(sockets), [connect_to_socket/3]).
%:- use_module(library(dynamic), [dynamic/1]).
%:- use_module(library(concurrency)).

:- use_module(dht_config).
:- use_module(dht_misc).

:- concurrent node_id/2.

:- doc(node_id/2, 
           "@pred{node_id/2}is an auxilary prediate for internal data 
            structures hanling.").

:- regtype dht_rpr_node_id(NodeID) #
   "@var{NodeID} is a term of type @pred{node_id/2} with first argument 
    as integer value and second as IP/DNS address.".

dht_rpr_node_id(NodeID):-
    ground(NodeID),
    NodeID = node_id(Node, Address),
    integer(Node),
    atm(Address).

:- regtype dht_rpr_comp_node_id(NodeID) #
    "@var{NodeID} is a term of type @pred{node_id/2} with first argument
     as free variable or integer and second an IP/DNS address".

dht_rpr_comp_node_id(NodeID):-
    NodeID = node_id(Node, Address),
    atm(Address),
    (
        var(Node)->
        true
        ;
        integer(Node)
    ).

:- regtype dht_rpr_comp_node_addr(NodeID) #
    "@var{NodeID} is a term of type @pred{node_id/2} with first argument
     as integer and second as free variable or IP/DNS address".

dht_rpr_comp_node_addr(NodeID):-
    NodeID = node_id(Node, Address),
    integer(Node),
    (
        var(Address) ->
        true
        ;
        atm(Address)
    ).

:- regtype dht_rpr_comp_node(NodeID) #
   "@var{NodeID} is a term of type @pred{node_id/2} with first argument is
    either a free variable or an integer and second is either a free variable
    or a IP/DNS address".

dht_rpr_comp_node(NodeID):-
    NodeID = node_id(Node, Address),
    (
        var(Node)->
        true
        ;
        integer(Node)
    ),
    (
        var(Address) ->
        true
        ;
        atm(Address)
    ).


dht_rpr_register_node(Node, NodeIP):-
         ground(Node), ground(NodeIP),
         (
             current_fact_nb(node_id(Node, DifferentIP)) ->
             ground(DifferentIP),
             (
                 DifferentIP = NodeIP ->
                 true
                 ;
                 retract_fact(node_id(Node, DifferentIP)),
                 asserta_fact(node_id(Node, NodeIP))
             )
             ;
             asserta_fact(node_id(Node, NodeIP))
         ).

:- doc(dht_rpr_register_node/2,
           "General write interface to DB of physical nodes (those that have IP
            addresses). Information is re-written every time, so no old entries
            are expected to remain.").

:- pred dht_rpr_register_node(Node, NodeIP): int * gnd #

    "@pred{dht_rpr_register_node/2} is responsible for the management
     of a module-specific database that stores information on
     node identifiers and IP addresses corresponding to them.
     Despite there is only one usage mode the behavior may differ
     depending on the state of module-specific database. For instance,
     if the database already contains information about a node, 
     whose identifier is equal to the one supplied in
     @var{Node}, a newly supplied entry would be written over the old one.
     A new entry is added to the module-specific database 
     otherwise.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_rpr_register_node(node_id(Node, NodeIP)):-
         dht_rpr_register_node(Node, NodeIP).

:- doc(dht_rpr_register_node/1,
           "Sometimes information from remote nodes is received in the form of
            a @pred{node_id/2}. In these cases a single argument predicate
            might be useful.").

:- pred dht_rpr_register_node(NodeID): dht_rpr_node_id #
    "Save node identity to module-specific DB.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_rpr_node_by_id(node_id(Node, _NodeIP), Node).

:- doc(dht_rpr_node_by_id/2,
           "Just a convenient wrapper around module-specific data structures,
            which is used in combination with Ciao functional syntax.").

:- pred dht_rpr_node_by_id(NodeID, Node): dht_rpr_node_id * var => 
                                          dht_rpr_node_id * gnd #
    "Extract value of node from the node identifier @var{NodeID}.".

:- pred dht_rpr_node_by_id(NodeID, Node): dht_rpr_node_id * gnd => 
                                          dht_rpr_node_id * gnd #
    "Check whether the value of the @var{NodeID} node identifier corresponds
     to the @var{Node} value.".

:- pred dht_rpr_node_by_id(NodeID, Node): var * gnd =>
                                          dht_rpr_comp_node_addr * gnd #
    "Return the node identifier associated to the exact node number.".

:- pred dht_rpr_node_by_id(NodeID, Node): var * var #
    "This case is merely useless, however perfectly possible.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_rpr_id_by_node(Node, NodeID):-
         integer(Node),
         current_fact_nb(node_id(Node, NodeIP)),
         NodeID = node_id(Node, NodeIP).

:- doc(dht_rpr_id_by_node/2,
           "Convenient wrapper around internal data structures, 
            as well as @pred{dht_rpr_node_by_id/2}, this predicate is usually
            used in combination with Ciao functional syntax.").

:- pred dht_rpr_id_by_node(Node, NodeID): int * var =>
                                          int * dht_rpr_node_id #
    "Get node identity by it's number.".

:- pred dht_rpr_id_by_node(Node, NodeID): int * dht_rpr_comp_node_id =>
                                          int * dht_rpr_node_id # 
    "Lookup and fill ramaining id field.".

:- pred dht_rpr_id_by_node(Node, NodeID): int * dht_rpr_comp_node_addr =>
                                          int * dht_rpr_node_id # 
    "Lookup and fill remainting address field.".

:- pred dht_rpr_id_by_node(Node, NodeID): int * dht_rpr_node_id # 
    "Check whether local database really has record about @var{NodeID} 
     with @var{Node} as its node number.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%dht_rpr_id(node_id(_Node, _NodeIP)).

%:- doc(dht_rpr_id/1,
%           "Convenient way to check whether an argument holds a node identity
%            structure.").

%:- pred dht_rpr_id(NodeID): var => var #
%    "No binding is performed within this predicate. Just type checking.".
%:- pred dht_rpr_id(NodeID): gnd => gnd #
%    "No modification of data, no binding. Just type checking.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_rpr_compose_id(Node, NodeIP, node_id(Node, NodeIP)):-
         integer(Node),
         ground(NodeIP).

:- doc(dht_rpr_compose_id/3,
           "Compose identity structure out of arguments provided").

:- pred dht_rpr_compose_id(Node, NodeIP, NodeID): int * gnd * var => 
                                                  int * gnd * dht_rpr_node_id #
    "Compose an internal structure.".

:- pred dht_rpr_compose_id(Node, NodeIP, NodeID):int * gnd * dht_rpr_node_id =>
                                                 int * gnd * dht_rpr_node_id #
    "Check whether arguments correspond to structure supplied.".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_rpr_clear_by_node(Node):-
         retract_fact_nb(node_id(Node, _)).

:- doc(dht_rpr_clear_by_node/1,
           "Write-interface (or more precisely, erase-interface) to DB of 
            physical nodes (those that have IP-address).
            @pred{dht_rpr_clear_node} is a dumb-wrapper around
            retraction operation over module-specific database. As usually,
            retraction may be performed when the argument is a free variable.").

:- pred dht_rpr_clear_by_node(Node): dht_rpr_comp_node_addr => gnd #
    "Erase any (possibly random) node information.".

:- pred dht_rpr_clear_by_node(Node): dht_rpr_node_id => gnd #
    "Erase exact node.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_rpr_node(node_id(NodeId, NodeIP)):-
         current_fact_nb(node_id(NodeId, NodeIP)).

:- doc(dht_rpr_node/1,
           "Generic read interface to DB of physical nodes.").

:- pred dht_rpr_node(Node): dht_rpr_node_id #

    "Checks for presence of any information on a
     node supplied as @var{Node}.".

:- pred dht_rpr_node(Node): dht_rpr_comp_node #

    "Get node that matches given template.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_rpr_call(node_id(_Id, IP), Goal):-
         ground(IP),
         rpr_call_or_fail(IP, Goal, nodebug),
         !.
dht_rpr_call(node_id(Id, _IP), Goal):-
         ground(Id),
         current_fact_nb(node_id(Id, Host)),
         rpr_call_or_fail(Host, Goal, nodebug),
         !.
dht_rpr_call(Node, Goal) :-
         current_fact_nb(node_id(Node, NodeIP)) ->
         rpr_call_or_fail(NodeIP, Goal, nodebug)
         ;
         rpr_call_or_fail(Node, Goal, nodebug).

:- doc(dht_rpr_call/2, 
           "@pred{dht_rpr_call/2} execute a goal remotely. The platform 
            for remote execution is specified by @var{HostId}. @var{Goal} 
            might be a fully instantiated term as well as partially
            instantiated one (as used in any other goal).").

:- pred dht_rpr_call(HostId, Goal): dht_rpr_comp_node_id * {var,callable} => 
                                    dht_rpr_comp_node_id * {gnd,callable} #
    "Perform remote call of partially instantiated goal: 
     e.g., a pattern search. Remote host for execution is specified via
     IP/DNS address of node identity, rest of identity is ignored".


:- pred dht_rpr_call(HostId, Goal): dht_rpr_comp_node_addr * {var,callable} => 
                                    dht_rpr_comp_node_addr * {gnd,callable} #
    "Perform remote call of partially instantiated goal: e.g., a pattern 
     search. Remote host for execution is specified via integer of node 
     identity, IP/DNS address is searched through local database.".


:- pred dht_rpr_call(HostId, Goal): dht_rpr_node_id * {var,callable} => 
                                    dht_rpr_node_id * {gnd,callable} #
    "Perform remote call of partially instantiated goal: e.g., a pattern 
     search. Remote host for execution is specified via
     IP/DNS address of node identity, rest of identity is ignored".


:- pred dht_rpr_call(HostId, Goal): int * {var,callable} => 
                                    int * {gnd,callable} #
    "Perform remote call of partially instantiated goal: e.g., a pattern 
     search.Remote host for execution is specified via integer, IP/DNS address 
     is searched through local database using that integer as part of node
     identity.".

:- pred dht_rpr_call(HostId, Goal): atm * {var,callable} => 
                                    atm * {gnd,callable} #
    "Perform remote call of partially instantiated goal: e.g., a pattern 
     search. Finally make a try to use first argument as directly-specified
     IP/DNS address.".

:- pred dht_rpr_call(HostId, Goal): dht_rpr_comp_node_id * {gnd,callable} => 
                                    dht_rpr_comp_node_id * {gnd,callable} #
    "Perform remote call of fully instantiated goal: 
     e.g., a pattern search. Remote host for execution is specified via
     IP/DNS address of node identity, rest of identity is ignored".


:- pred dht_rpr_call(HostId, Goal): dht_rpr_comp_node_addr * {gnd,callable} => 
                                    dht_rpr_comp_node_addr * {gnd,callable} #
    "Perform remote call of fully instantiated goal: e.g., a pattern 
     search. Remote host for execution is specified via integer of node 
     identity, IP/DNS address is searched through local database.".


:- pred dht_rpr_call(HostId, Goal): dht_rpr_node_id * {gnd,callable} => 
                                    dht_rpr_node_id * {gnd,callable} #
    "Perform remote call of fully instantiated goal: e.g., a pattern 
     search. Remote host for execution is specified via
     IP/DNS address of node identity, rest of identity is ignored".


:- pred dht_rpr_call(HostId, Goal): int * {gnd,callable} => 
                                    int * {gnd,callable} #
    "Perform remote call of fully instantiated goal: e.g., a pattern 
     search.Remote host for execution is specified via integer, IP/DNS address 
     is searched through local database using that integer as part of node
     identity.".

:- pred dht_rpr_call(HostId, Goal): atm * {gnd,callable} => 
                                    atm * {gnd,callable} #
    "Perform remote call of fully instantiated goal: e.g., a pattern 
     search. Finally make a try to use first argument as directly-specified
     IP/DNS address.".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_rpr_call(HostId, Goal, debug) :-
         current_fact_nb(node_id(HostId, Host)),
         rpr_call_or_fail(Host, Goal, debug),
         !.
dht_rpr_call(HostId, Goal,  _) :-
         current_fact_nb(node_id(HostId, Host)),
         rpr_call_or_fail(Host, Goal, nodebug).

:- doc(dht_rpr_call/3, 
           "The only difference this predicate has with @pred{dht_rpr_call/2}
            is debug-enabling switch. Third parameter is expected to be 
            responsible for that. It takes one of two possible values
            'debug' or 'nodebug'. The first one is default. If default 
            value is used, the standard output stream is populated with
            various debugging information.").

:- pred dht_rpr_call(HostId, Goal, Debug): int * term * gnd #

    "@pred{dht_rpr_call/3} executes a @var{Goal} 
     remotely and prints some debugging information 
     locally. The platform for the remote execution is 
     specified by @var{HostId}.@var{Debug} variable must be bound to
     value 'debug' in order for this case to fire.".

:- pred dht_rpr_call(HostId, Term, Anything): int * term * var #

    "@pred{dht_rpr_call/3} executes a @var{Goal} remotely 
     without writing any information on the local output 
     stream. The platform for the remote execution is 
     specified by @var{HostId}.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

rpr_call_or_fail(HostIP, Term, Debug):-
         catch(rpr_call_host(HostIP, Term, Debug),
               Error,
               fail_handler(HostIP, Error)).

:- doc(rpr_call_or_fail/3,
           "Wrapper for exception handling call upon remote predicate
            invocation.").

:- pred rpr_call_or_fail(HostIP, Term, Debug) #
    "Call remotely term @var{Term} on host @var{HostIP}, in case of failure
     execute @pred{fail_handler/0}.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fail_handler(HostIP, Error):- 
         rpr_clear_nodes(node_id(HostId,HostIP)),
         throw(error(node_failure(HostId),Error)).

:- doc(fail_handler/0,
           "Convenient handler of connection failure. Performs two following
            actions: @begin{enumerate}
            @item{} clear all information on this node from module-specific DB.
            @item{} propagate exception to upper module.
            @end{enumerate}").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rpr_call_host(HostIP, Term, Debug):-
         ground(Debug), Debug = debug,
         dht_config:dht_s2s_port(Port),
         format("Calling to ~p:~p", [HostIP,Port]),
         connect_to_socket(HostIP, Port, Stream),
         format("connected ~p~n", [Stream]),
         write_pr(Stream, dht_rpr_in(Term)),
         read_pr(Stream, dht_rpr_out(Term)),
         close(Stream),
         !.
rpr_call_host(HostIP, Term, nodebug):-
         dht_config:dht_s2s_port(Port),
         connect_to_socket(HostIP, Port, Stream),
         write_pr(Stream, dht_rpr_in(Term)),
         read_pr(Stream, dht_rpr_out(Term)),
         close(Stream).

:- doc(rpr_call_host/3,
           "Low-level machinery for remote predicate execution. Mostly relies
            on @pred{write_pr/2} and @pred{read_pr/2} from 
            @file{dht_misc.pl}. First and second parameter denote remote
            host IP and remotely executable term correspondingly. Third one
            is used for enabling debug mode ('debug' term turns debug on,
            'nodebug' switches debugging off).").

:- pred rpr_call_host(HostIP, Term, Debug): gnd * var * gnd => 
                                            gnd * var * gnd #
    "Execute @var{Term} on @var{HostIP}. Theoretically term may contain free 
     variables before and after it's execution. Once @var{Debug} is ground
     there is just one (out of two) possible modes: debug-enabled or d
     debug-disabled.".

:- pred rpr_call_host(HostIP, Term, Debug): gnd * var * gnd => 
                                            gnd * gnd * gnd #
    "Execute @var{Term} on @var{HostIP}. Term may contain free 
     variables before and have them ground after. Once @var{Debug} is ground
     there is just one (out of two) possible modes: debug-enabled or d
     debug-disabled.".

:- pred rpr_call_host(HostIP, Term, Debug): gnd * gnd * gnd => 
                                            gnd * gnd * gnd #
    "Execute @var{Term} on @var{HostIP}. Once @var{Term} is ground it remains
     ground after execution. Once @var{Debug} is ground
     there is just one (out of two) possible modes: debug-enabled or d
     debug-disabled.".

:- pred rpr_call_host(HostIP, Term, Debug): gnd * var * var => 
                                            gnd * var * gnd #
    "Same as first case. If @var{Debug} is free - 
     non-debug behavior is assumed.".

:- pred rpr_call_host(HostIP, Term, Debug): gnd * var * var => 
                                            gnd * gnd * gnd #
    "Same as second case. If @var{Debug} is free - 
     non-debug behavior is assumed.".

:- pred rpr_call_host(HostIP, Term, Debug): gnd * gnd * var => 
                                            gnd * gnd * gnd #
    "Same as third case. If @var{Debug} is free - 
     non-debug behavior is assumed.".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rpr_clear_nodes(node_id(HostId,HostIP)):-
         retractall_nb(node_id(HostId,HostIP)).

:- doc(rpr_clear_nodes/1,
           "Erase some (possibly all) entries from module-specific DB. 
            Technically a straight-forward wrapper around 
            @pred{retractall_nb/1}.").

:- pred rpr_clear_nodes(NodeId): dht_rpr_node_id #
    "Erase exact entries form module-specific DB.".

:- pred rpr_clear_nodes(NodeId): dht_rpr_node => dht_rpr_node_id #
    "Erase entries that match a given (by @var{NodeId}) pattern.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%retract_nb(Term):-
%      retract_fact_nb(Term).

%:- doc(retract_nb/1,
%           "Hand-made non-blocking 'retract'. Basically contains two cases:
%            @begin{enumerate}
%            @item{} @var{Term} is call-able. In this case erase it.
%            @item{} Otherwise succeed.
%            @end{enumerate}").

%:- pred retract_nd(Term): gnd => gnd #
%    "Look for exact term in module-specific DB.".

%:- pred retract_nd(Term): var => gnd #
%    "Look for a predicate matching given (by @var{Term}) pattern.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

retractall_nb(Term):-
      current_fact_nb(Term)->
      retractall_fact(Term)
      ;
      true.

:- doc(retractall_nb/1,
           "Hand-made non-blocking 'retractall'. Basically contains two same
            cases:@begin{enumerate}
            @item{} @var{Term} is call-able. In this case erase all it's 
                    occurrences.
            @item{} Otherwise succeed.
            @end{enumerate}").
/*
:- pred retractall_nd(Term): gnd => gnd #
    "Look for exact term in module-specific DB.".

:- pred retractall_nd(Term): var => gnd #
    "Look for a predicates matching given (by @var{Term}) pattern.".
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

