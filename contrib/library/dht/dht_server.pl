% This library is bent to be a prolog DHT implementation
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
:- module(dht_server, 
          [
            dht_server/1,
            dht_prolog/1
          ],[
            assertions,regtypes,isomodes
          ]).

:- use_module(library(sockets), [hostname_address/2]).
:- use_module(library(system), [current_host/1]).
:- use_module(library(format), [format/2]).

:- use_module(dht_config).
:- use_module(dht_logic_misc, [consistent_hash/2, hash_size/1]).
:- use_module(dht_s2s).
:- use_module(dht_s2c).

:- doc(title, "Top-level interface to a DHT server").
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
:- doc(module, "").

:- regtype dht_arguments_list(DHTArguments) #
   "@var{DHTArguments} is associated with simple list, 
    that represents pairs of command line arguments 
    (argument and its value). Possible values are:
   @tt{--join-host}, @tt{--hash-power}, @tt{--server-id}, 
   @tt{--s2c-port}, @tt{--s2c-threads}, @tt{--s2s-port},
   @tt{--s2s-threads}. All of the arguments (except 
   @tt{--join-host}) accept integer values. In case of
   @tt{--join-host} value of argument should be equal 
   to IP/DNS address of host running a copy of DHT.".

dht_arguments_list([]).
dht_arguments_list([A,B|Tail]):-
      dht_argument(A,B),
      dht_arguments_list(Tail).

dht_argument(A,B):-
      atm(A),atm(B),
      A = '--join-host'.
dht_argument(A,B):-
      atm(A),atm(B),
      A = '--hash-power'.
dht_argument(A,B):-
      atm(A),atm(B),
      A = '--server-id'.
dht_argument(A,B):-
      atm(A),atm(B),
      A = '--s2c-port'.
dht_argument(A,B):-
      atm(A),atm(B),
      A = '--s2c-threads'.
dht_argument(A,B):-
      atm(A),atm(B),
      A = '--s2s-port'.
dht_argument(A,B):-
      atm(A),atm(B),
      A = '--s2s-threads'.
      

dht_server(Arguments):-
      set_default_values,
      process_arguments(Arguments),
      process_server_id,
      print_arguments,
      dht_s2c_main,
      dht_s2s_main.

:- doc(dht_server/1,
           "@pred{main/1}: start the DHT server. Here is a step-by-step
            bahavior:@begin{enumerate}
            @item{} set all values of @file{dht_config.pl} module to default.
            @item{} modify those, for which command-line arguments were
                    supplied.
            @item{} modify server number separately, since it depends on
                    two command-line arguments : @tt{--server-id} and 
                    @tt{--hash-power}.
            @item{} output resulting values to terminal.
            @item{} start client side communication by executing 
                    @pred{dht_s2c:dht_s2c_mian/0}.
            @item{} start server side communication by executing 
                    @pred{dht_s2s:dht_s2s_mian/0}.
            @end{enumerate}").

:- pred dht_server(Arguments): dht_arguments_list #
    "".

set_default_values:-
      dht_set_s2s_port(31337),
      dht_set_s2s_threads(20),
      dht_set_s2c_port(3228),
      dht_set_s2c_threads(20),
      dht_set_hash_power(20),
      current_host(Host),
      hostname_address(Host, HostIP),
      dht_set_server_host(HostIP).
        

process_arguments([]).
process_arguments([A,B|Tail]):-
      get_arguments(A,B),
      process_arguments(Tail).

get_arguments(Name, AValue):-
      ground(Name), ground(AValue), 
      atom_number(AValue, Value),
      '--s2c-threads' = Name,
      format("Modifying s2c-threads~n",[]),
      dht_set_s2c_threads(Value),
      !.
get_arguments(Name, AValue):-
      ground(Name), ground(AValue),
      atom_number(AValue, Value),
      '--s2c-port' = Name,
      format("Modifying s2c-port~n",[]),
      dht_set_s2c_port(Value),
      !.
get_arguments(Name, AValue):-
      ground(Name), ground(AValue),
      atom_number(AValue, Value),
      '--s2s-threads' = Name,
      format("Modifying s2s-threads~n",[]),
      dht_set_s2s_threads(Value),
      !.
get_arguments(Name, AValue):-
      ground(Name), ground(AValue),
      atom_number(AValue, Value),
      '--s2s-port' = Name,
      format("Modifying s2s-port~n",[]),
      dht_set_s2s_port(Value),
      !.
get_arguments(Name, AValue):-
      ground(Name), ground(AValue),
      atom_number(AValue, Value),   
      '--hash-power' = Name,
      format("Modifying hash-power~n",[]),
      dht_set_hash_power(Value),
      !.
get_arguments(Name, AValue):-
      ground(Name), ground(AValue),
      atom_number(AValue, Value),
      '--server-id' = Name,
      format("Modifying server-id~n",[]),
      dht_set_server_id(Value),
      !.
get_arguments(Name, Value):-
      ground(Name), ground(Value),
      '--join-host' = Name,
      format("Modifying join-host~n",[]),
      dht_set_join_host(Value),
      !.
get_arguments(_Name, _Value):-
      print_usage, 
      fail.

process_server_id:-
      dht_server_id(ServerId) ->
      modify_server_id(ServerId)
      ;
      init_server_id.

modify_server_id(Id):-
      dht_logic_misc:hash_size(HashSize),
      ModifiedId is mod(integer(Id), HashSize),
      dht_set_server_id(ModifiedId).

init_server_id:-
      dht_server_host(ServerHost),
      dht_logic_misc:consistent_hash(ServerHost, Hash),
      dht_set_server_id(Hash).
       
print_usage:-
      format("
Usage: dht_server [options]
      Possible options are:
      --s2c-port <value> - set port for \"server to client\" communication.
      --s2c-threads <value> - set amount of threads for \"server to client\"
                              communication. Default value: 20.
      --s2s-port <value> - set port for \"server to server\" communication.
      --s2s-threads <value> - set amount of threads for \"server to server\"
                              communication. Default value: 20.
      --server-id <value> - override default ID value. Default value is
                            retrieved by hashing the IP address of a node.
                            Moreover if value passed is bigger than
                            maximum of nodes possible, this value would
                            be trimmed to fit into DHT-ring.
      --hash-power <value> - set value of power, that is used to calculate
                             size of DHT-ring. Size is calculated as
                             2^<hash-power-value>.
      --join-host <ip-or-dns-address> 
                            points to the other server that is also running
                            Ciao DHT system. Keep in mind that ports for
                            \"server to server\" communication _must_ be
                            equal on every server.
",[]).

print_arguments:-
      dht_s2s_port(S2SPort),
      dht_s2s_threads(S2SThreads),

      dht_s2c_port(S2CPort),
      dht_s2c_threads(S2CThreads),

      hash_power(HashPower),
     
      dht_server_id(ServerId),

      format("
Using following arguments:
s2s-port - ~p
s2s-threads - ~p~n
s2c-port - ~p
s2c-threads - ~p~n
hash-power - ~p~n
server-id - ~p~n", [S2SPort, S2SThreads, S2CPort, 
                    S2CThreads, HashPower, ServerId]).

dht_prolog(Parameters):-
      set_default_values,
      process_parameters(Parameters),
      process_server_id,
      print_arguments,
      dht_s2c_main,
      dht_s2s_main.

:- doc(dht_prolog/1, 
           "Another style to launch the DHT server. All the parameters
            are passed in the list. Members of the list are of type
            @tt{prameter_name(parameter_value)}, where 
            @tt{parameter_name} corresponds to name of the command line
            argument without two leading dashes (@tt{--}) and with internal 
            dash replaced by the underscore (@tt{_}). All the
            values are of integer type, except the value of @tt{join_host},
            which should be an atom.").

process_parameters([]).
process_parameters([Parameter|Tail]):-
      get_parameter(Parameter),
      process_parameters(Tail).

get_parameter(s2s_port(Port)):-
      ground(Port),
      dht_set_s2s_port(Port),
      !.
get_parameter(s2s_threads(Threads)):-
      ground(Threads),
      dht_set_s2s_threads(Threads),
      !.
get_parameter(s2c_port(Port)):-
      ground(Port),
      dht_set_s2c_port(Port),
      !.
get_parameter(s2c_threads(Threads)):-
      ground(Threads),
      dht_set_s2c_threads(Threads),
      !.
get_parameter(server_id(ID)):-
      ground(ID),
      dht_set_server_id(ID),
      !.
get_parameter(hash_power(HashPower)):-
      ground(HashPower),
      dht_set_hash_power(HashPower),
      !.
get_parameter(join_host(Host)):-
      ground(Host),
      dht_set_join_host(Host),
      !.
get_parameter(_WhateverElse). %just ignore it

      
