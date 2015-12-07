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
:- module(dht_config, 
          [
           hash_power/1,
           dht_set_hash_power/1,
           dht_s2c_port/1,
           dht_set_s2c_port/1,
           dht_s2c_threads/1,
           dht_set_s2c_threads/1,
           dht_s2s_port/1,
           dht_set_s2s_port/1,
           dht_s2s_threads/1,
           dht_set_s2s_threads/1,
           dht_join_host/1,
           dht_set_join_host/1,
           dht_server_id/1,
           dht_set_server_id/1,
           dht_server_host/1,
           dht_set_server_host/1
          ], [
           assertions,regtypes,isomodes
          ]).

%:- dynamic dht_config_option/2.

:- doc(title, "Configuration module").
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

:- doc(module, "This is the initial system-wide configuration storage 
           module. All the configurations stored here are represented by 
           command-line arguments or corresponding terms, depending on
           the server launching style.").

%:- use_module(library(concurrency)).
:- concurrent dht_config_option/2.

hash_power(Power):-
      current_fact_nb(dht_config_option('hash-power', Power)).

:- doc(hash_power/1, 
           "Get/check power of currently running DHT. Hash function has
            @tt{(2**m)} values, therefore @pred{hash_power/1} 
            returns/checks @tt{m}.").

:- pred hash_power(Power): var => int #
    "Bound @var{Power} to value of currently running DHT power.".

:- pred hash_power(Power): int #
    "Check whether @var{Power} is equal to power of the currently running 
     DHT.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_set_hash_power(Power):-
      integer(Power),
      retractall_nb(dht_config_option('hash-power', _)),
      assertz_fact(dht_config_option('hash-power', Power)).

:- doc(dht_set_hash_power/1, 
           "Set initial power of hash function.").

:- pred dht_set_hash_power(Power): int #
    "Set @tt{m} for @tt{(2**m)} formula.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_s2c_port(Port):-
      current_fact_nb(dht_config_option('s2c-port', Port)).

:- doc(dht_s2c_port/1, "Get/check server to client communication port.").

:- pred dht_s2c_port(Port): var => int #
    "Set value of @var{Port} to currently used port number for 
     server-to-client communication.".

:- pred dht_s2c_port(Port): int #
    "Check whether value of @var{Port} is equal to currently used
     port number for server-to-client communication.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_set_s2c_port(Port):-
      integer(Port),
      retractall_nb(dht_config_option('s2c-port', _)),
      assertz_fact(dht_config_option('s2c-port', Port)).

:- doc(dht_set_s2c_port/1,"Set port for server-to-client communication.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_s2c_threads(Threads):-
      current_fact_nb(dht_config_option('s2c-threads', Threads)).

:- doc(dht_s2c_threads/1,"Get/check number of threads for 
           server-to-client communication.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_set_s2c_threads(Threads):-
      retractall_nb(dht_config_option('s2c-threads', _)),
      assertz_fact(dht_config_option('s2c-threads', Threads)).

:- doc(dht_set_s2c_threads/1,"Set number of threads for 
           server-to-client communication.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_s2s_port(Port):-
      current_fact_nb(dht_config_option('s2s-port', Port)).

:- doc(dht_s2s_port/1, "Get/check server-to-server communication port.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_set_s2s_port(Port):-
      retractall_nb(dht_config_option('s2s-port', _)),
      assertz_fact(dht_config_option('s2s-port', Port)).

:- doc(dht_set_s2s_port/1,"Set server-to-server communication port.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_s2s_threads(Threads):-
      current_fact_nb(dht_config_option('s2s-threads', Threads)).

:- doc(dht_s2s_threads/1,"Get/check number of threads for 
           server-to-server communication.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_set_s2s_threads(Threads):-
      retractall_nb(dht_config_option('s2s-threads', _)),
      assertz_fact(dht_config_option('s2s-threads', Threads)).

:- doc(dht_set_s2s_threads/1,"Set number of threads for 
           server-to-server communication.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_join_host(Host):-
      current_fact_nb(dht_config_option('join-host', Host)).

:- doc(dht_join_host/1, 
           "Get/check address of the host to join.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_set_join_host(Host):-
      retractall_nb(dht_config_option('join-host', _)),
      assertz_fact(dht_config_option('join-host', Host)).

:- doc(dht_set_join_host/1,"Set address of the host to join.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_server_id(Id):-
      current_fact_nb(dht_config_option('server-id',Id)).

:- doc(dht_server_id/1,"Get/check node number of current server.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_set_server_id(Id):-
      retractall_nb(dht_config_option('server-id',_)),
      assertz_fact(dht_config_option('server-id',Id)).

:- doc(dht_set_server_id/1,"Set node number of current server.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_server_host(Host):-
      current_fact_nb(dht_config_option('server-host',Host)).

:- doc(dht_server_host/1,"Get/check address of the current server").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_set_server_host(Host):-
      retractall_nb(dht_config_option('server-host', _)),
      assertz_fact(dht_config_option('server-host', Host)).

:- doc(dht_set_server_host/1,"Set address of the current server").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

retractall_nb(Fact):-
      current_fact_nb(Fact)->
      ground(Fact),
      retractall_fact(Fact)
      ;
      true.

:- doc(retractall_nb/1,
           "Hand-made non-blocking @pred{retractall_fact/1}. Basically contains two 
            simple cases:@begin{itemize}
            @item{} @var{Fact} is call-able. In this case erase all it's 
                    occurrences.
            @item{} Otherwise succeed.
            @end{itemize}").

/*
:- pred retractall_nd(Fact): gnd => gnd #
    "Look for exact term in module-specific DB.".

:- pred retractall_nd(Fact): var => gnd #
    "Look for a predicates matching given (by @var{Fact}) pattern.".
*/