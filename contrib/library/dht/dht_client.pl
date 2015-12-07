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
:- module(dht_client,
         [
          dht_connect/2,
          dht_connect/3,
          dht_disconnect/1,
          dht_consult_b/4,
          dht_consult_nb/4,
          dht_extract_b/4,
          dht_extract_nb/4,
          dht_store/4,
          dht_hash/3
         ], [
          assertions,regtypes,isomodes
         ]).

:- doc(title, "Top-level user interface to DHT").
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

:- doc(module, "This module contains just a top-level interface to
           the utilities provided by the DHT system in Ciao. The
           'philosophy' of the current approach is that most details
           are hidden behind simple read/write primitives and that the
           handler is exposed to the client for a concrete DHT. By
           doing things this way we expect to preserve the simplicity
           of DHT usage while providing the freedom of being able to
           switch between various instances of the Ciao DHT system").

:- use_module(library(sockets), [connect_to_socket/3]).
% :- use_module(library(concurrency)).

% :- use_module(dht_config).
:- use_module(dht_misc).

:- concurrent dht_connection/2.
:- doc(dht_connection/2,
           "Simple way to store internal reference to particular DHT.
            First argument is bound to IP/DNS add res, second to 
            read-write open stream to that address.").

:- regtype dht_connection_type(DHTConnection) #
   "@var{DHTConnection} is a predicate of type @pred{dht_connection/2}, where
    first argument is of type @pred{atm/1} and second of type 
    @pred{streams_basic:stream/1}".

dht_connection_type(DHTConnection):-
      DHTConnection = dht_connection(Address, Stream),
      atm(Address),
      stream(Stream).

:- doc(dht_connect/2, 
           "Connect to the DHT specified by @var{Server} (IP address).").

:- pred dht_connect(Server, Connection): atm * var => gnd * dht_connection_type #

    "Perform a straightforward connection from the client-side of
     a DHT node. The information about DHT node to connect to is 
     supplied as @var{Server}. It could equally be a DNS name 
     or an IP address.".

dht_connect(Server, dht_connection(Server, Stream)):-
        atm(Server),
        connect_to_socket(Server, 3228, Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(dht_connect/3, 
           "Connect to the DHT specified by @var{Server} (IP address).").

:- pred dht_connect(Server, Port, Connection): atm * int * var =>
                                               atm * int * dht_connection_type #

    "Perform a straightforward connection from the client-side of
     a DHT node. The information about DHT node to connect to is 
     supplied as combination of @var{Server} and @var{Port}, if a 
     non standart port is used for server-to-client communication. 
     It could equally be a DNS name or an IP address.".

dht_connect(Server, Port, dht_connection(Server, Stream)):-
        atm(Server), integer(Port),
        connect_to_socket(Server, 3228, Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(dht_disconnect/1,
           "Disconnect from DHT, identified by special supplied connection.").

:- pred dht_disconnect(Connection): dht_connection_type #

    "Issues 'end_of_file' token to the stream supplied as @var{Connection}
     and closes it without delay.".

dht_disconnect(dht_connection(Address, Stream)):-
        dht_connection_type(dht_connection(Address, Stream)),
        write_pr(Stream, 'end_of_file'),
        close(Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(dht_consult_b/4,
           "Look either for exact predicate or predicate matching the
            pattern in DHT.").

:- pred dht_consult_b(Conn, Key, Value, Resp):
    dht_connection_type * atm * term * var => 
    dht_connection_type * atm * term * gnd # 
    "@pred{dht_consult/4} performs a lookup in the DHT represented by 
     @var{Conn} (see @pred{dht_connect/2}) and searches for @var{Value}
     previously associated with the @var{Key}. @var{Value} may be 
     partially instantiated in which case matching against the tuples
     stored in DHT is performed.".

dht_consult_b(dht_connection(Address, Stream), Key, Value, Response):-
        dht_connection_type(dht_connection(Address, Stream)),
        ground(Key),
        write_pr(Stream, dht_consult_b(Key, Value)),
        read_pr(Stream, Response).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred dht_consult_nb(Connection, Key, Value, Response):
    dht_connection_type * atm * term * var => 
    dht_connection_type * atm * term * gnd # 
    "@pred{dht_consult/4} performs a lookup in the DHT represented by 
     @var{Conn} (see @pred{dht_connect/2}) and searches for @var{Value}
     previously associated with the @var{Key}. @var{Value} may be 
     partially instantiated in which case matching against the tuples
     stored in DHT is performed.".

dht_consult_nb(dht_connection(Address, Stream), Key, Value, Response):-
        dht_connection_type(dht_connection(Address, Stream)),
        ground(Key),
        write_pr(Stream, dht_consult_nb(Key, Value)),
        read_pr(Stream, Response).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(dht_extract_b/4,
           "Extract from DHT an exact predicate of type @var{Key}(@var{Value})
            or one that matches given pattern.").

:- pred dht_extract_b(Connection, Key, Value, Response): 
    dht_connection_type * atm * term * var => 
    dht_connection_type * atm * term * gnd # 
    "This predicate extracts information from the DHT connected to by @var{Connection}, 
     that is stored under key @var{Key} if it matches the  pattern supplied 
     as @var{Value}".

dht_extract_b(dht_connection(Address, Stream), Key, Value, Response) :-
        dht_connection_type(dht_connection(Address, Stream)),
        ground(Key),
        write_pr(Stream, dht_extract_b(Key, Value)),
        read_pr(Stream, Response).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(dht_extract_nb/4,
           "Extract from DHT an exact predicate of type @var{Key}(@var{Value})
            or one that matches given pattern.").

:- pred dht_extract_nb(Connection, Key, Value, Response):
    dht_connection_type * atm * term * gnd #  
    "This predicate extracts information from the DHT connected to by @var{Connection}, 
     that is stored under key @var{Key} if it matches the  pattern supplied 
     as @var{Value}".

dht_extract_nb(dht_connection(Address, Stream), Key, Value, Response) :-
        dht_connection_type(dht_connection(Address, Stream)),
        ground(Key),
        write_pr(Stream, dht_extract_nb(Key, Value)),
        read_pr(Stream, Response).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_store(dht_connection(_Server, Stream), Key, Value, Response):-
        ground(Stream),ground(Key),
        write_pr(Stream, dht_store(Key, Value)),
        read_pr(Stream, Response).

:- doc(dht_store/4,
           "Store data to DHT in form of @var{Key}(@var{Value}) predicate. 
            No free variables are allowed in predicate.").

:- pred dht_store(Connection, Key,Value, Response): 
    dht_connection_type * atm * term * var => dht_connection_type * atm * term * gnd # 
    "The value provided in @var{Value} is stored under a key given as @var{Key}
     inside the DHT mentioned as @var{Connection}".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_hash(dht_connection(Address, Stream), Value, Hash):-
        dht_connection_type(dht_connection(Address, Stream)),
        write_pr(Stream, dht_hash(Value)),
        read_pr(Stream, Hash).

:- doc(dht_hash/3, 
           "Get value of hash function for a given term.").

:- pred dht_hash(Connection, Value, Hash): 
    dht_connection_type * term * var => dht_connection_type * term * int #
    "Get  (in @var{Hash}) the hash of @var{Value} as 
     determined by the DHT pointed to by @var{Connection}. Implemented 
     mostly for testing purposes.".

:- pred dht_hash(Connection, Value, Hash): 
    dht_connection_type * term * int => dht_connection_type * term * int #
    "Check whether @var{Hash} is equal to the hash of @var{Value} as 
     determined by the DHT pointed to by @var{Connection}. Implemented 
     mostly for testing purposes.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
