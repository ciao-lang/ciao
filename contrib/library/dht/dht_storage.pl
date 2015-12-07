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
:- module(dht_storage, 
         [ 
          dht_store/3,
          dht_extract_b/2,
          dht_extract_nb/2,
          dht_consult_b/2,
          dht_consult_nb/2,
          dht_key_hash/2
         ],[
          assertions,regtypes,isomodes
         ]).

:- doc(title, "Underlying data-storage module").
:- doc(subtitle, "Ciao DHT implementation reference manual").
:- doc(copyright, "This library is developed by Arsen Kostenko as part
           of Ciao Prolog system and is distributed under GNU Lesser 
           General Public License. Consider COPYING file in source package
           of visit following Internet address: 
           @href{http://www.gnu.org/licenses/lgpl.html} for complete text of the 
           license.").

:- doc(summary, "This is just a single module of the whole Ciao DHT 
           subsystem, and should be used mostly by developers (not
           casual users) of the system.").

:- doc(author, "Arsen Kostenko").

:- doc(module, "This module contains very low-level utilities of data 
   storage specific to a single node. Neither data sharing nor remote 
   invocation is performed at this level. Note that no dedicated data 
   manipulation is performed neither on this level. Also keep in mind that
   concept of @var{Key} should always meet the same constraints as the second 
   argument of @pred{functor/3} predicate. Important decision taken at this 
   point is representation of all information stored in the DHT as usual Prolog
   facts. This behavior is implemented by representing relations of form
   @var{Key} -> @var{Value} in form of @var{Key}(@var{Value}) facts.").


:- use_module(library(concurrency)).
:- concurrent key_stored/3.


dht_store(Key, KeyHash, Value):-
      functor(To_Store, Key, 1),
      concurrent(Key/1),
      arg(1, To_Store, Value),
      assertz_fact(To_Store),
      accumulate_key(Key, KeyHash).

:- doc(dht_store/3, 
           "This predicate stores information into the local (module-local) 
            database. The three arguments stand for
            key, value, and value of hash-function, when applied to the key. 
            The third argument is needed for auxiliary database, which is 
            used by reverse lookup.").

:- pred dht_store(Key, KeyHash, Value): atm * int * gnd # 
    "The value provided in @var{Value} is stored under a key given as 
     @var{Key}.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

accumulate_key(Key, KeyHash):-
     ground(Key),ground(KeyHash),
     current_fact_nb(key_stored(Key, KeyHash, Amount)),
     AmountPlus1 is Amount+1,
     retractall_fact(key_stored(Key,KeyHash,Amount)),
     assertz_fact(key_stored(Key,KeyHash,AmountPlus1)),
     !.
accumulate_key(Key, KeyHash):-
     ground(Key),ground(KeyHash),int(KeyHash),
     assertz_fact(key_stored(Key,KeyHash,1)),
     !.

:- doc(accumulate_key/2, 
           "This predicate performs accumulation of data in auxiliary database.
            Every time a new predicate is added, auxiliary database is modified
            to represent number of predicates present at the moment.").

:- pred accumulate_key(Key, KeyHash): gnd * int #
    "Predicate @pred{accumulate_key} checks for presence of pair supplied as 
     @var{Key} and @var{KeyHash} in an auxiliary database. If it is found there
     corresponding DB entry is modified by increasing third parameter by one,
     otherwise a third parameter is assigned to 1, and resulting predicate 
     is stored in DB.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%dht_require(Key, Value):-
%      functor(To_Require, Key, 1),
%      arg(1, To_Require, Value),
%      retract_predicate(To_Require).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_extract_b(Key, Value):-
      functor(To_Retrieve, Key, 1),
      arg(1, To_Retrieve, Value),
      retract_fact(To_Retrieve),
      reduce_key(Key).

:- doc(dht_extract_b/2, 
           "Looks up local database for fact of following type @var{Key}/1, 
            that takes @var{Value} as argument. The pair is extracted from 
            the database if there is at lease one match. Otherwise call is 
            blocking once the fact is not found instantly.").

:- pred dht_extract_b(Key, Value): atm * gnd => atm * gnd  #
      "Search for exact fact.".
:- pred dht_extract_b(Key, Value): atm * var => atm * gnd  #
      "Search for fact matching pattern. Pattern is supplied as @var{Value}.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%retract_predicate(ToRetract):-
%      functor(ToRetract, Key, _Args),
%      retract_fact(ToRetract),
%      reduce_key(Key).
%
%:- doc(retract_predicate/1,     
%           "@pred{retract_predicate} is a straight-forward wrapping around 
%            common @pred{retract_fact/1} predicate with previous application
%            of @pred{functor/3} predicate and subsequent call to 
%            @pred{reduce_key/1}.").
%
%:- pred retract_predicate(ToRetract): gnd => gnd #
%    "Search DB for exact combination of @var{Key}/@var{Value}".
%:- pred retract_predicate(ToRetract): var => gnd #
%    "Search DB for combination of @var{Key}/@var{Value} matching pattern.
%     Pattern is supplied as @var{Value}.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_extract_nb(Key, Value):-
      functor(To_Retrieve, Key, 1),
      arg(1, To_Retrieve, Value),
      retract_fact_nb(To_Retrieve),
      reduce_key(Key).

:- doc(dht_extract_nb/2,
           "Looks up the local database for a fact of type @var{Key}/1 
            that takes @var{Value} as argument. If a corresponding fact is 
            found in database, it is extracted from it. With respect to 
            behavior of @pred{dht_extract_b/2}, this predicated has a useful 
            difference - it does not block while searching.").

:- pred dht_extract_nb(Key, Value): atm * gnd => atm * gnd # 
    "Check that combination of @var{Key}/@var{Value} appears in database.".
:- pred dht_extract_nb(Key, Value): atm * var => atm * gnd # 
    "Search for combination of @var{Key}/@var{Value} matching pattern. 
     Pattern is supplied as @var{Value}.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%retract_predicate_nb(ToRetract):-
%      functor(ToRetract, Key, _Args),
%      retract_fact_nb(ToRetract),
%      reduce_key(Key).
%
%:- doc(retract_predicate_nb/1,
%           "@pred{retract_predicate_nb/1} does two simple things, first of all 
%            checks for existence of predicate supplied as @var{ToRetract} by 
%            executing @pred{call/1}. If anything is found - it is retracted via
%            call to @pred{retract_predicate/1}. Otherwise, 
%            @pred{retract_predicate_nb/1} defaults to true/success.").
%
%:- pred retract_predicate_nb(ToRetract): gnd => gnd #
%    "Search DB for exact combination of @var{Key}/@var{Value}".
%:- pred retract_predicate_nb(ToRetract): var => gnd #
%    "Search DB for combination of @var{Key}/@var{Value} matching pattern. 
%     Pattern is supplied as @var{Value}.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(dht_consult_b/2, 
           "Looks up local database for combination of @var{Key}/@var{Value} of following type @var{Key}/1 that
            takes @var{Value} as argument. The combination of @var{Key}/@var{Value} is only looked up. If there
            is no corresponding combination of @var{Key}/@var{Value} in local database, @pred{dht_consult_b/2} 
            blocks until such fact is inserted. Nothing is done neither to combination of @var{Key}/@var{Value} 
            nor to local database.").

:- pred dht_consult_b(Key, Value): atm * gnd => atm * gnd # 
    "Search for exact combination of @var{Key}/@var{Value}.".
:- pred dht_consult_b(Key, Value): atm * gnd => atm * gnd # 
    "Search for combination of @var{Key}/@var{Value} matching pattern. 
     Pattern is supplied as @var{Value}.".

dht_consult_b(Key, Value):-
      functor(To_Consult, Key, 1),
      arg(1, To_Consult, Value),
      current_fact(To_Consult).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(dht_consult_nb/2, 
           "Looks up local database for combination of 
            @var{Key}/@var{Value} of following type @var{Key}/1 that takes
            @var{Value} as argument. The combination of @var{Key}/@var{Value}
            is only looked up - no modifications are done in any case (whether
            a match was found or not). As in case with @pred{dht_extract_nb/2},
            this predicate does not block while searching for matching 
            values.").

:- pred dht_consult_nb(Key, Value): atm * gnd => atm * gnd # 
    "Search for exact combination of @var{Key}/@var{Value}.".
:- pred dht_consult_nb(Key, Value): atm * gnd => atm * gnd # 
    "Search for combination of @var{Key}/@var{Value} matching pattern. 
     Pattern is supplied as @var{Value}.".

dht_consult_nb(Key, Value):-
      functor(To_Consult, Key, 1),
      arg(1, To_Consult, Value),
      current_fact_nb(To_Consult).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dht_key_hash(Key, KeyHash):-
      current_fact_nb(key_stored(Key, KeyHash, Amount)),
      Amount > 0.

:- doc(dht_key_hash/2, 
           "A general read-interface to auxiliary database, in order to perform
            reverse search: 'get set of keys by corresponding value of hash
            function'.").

:- pred dht_key_hash(Key, KeyHash): var * var => gnd * int # 
    "The most general case (when both arguments are free) is searching for any
     key-hash pair stored in auxiliary database. ".

:- pred dht_key_hash(Key, KeyHash): gnd * var => gnd * int # 
    "Here @var{Key} is ground, which in turn leads to search for information on
     concrete predicate.".

:- pred dht_key_hash(Key, KeyHash): var * int => gnd * int # 
    "On the contrary to previous example, this one has only second argument 
     @var{KeyHash} ground. Therefore, this type of call would search for keys, 
     that where mapped into given value of hash-function.".

:- pred dht_key_hash(Key, KeyHash): gnd * int # 
    "Finally, calling @pred{dht_key_hash} with both arguments ground is similar
     to straight-forward check on auxiliary database, or to asking a question:
     ``Does auxiliary database has any information on this key, which is mapped
     into that hash value?''".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reduce_key(Key):-
      ground(Key),
      current_fact_nb(key_stored(Key, KeyHash, 1)),
      retractall_fact(key_stored(Key, KeyHash, 1)),
      !.
reduce_key(Key):-
      ground(Key),
      current_fact_nb(key_stored(Key, KeyHash, Amount)),
      ground(Amount), int(Amount),
      Amount > 1,
      AmountMinus1 is Amount - 1,
      retractall_fact(key_stored(Key, KeyHash, Amount)),
      assertz_fact(key_stored(Key, KeyHash, AmountMinus1)).

:- doc(reduce_key/1,
           "This is another write-interface to auxiliary DB. Difference between
            this and previous predicate is that this time data from auxiliary
            DB is reduced and/or extracted.").

:- pred reduce_key(Key): gnd #
    "@pred{reduce_key} is a usual way to store variables in dynamic DB context
     a predicate @pred{key_stored} is holding amount of times a predicate was
     stored under certain key. Once the amount reaches zero, information about
     that particular key disappears from database.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
