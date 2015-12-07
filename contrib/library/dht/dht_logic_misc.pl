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
:- module(dht_logic_misc, 
         [
          hash_size/1,
          highest_hash_number/1,
          consistent_hash/2,
          next_on_circle/2,
          not_in_circle_oc/3,
          in_circle_oo/3,
          in_circle_oc/3
         ], [
          assertions,regtypes,isomodes
         ]).

:- doc(title, "Various wrappers for DHT logics module").
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
:- doc(module, "This module contains miscellaneous predicates related
           to the @file{dht_logic.pl} module. Mostly various 
           calculation-wrappers.").

:- use_module(library(indexer/hash), [hash_term/2]).

:- use_module(dht_config).

hash_size(Size):-
        hash_power(Power),
        Size is integer(2**Power).


:- doc(hash_size/1, 
           "This predicate calculates @tt{(2**m)}, where 'm' is equal
            to @pred{hash_power/1} received from system wide 
            configurations stored in @file{dht_config.pl}.").

:- pred hash_size(Size): var => {gnd, int} #
    "The only purpose of @pred{hash_size/1} predicate is
     to get corresponding value from system wide configurations 
     stored in @file{dht_config.pl} file and convert it into the 
     number of nodes virtually available in the current DHT 
     installation.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

highest_hash_number(N):-
      hash_size(Size),
      N is Size-1.

:- doc(highest_hash_number/1, 
           "This predicate calculates @tt{(2**m)-1}, where 'm' is 
            equal to @pred{hash_power/1} received from system wide 
            configurations stored in @file{dht_config.pl}.").

:- pred highest_hash_number(N): int #
    "If used with a ground argument @pred{highest_hash_number/1} simply checks
     whether the value supplied corresponds to the biggest hash number possible
     in the DHT. ".

:- pred highest_hash_number(N): var => int #
    "Otherwise (if argument happens to be a free variable) the value of
     @var{N} is bound to the highest hash number possible in the DHT.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

consistent_hash(Term, Hash):-
      ground(Term), var(Hash),
      hash_term(Term, TermHash),
      hash_size(HashSize),
      Hash is mod(integer(TermHash), HashSize).

:- doc(consistent_hash/2, 
           "This kind of computation is usually performed every time, when one
            needs to get value of hash-function.").


:- pred consistent_hash(Term, DHTHash): term * var => term * int # 
    "Straightforward computation of hash (@var{DHTHash}) on 
     the basis of @var{Term} supplied.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

next_on_circle(Id, 0):-
      ground(Id),
      highest_hash_number(Num),
      Id = Num,
      !.
next_on_circle(Id, Num):-
      ground(Id),
      highest_hash_number(Max),
      Id \= Max,
      Num is Id+1.

:- doc(next_on_circle/2, 
           "Performs the calculation  of @tt{(id+1) mod ((2**m)-1)}, where @tt{id} 
            is the first argument supplied, and @tt{m} is equal to @pred{hash_power/1}.").

:- pred next_on_circle(Id, Num): int * var => int * int # 
    "@pred{next_on_circle/2} is a calculation wrapping. One can only use it 
     with first argument ground and equal to integer value. What is more, 
     that value @bf{MUST} be kept within certain bounds: value of  @var{Id} 
     @bf{MUST} meet the constraints 0<=@var{Id}<=@var{HighNum}, where @var{HighNum} is retrieved
     via @pred{highest_hash_number/1} predicate.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

not_in_circle_oc(Id, Start, End) :-
      ground(Start), ground(End), ground(Id),
      int(Start), int(End),int(Id),
      Start > End,
      highest_hash_number(Highest),
      not_in_circle_oc(Id, Start, Highest), % id not_in (a, 2**(m-1)]
      not_in_interval_cc(Id, 0, End),   % id not_in [0, b]
      !.
not_in_circle_oc(Id, Start, End):-
      ground(Start), ground(End),ground(Id),
      int(Start), int(End),int(Id),
      Start < End,
      Id =< Start,
      !.
not_in_circle_oc(Id, Start, End):-
      ground(Start), ground(End),ground(Id),
      int(Start), int(End),int(Id),
      Start < End,
      Id > End.

:- doc(not_in_circle_oc/3, 
           "@pred{not_in_circle_oc/3}: the 'oc' suffix stands for 
            \"open-closed\" interval. The predicate implements the behavior 
            of the expression: @tt{Id not_in (a,b]}. where both 'a' and 'b' 
            are numbers/identifiers on the DHT-ring.").

:- pred not_in_circle_oc(Id, Start, End): int * int * int # 
    "Yet another wrapper around simple calculations. All arguments are
     expected to be ground, by the time the predicate is called. 
     The predicate checks whether the value supplied as @var{Id} fits 
     into circle sector defined by values of @var{Start} and @var{End}.
     If the condition is not true - the whole predicate fails.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

in_circle_oo(Id, Start, Start):-
      ground(Start), ground(Id),
      int(Start), int(Id),
      Id \= Start,
      !.
in_circle_oo(Id, Start, End):-
      ground(Start), ground(End),ground(Id),
      int(Start), int(End),int(Id),
      Start > End,
      highest_hash_number(N),
      in_interval_oc(Id, Start, N),
      !.
in_circle_oo(Id, Start, End):-
      ground(Start), ground(End),ground(Id),
      int(Start), int(End),int(Id),
      Start > End,
      in_interval_co(Id, 0, End),
      !.
in_circle_oo(Id, Start, End):-
      ground(Start), ground(End),ground(Id),
      int(Start), int(End),int(Id),
      Start < End,
      Id < End,
      Id > Start.

:- doc(in_circle_oo/3, 
           "@pred{in_circle_oo/3}: the 'oo' suffix stands for \"open-open\" 
            circle interval. The predicate implements the behavior of 
            following expression: @tt{Id in (a,b)}.").

:- pred in_circle_oo(Id, Start, End): int * int * int # 
    "Check whether value of @var{Id} fits into sector defined by
     @var{Start} and @var{End} excluding both, fail otherwise.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

in_circle_oc(Id, Start, End):-
      ground(Start), ground(End),
      int(Start), int(End),
      Id = End,
      !.
in_circle_oc(Id,Start,End):-
      in_circle_oo(Id, Start, End).

:- doc(in_circle_oc/3, 
           "@pred{in_circle_oc/3}: the 'oc' suffix stands for \"open-closed\" 
            interval. The predicate is based on @pred{in_circle_oo/3} and 
            considers the interval to be closed on the right. This predicate
            implements the behavior of the expression: @tt{Id in (a,b]}").

:- pred in_circle_oc(Id,Start,End):int * int * int # 
    "Check whether value of @var{Id} fits into sector defined by
     @var{Start} and @var{End} excluding first and including second.
     Fail if the condition does not hold.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(not_in_interval_cc/3,
           "@pred{not_in_interval_cc/3}'cc' suffix stands for closed-closed 
            interval and should imitate behavior of following expression: 
            @tt{id not_in [a,b]}.").

:- pred not_in_interval_cc(Id, Start, End): int * int * int #
    "Check whether value of @var{Id} fits outside interval defined by
     @var{Start} and @var{End} including both.".

not_in_interval_cc(Id, Start, End):-
      ground(Start), ground(End),
      int(Start), int(End),
      Id < Start,
      !.      
not_in_interval_cc(Id, Start, End):-
      ground(Start), ground(End),
      int(Start), int(End),
      Id > End.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

in_interval_co(Id, Start, End):-
      ground(Id), ground(Start), ground(End),
      int(Id),int(Start),int(End),
      Id < End,
      Id >= Start.

:- doc(in_interval_co/3,
           "@pred{in_interval_co/3} 'co' suffix stands for closed-open interval 
            and should imitate behavior of following expression: 
            @tt{id in [a,b)}.").

:- pred in_interval_co(Id, Start, End):int * int * int # 
    "Check whether value of @var{Id} fits into interval defined by
     @var{Start} and @var{End} including first and excluding second".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

in_interval_oc(Id, Start, End):-
      ground(Id), ground(Start), ground(End),
      int(Id),int(Start),int(End),
      Id =< End,
      Id > Start.

:- doc(in_interval_oc/3, 
           "@pred{in_interval_oc/3} 'oc' suffix stands for \"open-closed\" interval 
            and should imitate behavior of following expression: 
            @tt{id in (a,b]}.").

:- pred in_interval_oc(Id, Start, End): int * int * int # 
    "Check whether value of @var{Id} fits into interval defined by
     @var{Start} and @var{End} excluding first and including second".



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
:- doc(in_interval_cc/3, 
           "@pred{in_interval_cc/3} 'cc' suffix stands for closed-closed 
            interval and should imitate behavior of following expression: 
            @tt{id in [a,b]}.").

:- pred in_interval_cc(Id, Start, End): int * int * int # 
    "Check whether value of @var{Id} fits into interval defined by
     @var{Start} and @var{End} including both.".

in_interval_cc(Id, Start, End):-
      ground(Id), ground(Start), ground(End),
      int(Id),int(Start),int(End),
      Id =< End,
      Id >= Start.
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

