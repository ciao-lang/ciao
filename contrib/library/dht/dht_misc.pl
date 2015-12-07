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
:- module(dht_misc, 
          [
           write_pr/2, read_pr/2
          ],[
           assertions,regtypes,isomodes
          ]).

:- doc(title, "Tiny module with miscellaneous functions").
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

:- doc(module, "This module holds just two predicates at the
           moment: @pred{write_pr/2} and @pred{read_pr}. In both of
           them the '_pr' suffix is standing for 'predicate', which in
           turn means that both of them are intended for
           transportation of predicates from one environment to
           another.").

:- use_module(library(fastrw)).

write_pr(Stream, Term):-
        fast_write(Stream, Term).

:- doc(write_pr/2, 
           "@pred{write_pr/2} is a straight-forward wrapping around the
            @pred{fast_write/2} predicate, without any checks on arguments
            The sole purpose is to allow usage of various ways of writing to
            streams without changing entire code of DHT.").

:- pred write_pr(Stream, Term): gnd * term #
    "Write the value of the @var{Term} into stream provided by @var{Stream}.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


read_pr(Stream, Pred):-
        fast_read(Stream, Pred).

:- doc(read_pr/2, 
           "Similarly to the previous predicate, this one serves currently 
            as a wrap around the @pred{fast_read/2} predicate, and was 
            implemented with the same purpose - to allow transparent 
            switching to different stream reading systems. ").

:- pred read_pr(Stream, Term): gnd * term #
    "Read stream represented by @var{Stream} looking for presence of pattern 
     given by @var{Term}. If none found, the predicate does not block. Result
     found may be non-fully ground.".

:- pred read_pr(Stream, Term): gnd * term => gnd * gnd #
    "Same as previous, with only modification - result found may be ground as
     well.".

:- pred read_pr(Stream, Term): gnd * gnd => gnd * gnd #
    "Scan stream @var{Stream} for presence of concrete (exact) predicate
     given by value of @var{Term}. @var{Term} is fully bound.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
