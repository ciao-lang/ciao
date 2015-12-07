%% ---------------------------------------------------------------------------
%% This file is part of the clpfd package for Ciao
%%
%% Copyright (C) 2006-2012 CLIP Group
%%
%% Authors
%%   * Remy Haemmerle
%%   * Emilio Jesús Gallego Arias
%%   * Jose F. Morales
%%
%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version 2
%% of the License, or (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
%% ---------------------------------------------------------------------------

% TODO: sup and inf are not well handled in labeling and fd_optim.
% TODO: Does this support negative integers? Does the brigde bench needs them?

:- module(fd_range_bits_unsafe,
	[
	    fd_range_type/1,
	    fd_range_bound_t/1,
	    fd_range_t/1,

	    new/3,
	    default/1,

	    is_singleton/1,
	    singleton_to_bound/2,
            bound_const/2,

	    enum/2,
	    next_in_dom/3,

	    get_domain/2,
	    get_domain_term/2,

	    min/2,
	    max/2,
	    size/2,

	    bound_add/3,
	    bound_sub/3,
	    bound_mul/3,
	    bound_div/3,

	    range_add/3,
	    range_sub/3,
	    range_mul/3,

	    intersect/3,
	    union/3,
	    complement/2,

	    remove/3,

	    in_range/2
	%	 portray/3
	],
	[assertions, regtypes,  fsyntax, dcg]).

:- doc(title, "Range Handling").

:- use_module(library(clpfd/fd_utils), [min/3, max/3, clpfd_error/2]).
:- use_module(library(clpfd/int_extra), [lsb/2, msb/2, bits_set/2]).

:- use_module(library(lists), [last/2, append/3]).
:- use_module(library(between), [between/3]).


fd_range_type(prolog_bits_unsafe).
fd_range_t(_).
fd_range_bound_t(_).

mymax(inf, X) := X :-!.
mymax(X, inf) := X :-!.
mymax(sup, _) := sup:-!.
mymax(_, sup) := sup:-!.
mymax(X, Y) := ~(fd_utils:max(X,Y)).

:- pred default(fd_range_t).
default := -1.

:- pred new(+int, +int, -fd_range_t). 
new(sup, sup, 0) :- !, fail.
new(I, I, X) :- !, 
	X is 1 << I.
new(Min, sup, X) :-!,
	X is  \ ((1 << ~mymax(Min,0)) - 1). 
new(Min, Max, X) 
	:- Min =< Max, !,
	X is (((1 << (~mymax(Max,-1) +1)) -1) # ((1 << ~mymax(Min,0)) - 1)).
new(_, _, 0).

is_singleton(X) :-
	min(X, C),
	max(X, C).

singleton_to_bound(X, C) :-
	min(X, C),
	max(X, C).

% TODO: Not entirely right.
bound_const(sup, sup).
bound_const(inf, 0).
bound_const(X, X).

:- pred non_empty(+fd_range_t).
non_empty(X):-
	X =\= 0.

min(X, M):-
	X \= 0,
	int_extra:lsb(X, M).

max(X, M):-
	X < 0, !,
	M = sup.
max(X, M):-
	X \= 0,
	int_extra:msb(X, M).

%% Gets the size of the range.
size(0, M) :- !, M = 0.
size(X, M):-
	X < 0, !,
	M = 999. % Emilio: use this hack for optim.
% TODO:% M = sup.
size(X, M):-
	int_extra:bits_set(X, M).


bound_add(sup, _, sup) :- !.
bound_add(_, sup, sup) :- !.
bound_add(X, Y, Res) :-
	Res is X + Y.

bound_sub(sup, _, sup) :- !.
bound_sub(_, sup, sup) :- !.
bound_sub(X, Y, Res) :-
	Res is X - Y.

bound_mul(sup, _, sup) :- !.
bound_mul(_, sup, sup) :- !.
bound_mul(X, Y, Res) :-
	Res is X * Y.

bound_div(X, Y, Res) :-
	Res is X // Y.

range_add(X, I, Y) :- Y is X << I.

range_sub(X, I, Y) :- Y is X >> I.

range_mul(_X, _I, _) :-
	throw(not_implemented(fd_range_bits:mul/3)).

intersect(X, Y, Z) :-
	Z is X /\ Y.

union(X, Y, Z) :-
	Z is X \/ Y.

complement(X, Z) :-
	Z is \ X.

remove(X, I, Y) :-
	Y is  X /\ \(1 << I).

in_range(I, X) :-
	(X /\ (1<< I)) =\= 0.

get_domain(_X) := _ :- 
	throw(not_implemented(fd_range_bits:get_domain/2)).

get_domain_term(X, '..'(Min,Max)) :-
	min(X, Min), 
	max(X, Max).

intervals(_, _) :-
	throw(not_implemented(fd_range_bits:intervals/2)).	

%% This is used mainly for labelling, selects all the values of a range.
:- pred enum(-fd_range_t, -int).

enum(_X) := _ :-
	throw(not_implemented(fd_range_bits:enum/2)).

:- pred next_in_dom(+, +fd_range_t, -int).
% fail for singleton
next_in_dom(up, X) := ~min(X):-true.
% TODO: next_in_dom(down, X) := sup :-
next_in_dom(down, X) := 999 :-
	X < 0, !,
	throw(error(representation_error, fd_range_bits:next_in_dom/3)).
next_in_dom(down, X) := ~max(X).
