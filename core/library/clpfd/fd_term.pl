%% ---------------------------------------------------------------------------
%% This file is part of the clpfd package for Ciao
%%
%% Copyright (C) 2006-2012 CLIP Group
%%
%% Originally written by:
%%   * Emilio Jesús Gallego Arias
%%
%% Modified by:
%%   * Rémy Haemmerlé
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

:- module(fd_term,
	[
	    fd_term_t/1,

	    max/2,
	    min/2,
	    bounds/3,
	    size/2,
	    dom/2,
	    dom_term/2,
	    range/2,

	    is_singleton/1,
	    integerize/2,

	    in/2,
	    new/1,
	    var_id/2,

	    prune/2,
	    tell_range/2,
	    % delay_val/2,
	    %
	    add_propag/3,

	    next_in_dom/3,
	    enum/2
	],
	[assertions, regtypes, fsyntax, dcg]).

:- include(library(clpfd/fd_ops)).

:- use_package(library(clpfd/clpfd_debug)).

:- use_module(library(clpfd/fd_var)).
:- use_module(library(clpfd/fd_range)).
:- use_module(library(clpfd/fd_pchains)).
:- use_module(library(clpfd/clpfd_stats)).

%% FD solver Core.

%% This part is pure, but relies in an imperative fd_range and fd_var
%% implementation.

%% Finite Domain terms.
:- regtype fd_term_t/1  # "Finite Domain Objects, representing a free/ground FD variable".

fd_term_t := ~fd_var_t  % # "A FD variable"
          |  ~int. % # "A FD integer"

:- pred min(+fd_term_t, -fd_int_t) #
	"Returns the minimum value a FD term may take. Not complete.".
min(X, Min):-
	fd_var:get_range(X, Range), !,
	fd_range:min(Range, Min).
min(X, Min) :-
	% trust(fd_int_t(X)), !,
	Min = X.

:- pred max(+fd_term_t, -fd_int_t) #
	"Returns the maxium value a FD term may take. Not complete.".
max(X, Max) :-
	fd_var:get_range(X, Range), !,
	fd_range:max(Range, Max).
max(X, Max) :-
	% trust(fd_int_t(X)), !,
	Max = X.

:- pred bounds(+fd_term_t, -fd_int_t, -fd_int_t) #
	"Returns current bounds for a FD term. Not complete.".
bounds(X, Min, Max) :-
	fd_var:get_range(X, Range), !,
	fd_range:min(Range, Min),
	fd_range:max(Range, Max).
bounds(X, Min, Max) :-
	% trust(fd_int_t(X)), !,
	Min = X, Max = X.

:- pred size(+fd_term_t, -fd_int_t) #
	"Returns the size of the range of a FD term".
size(X, Size):-
	fd_var:get_range(X, Range), !,
	fd_range:size(Range, Size).
size(_X, Size):-
	% trust(fd_int_t(_X)), !,
	Size = 1.

:- pred dom(+fd_term_t, -idx_dom_t) #
	"".
dom(X, Dom):-
	fd_var:get_range(X, Range), !,
	fd_range:get_domain(Range, Dom).
dom(X, Dom):-
	% trust(fd_int_t(X)), !,
	Dom = [X].

:- pred dom_term(+fd_term_t, -term).
dom_term(X, Dom):-
	fd_var:get_range(X, Range), !,
	fd_range:get_domain_term(Range, Dom).
dom_term(X, Dom):-
	% trust(fd_int_t(X)), !,
	Dom = X.

:- pred range(+fd_term_t, -fd_range).
range(X, Range) :- 
	fd_var:get_range(X, Range), !.
range(X, Range) :-
	% trust(integer(X)), !,
	fd_range:new(X, X, Range).

enum(X, E):-
	fd_var:get_range(X, Range),!,
	fd_range:enum(Range, E).
enum(X, E):-
	% trust(integer(X)), !
	X = E.

% fails for constante
next_in_dom(Order, X, Next):-
	fd_var:get_range(X, Range),
	fd_range:next_in_dom(Order, Range, Next).

is_singleton(X) :- int(X), !.
is_singleton(X) :- !, fd_var:get_range(X, R), fd_range:is_singleton(R).

integerize(X, Y) :-
	int(X), !, X = Y.

integerize(X, Y) :- !,
	fd_var:get_range(X, Range),
	fd_range:singleton_to_bound(Range, Y).

% :- pred delay_val(+fd_term_t, -int).
% delay_val(X, V) :-
% 	fd_var:get_range(X, R), !,
% 	(
% 	    fd_range:singleton_to_int(R, V) -> 
% 	    true
% 	;
% 	    clpfd_error("Aggg, ground-need analysis failed at compile time: ~n", [])
% 	).
% delay_val(X, V) :- 
% 	% trust(integer(X)), !,
% 	X = V.

:- pred in(+fd_term_t, +idx_dom_t).
in(X, Dom):-
	( 
	    make_range(Dom, Range) ->
	    tell_range(X, Range)
	;
	    clpfd_error("in/2 expected range found ~p", [Dom])
	).

make_range(I .. J) := ~(fd_range:new(I, J)) :- !, integer(I), integer(J).
make_range(R1 \/ R2) := ~(fd_range:union(~make_range(R1), ~make_range(R2))) :- !.
make_range(I) := ~(fd_range:new(I, I)) :- !, integer(I).

:- pred new(+fd_term_t).
new := ~(fd_var:default).


:- pred var_id(+fd_var, -int).
var_id(Var) :=  ~(fd_var:get_id(Var)).


:- pred prune(+fd_term_t, +int).
prune(X, I):-
	fd_var:get_range(X, VarRange), !,
	(
	    fd_range:remove(VarRange, I, NewRange) ->
	    inc_stat(var_tell_succ),
	    tell_range_and_wakeups(X, VarRange, NewRange)
	;
	    inc_stat(var_tell_fail), 
	    fail
	).
prune(X, I):-
	% trust(integer(X)), !, 
	(
	    X \= I  ->
	    inc_stat(int_tell_succ)
	;
	    inc_stat(int_tell_fail), 
	    fail
	).


:- pred tell_range(+fd_term_t, +fd_range).
tell_range(X, TellRange):-
%DEBUG	clpfd_debug("Telling ~p in ~p~n", [~fd_var:get_id(X), Range]),
	fd_var:get_range(X, VarRange), !,
	(
	    fd_range:intersect(VarRange, TellRange, NewRange) ->
	    inc_stat(var_tell_succ),
	    tell_range_and_wakeups(X, VarRange, NewRange)
	;
	    inc_stat(var_tell_fail), 
	    fail
	).
tell_range(X, Range):-
	% trust(integer(X)), !, 
	(
	    fd_range:in_range(X, Range) ->
	    inc_stat(int_tell_succ)
	;
	    inc_stat(int_tell_fail), 
	    fail
	).


%% THIS IS THE MAIN HOTSPOT

%% Note that we only support linear wakeup, this is what execute
%% does. Lots os possibilities to improve that.

:- pred tell_range_and_wakeups(+fd_var, +fd_range, +fd_range).
tell_range_and_wakeups(_X, VarRange, NewRange) :-
	VarRange == NewRange, !.
tell_range_and_wakeups(X, VarRange, NewRange) :-
	fd_var:set_range(X, NewRange),
	fd_range:min(NewRange, Min),
	fd_range:max(NewRange, Max),
	fd_var:get_propags(X, Chains),
	(
	    Min = Max ->
	    execute(val, Chains)
	    % fd_pchains:empty(EC),
	    % fd_var:set_propags(X, EC) % Save memory? Use with care
	;
	    true
	),
	execute(dom, Chains),
	(
	    fd_range:min(VarRange, Min) ->
	    true
	;
	    execute(min, Chains)
	),
	(
	    fd_range:max(VarRange, Max) ->
	    true
	;
	    execute(max, Chains)
	).

add_propag(X, Type, Const) :-
	fd_var:get_propags(X, Chains), !,
	fd_pchains:add(Type, Chains, Const).
add_propag(_Type, _X, _Const) :-
	% trust(integer(_X)), !,
	true.


