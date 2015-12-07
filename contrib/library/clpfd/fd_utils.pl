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

:- module(fd_utils,
	[
	    min/3,
	    max/3,
	    member_ro/2,
	    clpfd_error/2
	], [assertions]).

:- doc(title, "Auxiliary Predicates for the FD Implementation").

% TODO: include as a builtin? (JFMC)
max(X, Y, X):- X > Y, !.
max(X, Y, Y):- X =< Y.

min(X, Y, X):- X < Y, !.
min(X, Y, Y):- X >= Y.

% TODO: not implemented in Ciao?
member_ro(X, [Y|_]) :-
	X == Y.
member_ro(X, [_|Xs]) :-
	member_ro(X, Xs).


clpfd_error(A, B):-
	throw(error(A, B)).
