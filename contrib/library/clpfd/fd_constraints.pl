%% ---------------------------------------------------------------------------
%% This file is part of the clpfd package for Ciao
%%
%% Copyright (C) 2006-2013 CLIP Group
%%
%% Originally written by:
%%   * Emilio Jesus Gallego Arias
%%
%% Modified by:
%%   * Remy Haemmerle
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

:- module(fd_constraints,
	[x_in_range/3,
	 x_in_y/2,
	 'a+b=c'/3,
	 'a+t=c'/3,
	 'a-b=c'/3,
	 'a-t=c'/3,
	 'a=b*t'/3,
	 'a=b*c'/3,
	 'a=b'/2,
	 'a=t'/2,
	 'a=b'/2,
	 'a=<b'/2,
	 'a=<t'/2,
	 't=<b'/2,
	 'a<b'/2,
	 'a<t'/2,
	 't<b'/2,
	 'a<>b'/2,
	 'a<>t'/2,
	 'a<>b+c'/3,
	 'a<>b-c'/3,
	 'a<>b+t'/3,
	 'a<>b-t'/3, 
	 fd_all_different/1
	],
	[assertions, regtypes]).

:- use_package(library(clpfd/indexicals)).

:- doc(title,   "Basic Indexicals for the Finite Domain Library").

:- doc(author,  "Emilio Jes@'{u}s Gallego Arias"). %
:- doc(license, "GPL"). 

:- doc(module,  "This module provides basic term manipulation.").

% Constant range.
x_in_range(X, Min, Max) +:
	X in c(Min)..c(Max).

x_in_y(X, Y) +:
	X in dom(Y).

% Example from German on how to select the modes, this needs much more work.
% :- trust 'a+b=c'(A,B,C) : integer(B) => 'a+b=c'(A,B,C) equiv 'a+t=c'(A,B,C).
%% KK :- trust pred 'a+b=c'(A,B,C) : integer(B) + equiv('a+t=c'(A,B,C)).
%% KK :- calls 'a+t=c'(A,B,C) : integer(B).
%% KK :- pred 'a+t=c'(A,B,C) : integer(B).

% Experimental support for auto-compiling.
:- new_declaration(shape/2).
:- shape('a+b=c', A+B=C).

% Adds three numbers.
'a+b=c'(X, Y, Z) +:
	X in min(Z)-max(Y) .. max(Z)-min(Y),
	Y in min(Z)-max(X) .. max(Z)-min(X),
	Z in min(X)+min(Y) .. max(X)+max(Y).

'a+t=c'(X, Y, Z) +:
	X in min(Z)-c(Y) .. max(Z)-c(Y),
	Z in min(X)+c(Y) .. max(X)+c(Y).

% Subtraction
%% Note (MCL): isn't it easier to put it in terms of addition?
'a-b=c'(X, Y, Z) +:
	X in min(Z)+min(Y) .. max(Z)+max(Y),
	Y in min(X)-max(Z) .. max(X)-min(Z),
	Z in min(X)-max(Y) .. max(X)-min(Y).

'a-t=c'(X, Y, Z) +:
	X in min(Z)+c(Y) .. max(Z)+c(Y),
	Z in min(X)-c(Y) .. max(X)-c(Y).

%% Cannot do pointwise ranges :/
'a=b*t'(X,Y,C) +:
	X in min(Y)*c(C)..max(Y)*c(C),
	Y in min(X)/c(C)..max(X)/c(C).

%% Cannot do pointwise ranges :/
'a=b*c'(X,Y,Z) +:
	X in min(Y)*min(Z)..max(Y)*max(Z),
 	Y in min(X)/max(Z)..max(X)/min(Z),
	Z in min(X)/max(Y)..max(X)/min(Y).

%	X in dom(Y)*c(C),
%% Umm this could be 
%%      Y in dom(X) //c(C). so if dom(X) = [1,4],[8,10], C= 2 the res wb [1,2], [4,5]
%	Y in {val(X)//c(C)}.

% X < Y
'a=<b'(X, Y) +:
	X in c(inf) .. max(Y),
	Y in min(X) .. c(sup).

'a=<t'(X, Y) +:
	X in c(inf) .. c(Y).

't=<b'(X, Y) +:
	Y in c(X) .. c(sup).

'a<b'(X, Y) +:
 	X in c(inf) .. max(Y)-c(1),
 	Y in min(X)+c(1) .. c(sup).

'a<t'(X, Y) +:
 	X in c(inf) .. c(Y)-c(1).

't<b'(X, Y) +:
 	Y in c(X)+c(1) .. c(sup).

% Indexicals optimized for queens.
'a<>b+c'(X, Y, Z) +:
	X in -{val(Y)+val(Z)},
	Y in -{val(X)-val(Z)},
	Z in -{val(X)-val(Y)}.

'a<>b-c'(X, Y, Z) +:
	X in -{val(Y)-val(Z)},
	Y in -{val(X)+val(Z)},
	Z in -{val(Y)-val(X)}.

% Super optimized ones.
'a<>b+t'(X, Y, Z) +:
	X in -{val(Y)+c(Z)},
	Y in -{val(X)-c(Z)}.

'a<>b-t'(X, Y, Z) :-
	'a<>b+t'(Y, X, Z).

%  - is not monotonic, so it doesn't work ok.
% diff(X, Y) :+
%	X in -dom(Y).
'a<>b'(X, Y) +:
	X in -{val(Y)},
	Y in -{val(X)}.

%'a<>t'(X, Y) +:
%	X in -{c(Y)}.
% Optimization
'a<>t'(X, Y):-
	fd_term:prune(X, Y).

'a=b'(X, Y) +:
	X in dom(Y),
	Y in dom(X).

'a=t'(X, Y) +:
	X in {c(Y)}.

fd_all_different([]).
fd_all_different([X|Xs]) :-
	diff_with_list(X,Xs),
	fd_all_different(Xs).

diff_with_list(_X,[]).
diff_with_list(X, [Y|Ys]) :-
	'a<>b'(X,Y),
	diff_with_list(X,Ys).


%% Example:
%% x_in_range(X, 1, 200), x_in_range(Y, 90, 1000), 'a<=b'(X, Y), Y = 123, 'a+b=c'(X,Y,Z), X = 3, Z=123.
%% domain([X,Y,Z], 1,10), 'a<b'(X,Y), 'a<b'(Y,Z), 'a+b=c'(X,Y,Z), labeling([X,Y,Z]).
