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

:- package(clpfd). 

%% Operators.
:- include(library(clpfd/clpfd_ops)).

%% MCL: Every user-level constraint is a goal, so it is easier to
%% perform the translation goal by goal.  We may return a conjunction
%% of goals, but the compiler is happy with that.
:- load_compilation_module(library(clpfd/clpfd_tr)).
:- add_goal_trans(clpfd_tr:trans_fd/2, 750). % TODO: Right priority?

%% Runtime predicates.
:- use_module(library(clpfd/clpfd_rt)).
:- use_module(library(clpfd/fd_term), [new/1]).
:- use_module(library(clpfd/fd_constraints)).
% :- use_module(library(clpfd/clpfd_optim)).
