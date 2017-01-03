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

:- package(clpfd_debug).

:- doc(title, "Debug Package for CLP(FD)").

% Enable/disable debug code for CLP(FD)
%
%   The default compiler is not very smart removing unused code and
%   its dependencies. I.e., a call like 'debug("big string")' can be
%   costly even with debug(_).
%
%   This is the reason why it is better to disable predicates using
%   source translations (e.g., 'debugpred' package).
%
%   -- Jose F. Morales

:- use_package(library(clpfd/clpfd_options)).

% Use the 'debugpred' package to enable/disable debug
% predicates.
:- use_package(debugpred).
:- debugpred clpfd_debug/2.

:- if(defined(debug_clpfd_runtime)).
:- use_module(library(clpfd/clpfd_debug_rt)).
:- debugpredstatus(on).
:- else.
:- use_module(library(clpfd/clpfd_debug_rt), [clpfd_error/2]).
:- debugpredstatus(off).
:- endif.


