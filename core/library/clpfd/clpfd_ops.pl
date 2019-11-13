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

%% FD operator definitions. Should be compatible with Sicstus clpfd lib.

%:- op(760, yfx, #<=>).
%:- op(750, xfy, #=>).
%:- op(750, xfy, #<=).
%:- op(740, yfx, #\/).
%:- op(740, yfx, #\).
%:- op(720, yfx, #/\).
%:- op(710, fy,  #\).
:- op(700, xfx, in).
%:- op(700, xfx, in_set).
:- op(700, xfx, [#=, #\=, #<, #=<, #>, #>=]).
:- op(550, xfx, ..).
