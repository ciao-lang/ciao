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

% A package for writing indexicals.

:- package(indexicals).

:- include(library(clpfd/indexicals_ops)).
:- use_module(library(clpfd/fd_range)).
:- use_module(library(clpfd/fd_term)).

:- if((defined(optim_comp), backend(js_backend))).
% TODO: fix for JS-backend (removes module qualification)
:- load_compilation_module(library(clpfd/indexicals_tr)).
:- add_sentence_trans(indexicals_tr:translate/3, 340).
:- add_term_trans(indexicals_tr:translate_fix/3, 340).
:- clpfd_flag(atom_based). % do not use module expansion tricks
:- else.
:- load_compilation_module(library(clpfd/indexicals_tr)).
:- add_sentence_trans(indexicals_tr:translate/3, 340).
:- endif.

%:- multifile clpfd_call_wrapper/1.
