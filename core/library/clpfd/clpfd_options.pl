%% ---------------------------------------------------------------------------
%% This file is part of the clpfd package for Ciao
%%
%% Copyright (C) 2006-2012 CLIP Group
%%
%% Written by:
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

:- package(clpfd_options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Compile-time options for the CLP(FD) package
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Imperative assignment emulation

% Uses setarg/3 for imperative assignment.
% Comment to use attributed variables.
:- if(\+ (defined(optim_comp), backend(js_backend))).
:- compilation_fact(fd_use_setarg).
:- endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Range selection.

% TODO: I'd like to do this, but it seems is too fancy for condcomp.
% :- compilation_fact(fd_use_range(intervals)).
% :- compilation_fact(fd_use_range(bits)).
% :- compilation_fact(fd_use_range(finite_intervals)).

%:- compilation_fact(fd_use_range_finite_intervals).
:- compilation_fact(fd_use_range_intervals).
% :- compilation_fact(fd_use_range_bits).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Uncomment to enable collection of statistics
% :- compilation_fact(collect_statistics).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Uncomment to enable debug mode in the translation code
% :- compilation_fact(debug_translation).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Uncomment to enable debug trace messages at runtime
% :- compilation_fact(debug_clpfd_runtime).

