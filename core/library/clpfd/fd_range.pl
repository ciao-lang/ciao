%% ---------------------------------------------------------------------------
%% This file is part of the clpfd package for Ciao
%%
%% Copyright (C) 2006-2012 CLIP Group
%%
%% Authors
%%   * Remy Haemmerle
%%   * Emilio Jesés Gallego Arias
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
:- module(fd_range, [], [assertions, regtypes]).

%% Ciao's CLPFD library supports several ranges using this range
%% selection module.

%% Implementing new ranges may be very useful for particular
%% benchmarks.

%% Here we document the interface that a range implementation must
%% support.

:- pred fd_range_type(string) # "Name of the range module (for debug purposes)".

:- regtype fd_range_bound_t/1 #	"Type of a range bound. Used in min and max and operations".

:- regtype fd_range_t/1 # "Type of a range object".

:- pred default(-fd_range_t) # "Returns The Default Range".

:- pred new(+int, +int, -fd_range_t) #
	"Create a new FD range from two integers".

:- pred is_singleton(+fd_range_t) # "The range is a singleton".

:- pred singleton_to_bound(+fd_range_t, -fd_range_bound_t) # "If the range is a singleton, return its value".

:- pred min(+fd_range_t, -fd_range_bound_t) # "Get the lower bound of a range".

:- pred max(+fd_range_t, -fd_range_bound_t) # "Get the upper bound of a range".

:- pred size(+fd_range_t, -int) # "Number of elements in a range. Fails if infinite".

:- pred get_domain(+fd_range_t, -list(int)) # "List all integers in a Range".

%% Iterative selection of all the values on a range.
:- pred enum(-fd_range_t, -int) # "Returns on backtracking the list of get_domain".

:- pred next_in_dom(+, +fd_range_t, -int) # "Like enum, but with an additional parameter up/down)".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Operations on Ranges
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Bounds operations.
:- pred bound_const(+fd_const_spec, -fd_range_bound_t) # "Return a const bound from an indexical primitive".

:- pred bound_add(+fd_range_bound_t, +fd_range_bound_t, -fd_range_bound_t) # "Add two bounds".
:- pred bound_sub(+fd_range_bound_t, +fd_range_bound_t, -fd_range_bound_t) # "Substrats two bounds".
:- pred bound_mul(+fd_range_bound_t, +fd_range_bound_t, -fd_range_bound_t) # "Bound multiplicaton".
:- pred bound_div(+fd_range_bound_t, +fd_range_bound_t, -fd_range_bound_t) # "Bound division".

%% Pointwise operations.

:- pred range_add(+fd_range_t, +fd_range_bound_t, -fd_range_t) # "Pointwise addition to a range".
:- pred range_sub(+fd_range_t, +fd_range_bound_t, -fd_range_t) # "Pointwise substraction for ranges".
:- pred range_mul(+fd_range_t, +fd_range_bound_t, -fd_range_t) # "Pointwise multiplicaton for ranges".

%% Pointfree operations.

% TODO: Rename to intersection
:- pred intersect(+fd_range_t, +fd_range_t, -fd_range_t) # "Range Intersection".
:- pred union(+fd_range_t, +fd_range_t, -fd_range_t) # "Range Union".
:- pred complement(+fd_range_t, -fd_range_t) # "Range Complement".

:- pred remove(+fd_range_t, +int, -fd_range_t) # "Remove an integer from a range".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ranges Membership
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred in_range(+fd_range_t, +int) # "Range Membership".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ranges Selection
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_package(library(clpfd/clpfd_options)).

:- if(defined(fd_use_range_bits)).

 :- reexport(library(clpfd/fd_range_bits_unsafe)).

:- elif(defined(fd_use_range_intervals)).

 :- reexport(library(clpfd/fd_range_intervals)).

:- elif(defined(fd_use_range_finite_intervals)).

 :- reexport(library(clpfd/fd_range_finite_intervals)).

:- endif.
