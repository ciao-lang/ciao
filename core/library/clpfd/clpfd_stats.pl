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

:- module(clpfd_stats, 
	[inc_stat/1,
	 get_stat/2,
	 clpfd_stats/0
	],[assertions]).

:- doc(title, "Statistics for CLP(fd)").

:- use_package(library(clpfd/clpfd_options)).

:- use_module(library(format)).

:- if(defined(collect_statistics)).
% (Enabled collect statistics)

:- data fd_stats/2.

fd_stats(chain_calls, 0).
fd_stats(var_tell_succ, 0).
fd_stats(var_tell_fail, 0).
fd_stats(int_tell_succ, 0).
fd_stats(int_tell_fail, 0).

inc_stat(Stat) :-
	retract_fact(fd_stats(Stat, N)),
	N1 is N + 1,
	assertz_fact(fd_stats(Stat, N1)).

get_stat(Stat, R) :-
	retract_fact(fd_stats(Stat, R)),
	assertz_fact(fd_stats(Stat, 0)).

:- use_module(library(aggregates)).

clpfd_stats :-
	stats_list(L),
	print_stats(L).

stats_list(L) :-
	findall(S, fd_stats(S, _), L).

print_stats([]).
print_stats([S|Sl]) :-
	get_stat(S, N),
	format(user_error, "~p value: ~p~n", [S,N]),
	print_stats(Sl).

:- else.
% (Disabled collect statistics)

inc_stat(_).

get_stat(_, 0).

clpfd_stats:-
        format(user_error, 
               "CLP(FD) statistics not turned on at compile time.~n", []),
        format(user_error, "See the `clpfd_options' file for a list of compile time options.~n", []).
:- endif.

