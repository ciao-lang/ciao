%% ---------------------------------------------------------------------------
%% This file is part of the clpfd package for Ciao
%%
%% Copyright (C) 2006-2013 CLIP Group
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

:- module(fd_pchains,
	[
	    fd_pchains_t/1,
	    fd_pchain_type_t/1,

	    empty/1,
	    add/3,
	    execute/2
	],
	[assertions, regtypes, fsyntax]).

% TODO: [Emilio] Should't we call this file fd_chains.pl?
:- doc(title, "Constraint Propagation Chains").

:- use_package(library(clpfd/clpfd_options)).

:- use_module(clpfd_stats, [inc_stat/1]).

:- regtype fd_pchains_t/1.

% TODO: The problem here is with the ~g hack.
fd_pchains_t(_).

% A goal is a callable
% fd_propags_t(Chain) :-
% 	list(Chain).

:- regtype fd_pchain_t/1.
fd_pchain_t(_).

:- regtype fd_pchain_type_t/1.
fd_pchain_type_t := val | dom | min | max.

%% This is just an implementation for a list of efficient imperative
%% closures. Some features missing:

%% - Detection of duplicate closures/constraints.
%% - Different order of execution/storage than linear/lists.

% TODO: Report and fix bug in printing cyclic terms so we can get rid
% of this hack.

:- if(defined(fd_use_setarg)).

:- use_module(library(odd), [setarg/3]).

:- pred empty(-fd_pchains_t) # "Empty propagation chain".
empty(propagator([],[],[],[])).

:- pred add(+fd_pchains_type_t, +fd_pchains_t, +callable) # "Add a constraint to a chain".
add(min, Chains, Goal) :- Chains = propagator(C,_,_,_), odd:setarg(1, Chains, [Goal|C]).
add(max, Chains, Goal) :- Chains = propagator(_,C,_,_), odd:setarg(2, Chains, [Goal|C]).
add(dom, Chains, Goal) :- Chains = propagator(_,_,C,_), odd:setarg(3, Chains, [Goal|C]).
add(val, Chains, Goal) :- Chains = propagator(_,_,_,C), odd:setarg(4, Chains, [Goal|C]).

:- pred execute(+fd_pchains_type_t, +fd_pchains_t) # "Propagate constraints in a given chain".
execute(min, propagator(C,_,_,_)) :- propagate(C).
execute(max, propagator(_,C,_,_)) :- propagate(C).
execute(dom, propagator(_,_,C,_)) :- propagate(C).
execute(val, propagator(_,_,_,C)) :- propagate(C).

:- else.

:- use_module(engine(attributes)).

% The debugger does not handle well cyclic terms. Hence, we wrap
% propagators into attributes to cut cycles.
% Also usefull for js_backend

empty(propagator(C1,C2,C3,C4)):-
	attach_attribute(C1, []), 
	attach_attribute(C2, []), 
	attach_attribute(C3, []), 
	attach_attribute(C4, []).

add(min, propagator(C,_,_,_), Goal) :- get_attribute(C, C_), update_attribute(C, [Goal|C_]).
add(max, propagator(_,C,_,_), Goal) :- get_attribute(C, C_), update_attribute(C, [Goal|C_]).
add(dom, propagator(_,_,C,_), Goal) :- get_attribute(C, C_), update_attribute(C, [Goal|C_]).
add(val, propagator(_,_,_,C), Goal) :- get_attribute(C, C_), update_attribute(C, [Goal|C_]).

execute(min, propagator(C,_,_,_)) :- get_attribute(C, Goal), propagate(Goal).
execute(max, propagator(_,C,_,_)) :- get_attribute(C, Goal), propagate(Goal).
execute(dom, propagator(_,_,C,_)) :- get_attribute(C, Goal), propagate(Goal).
execute(val, propagator(_,_,_,C)) :- get_attribute(C, Goal), propagate(Goal).

:-endif. 

%% Closure Call mechanism
:- if(\+ (defined(optim_comp), backend(js_backend))).

:- use_module(engine(hiord_rt), ['$meta_call'/1]).

% TODO: Should we use a cut between chain calls? Surely there is space
% for optimization. We gain performance but I'm not sure about
% correctness.
propagate([]).
propagate([C|Cs]) :-
        inc_stat(chain_calls),
        '$meta_call'(C),         % (see goal_to_closure/3)
        propagate(Cs).

:- else.

% For the js_backend
propagate([]).
propagate([C|Cs]) :-
        call(C),
        propagate(Cs).

:- endif.









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% CRUFT %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: this must be done from scratch.

% Detection of duplicates.

% This is very costly in some cases :( as in calling all_different
% with a list of 200 Free Vars, so ATM we leave it disabled until I
% find a use case.



% :- compilation_fact(remove_dups).

% :- if((defined(remove_dups))). % TODO: not working in JS-backend (== only for atoms and vars)
% not_in_propagator(_Constraint, _):-!.
% not_in_propagator(_Constraint, []).
% not_in_propagator(Constraint, [C|Chain]) :-
% 	\+ Constraint == C,
% 	not_in_propagator(Constraint, Chain).
% :- else.
% not_in_propagator(_, _).
% :- endif.


