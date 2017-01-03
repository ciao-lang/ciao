%% ---------------------------------------------------------------------------
%% This file is part of the clpfd package for Ciao
%%
%% Copyright (C) 2012-2013 CLIP Group
%%
%% Originally written by:
%%   * Rémy Haemmerlé
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
:- module(fd_optim, [fd_minimize/2, fd_maximize/2], 
	            [assertions, fsyntax]).
:- doc(nodoc, assertions).

:- doc(title, "Optimization constraints").
:- doc(author, "R@'{e}my Haemmerl@'{e}").
:- doc(copyright,"@include{DocCopyright.lpdoc}").

:- doc(summary, "This module provides meta-predicates for optimizing constraints.").

:- include(.(clpfd_ops)).

:- use_module(library(clpfd/fd_term), [in/2, min/2, max/2]).
:- use_module(library(clpfd/fd_constraints), ['a=t'/2]).
%:- use_module(library(clpfd/fd_range), [default/1, min/2, max/2]).

:- meta_predicate(fd_minimize(:, ?)).
:- meta_predicate(fd_minimize_(:, ?, ?)).

:- pred fd_minimize(Goal, Var) # "Repeatedly calls @var{Goal} to find
a value that fd_minimizes the variable @var{X}. @var{Goal} is a Prolog
goal that should instantiate @var{X}, a common case being the use of
@pred{fd_labeling/2}. This predicate uses a branch-and-bound algorithm
with restart: each time @pred{call(Goal)} succeeds the computation
restarts with a new constraint @pred{X #< V} where @var{V} is the
value of @var{X} at the end of the last call of @var{Goal}. When a
failure occurs (either because there are no remaining choice-points
for @var{Goal} or because the added constraint is inconsistent with
the rest of the store) the last solution is recomputed since it is
optimal.".

fd_minimize(Goal, Var) :-
	new_cur_obj(J),
	catch(fd_minimize_(Goal, Var, J), 
	      E, 
	      (
		  del_cur_obj(J),
		  throw(E)
	      )),
	del_cur_obj(J).
				
fd_minimize_(_Goal, _Var, I):-
	set_cur_obj(I, 99999),
	% set_cur_obj(I, ~(fd_range:max(~(fd_range:default)))),
	fail.
:- if((defined(optim_comp), backend(js_backend))).
% TODO: the two versions are equivalent, but the JS-backend has
%       problems compiling the second one (fix)
fd_minimize_(Goal, Var, I):-
	repeat,
	get_cur_obj(I, Obj),
	Obj_ is Obj - 1,
	( 
	    fd_term:in(Var, 0..Obj_), call(Goal) -> 
	    fd_term:min(Var, Min),
	    set_cur_obj(I, Min),
	    fail
	; 
	    true
	),
	!, % (cuts 'repeat')
	fd_constraints:'a=t'(Var, Obj),
	call(Goal).
:- else.
fd_minimize_(Goal, Var, I):-
	repeat,
	get_cur_obj(I, Obj),
	Obj_ is Obj - 1,
	( Var in  0..Obj_, call(Goal) -> 
	    fd_term:min(Var, Min),
	    set_cur_obj(I, Min),
	    fail
	; !, % (cuts 'repeat')
	  fd_constraints:'a=t'(Var, Obj),
	  call(Goal)
	).
:- endif.


:- pred fd_maximize(Goal, Var) # "This predicate is similar to
@pred{fd_maximize(Goal, Var)} but @var{X} is maximized.".


:- meta_predicate(fd_maximize(:, ?)).
:- meta_predicate(fd_maximize_(:, ?, ?)).

fd_maximize(Goal, Var) :-
	new_cur_obj(J),
	catch(fd_maximize_(Goal, Var, J), 
	      E, 
	      (
		  del_cur_obj(J), 
		  throw(E)
	      )),
	del_cur_obj(J).
				
fd_maximize_(_Goal, _Var, I):-
	set_cur_obj(I, -9999),
	% set_cur_obj(I, ~(fd_range:min(~(fd_range:default)))),
	fail.
:- if((defined(optim_comp), backend(js_backend))).
% TODO: the two versions are equivalent, but the JS-backend has
%       problems compiling the second one (fix)
fd_maximize_(Goal, Var, I):-
	repeat,
	get_cur_obj(I, Obj),
	Obj_ is Obj - 1,
	( fd_term:in(Var,0..Obj_), call(Goal) -> 
	    fd_term:max(Var, Min),
	    set_cur_obj(I, Min),
	    fail
	; true
	),
	!, % (cuts 'repeat')
	fd_constraints:'a=t'(Var, Obj),
	call(Goal).
:- else.
fd_maximize_(Goal, Var, I):-
	repeat,
	get_cur_obj(I, Obj),
	Obj_ is Obj - 1,
	( Var in  0..Obj_, call(Goal) -> 
	    fd_term:max(Var, Min),
	    set_cur_obj(I, Min),
	    fail
	; !, % (cuts 'repeat')
	  fd_constraints:'a=t'(Var, Obj),
	  call(Goal)
	).
:- endif.

% ---------------------------------------------------------------------------
% Global non-backtrackable variables
% TODO: move to a separate module

:- if((defined(optim_comp), backend(js_backend))).

:- use_package(mutables). % (ptojs mutables)
:- use_package(oo_syntax).
:- use_module(library(mutables/mutables_rt)). % (ptojs mutables)

% Allocate a new global variable J
new_cur_obj(J) :-
	'\6\dot'(mutables_rt,nb_mut_num(0, J)). % TODO: syntax hack to avoid parsing problems with condcomp

% Delete the global variable J
del_cur_obj(_). % (reclaimed as GC)

% Unify Val with the current value of J
get_cur_obj(J, Val) :-
	Val = '@'(J).

% Set the value of J to Val
set_cur_obj(J, Val) :-
	'<-'(J, Val).
	
:- else.

:- data(cur_obj/2).

cur_obj(-1, _).

% Allocate a new global variable J
new_cur_obj(J) :-
	current_fact(cur_obj(I, _)), !,
	J is I + 1.

% Delete the global variable J
del_cur_obj(J) :-
	retractall_fact(cur_obj(J, _)).

% Unify Val with the current value of J
get_cur_obj(J, Val) :-
	current_fact(cur_obj(J, Val)), !.

% Set the value of J to Val
set_cur_obj(J, Val) :-
	retractall_fact(cur_obj(J, _)), 
	asserta_fact(cur_obj(J, Val)).

:- endif.

% ---------------------------------------------------------------------------
