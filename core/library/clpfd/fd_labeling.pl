%% ---------------------------------------------------------------------------
%% This file is part of the CLP(FD) package for Ciao.
%% It is originally a part of SWI-Prolog.
%%
%% Copyright (C) 2007-2011 Markus Triska
%% Copyright (C) 2012-2013 CLIP Group
%%
%% Originally written by: 
%%  * Markus Triska <triska@gmx.at>
%%
%% Modify by:
%%  * Remy Haemmerle
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
%% You should have received a copy of the GNU General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
%%
%% As a special exception, if you link this library with other files,
%% compiled with a Free Software compiler, to produce an executable, this
%% library does not by itself cause the resulting executable to be covered
%% by the GNU General Public License. This exception does not however
%% invalidate any other reasons why the executable file might be covered by
%% the GNU General Public License.
%% ---------------------------------------------------------------------------



:- module(fd_labeling,[labeling/2], [fsyntax, hiord]).


%:- use_module(library(clpfd/fd_range), [is_singleton/1]).
:- use_module(library(clpfd/fd_term)).
:- use_module(library(clpfd/fd_utils), [clpfd_error/2]).
:- use_module(library(clpfd/fd_constraints)).

:- use_module(library(hiordlib), [map/3]).
:- use_module(library(random)).


labeling(Options, Vars) :-
        % must_be(list, Options),
        % must_be(list, Vars),
        % maplist(finite_domain, Vars),
        label_(Options, Options, default(leftmost), default(up), default(step), [], upto_ground, Vars).

label_([O|Os], Options, Selection, Order, Choice, Optim, Consistency, Vars) :-
        (   var(O)-> clpfd_error(instantiation_error, fd_labeling:labeling/2-2)
        ;   override(selection, Selection, O, Options, S1) ->
            label_(Os, Options, S1, Order, Choice, Optim, Consistency, Vars)
        ;   override(order, Order, O, Options, O1) ->
            label_(Os, Options, Selection, O1, Choice, Optim, Consistency, Vars)
        ;   override(choice, Choice, O, Options, C1) ->
            label_(Os, Options, Selection, Order, C1, Optim, Consistency, Vars)
%        ;   optimisation(O) ->
%            label_(Os, Options, Selection, Order, Choice, [O|Optim], Consistency, Vars)
        ;   consistency(O, O1) ->
            label_(Os, Options, Selection, Order, Choice, Optim, O1, Vars)
        ;   clpfd_error(domain_error(label_option, O), fd_labeling:labeling/1-2)
        ).
label_([], _Options, Selection, Order, Choice, Optim0, Consistency, Vars) :-
%        maplist(arg(1), [Selection,Order,Choice], [S,O,C]),
	map([Selection,Order,Choice], (''(X, Y) :- arg(1, X, Y)), [S,O,C]),
	(   Optim0 == [] ->
            label__(Vars, S, O, C, Consistency)
        ;   
	    % TODO: Optimization not implemented
	    clpfd_error(not_implemented, fd_labeling:labeling/1)
        ).


label__([], _, _, _, Consistency) :- !,
        (   Consistency = upto_in(I0,I) -> I0 = I
        ;   true
        ).
label__(Vars, Selection, Order, Choice, Consistency) :-
	select_var(Selection, Vars, Var, RVars),
	% (   Consistency = upto_in(I0,I), clpfd:fd_get(Var, _, Ps), all_dead(Ps) ->
	%                     clpfd:fd_size(Var, Size),
	%                     I1 is I0*Size,
	%                     label(RVars, Selection, Order, Choice, upto_in(I1,I))
	%                 ;   Consistency = upto_in, clpfd:fd_get(Var, _, Ps), all_dead(Ps) ->
	%                     label__(RVars, Selection, Order, Choice, Consistency)
	%                 ;   choice_order_variable(Choice, Order, Var, RVars, Vars, Selection, Consistency)
	%                 )   
	choice_order_variable(Choice, Order, Var, RVars, Vars, Selection, Consistency).

choice_order_variable(step, Order, Var, Vars, Vars0, Selection, Consistency) :-
	fd_term:next_in_dom(Order, Var, Next),!,
	(   fd_constraints:'a=t'(Var, Next),
	    label__(Vars, Selection, Order, step, Consistency)
	;
	    fd_constraints:'a<>t'(Var, Next),
	    label__(Vars0, Selection, Order, step, Consistency)
	).
choice_order_variable(enum, Order, Var, Vars, _, Selection, Consistency) :-!,
	fd_term:enum(Var, Val),
        fd_constraints:'a=t'(Var, Val),
        label__(Vars, Selection, Order, enum, Consistency).
choice_order_variable(bisect, Order, Var, _, Vars0, Selection, Consistency) :-
	fd_term:bounds(Var, I, S), I \= S, !,
        Mid0 is (I + S) // 2,
        (   Mid0 = S -> Mid is Mid0 - 1 ; Mid = Mid0 ),
        (   Order == up ->   ( fd_constraints:'a=<t'(Var, Mid) ; fd_constraints:'a<t'(Mid, Var) )
        ;   Order == down -> ( fd_constraints:'a<t'(Mid, Var) ; fd_constraints:'a=<t'(Var, Mid) )
        ;   clpfd_error(domain_error(bisect_up_or_down, Order), fd_labeling:labeling/2)
        ),
        label__(Vars0, Selection, Order, bisect, Consistency).
choice_order_variable(Choice, Order, _, Vars, _, Selection, Consistency) :-
	label__(Vars, Selection, Order, Choice, Consistency).

:- meta_predicate(override(pred(1), +, +, +, -)).
override(What, Prev, Value, Options, Result) :-
        What(Value),
        override_(Prev, Value, Options, Result).

override_(default(_), Value, _, user(Value)).
override_(user(Prev), Value, Options, _) :-
        (   Value == Prev ->
            clpfd_error(domain_error(nonrepeating_labeling_options, Options),  fd_labeling:labeling/2)
        ;   clpfd_error(domain_error(consistent_labeling_options, Options), fd_labeling:labeling/2)
        ).

selection(ff).
selection(ffc).
selection(min).
selection(max).
selection(leftmost).
selection(random_variable(Seed)) :-
%        must_be(integer, Seed),
        random:srandom(Seed).

choice(step).
choice(enum).
choice(bisect).

order(up).
order(down).
% TODO: random_variable and random_value currently both set the seed,
% so exchanging the options can yield different results.
order(random_value(Seed)) :-
%        must_be(integer, Seed),
        random:srandom(Seed).

consistency(upto_in(I), upto_in(1, I)).
consistency(upto_in, upto_in).
consistency(upto_ground, upto_ground).

%optimisation(min(_)).
%optimisation(max(_)).

select_var(leftmost, [Var|Vars], Var, Vars).
select_var(min, [V|Vs], Var, RVars) :-
        find_min(Vs, V, Var),
        delete_eq([V|Vs], Var, RVars).
select_var(max, [V|Vs], Var, RVars) :-
        find_max(Vs, V, Var),
        delete_eq([V|Vs], Var, RVars).
select_var(ff, [V|Vs], Var, RVars) :-
        fd_term:size(V, S),
        find_ff(Vs, V, S, Var),
        delete_eq([V|Vs], Var, RVars).
%select_var(ffc, [V|Vs], Var, RVrs) :-
%        find_ffc(Vs, V, Var),
%        delete_eq([V|Vs], Var, RVars).
%select_var(random_variable(_), Vars0, Var, Vars) :-
%        length(Vars0, L),
%        I is random(L),
%        nth0(I, Vars0, Var),
%        delete_eq(Vars0, Var, Vars).

find_min([], Var, Var).
find_min([V|Vs], CM, Min) :-
        (   min_lt(V, CM) ->
            find_min(Vs, V, Min)
        ;   find_min(Vs, CM, Min)
        ).

find_max([], Var, Var).
find_max([V|Vs], CM, Max) :-
        (   max_gt(V, CM) ->
            find_max(Vs, V, Max)
        ;   find_max(Vs, CM, Max)
        ).		

find_ff([], Var, _, Var) :- !.
find_ff(_, Var, 1, Var) :- !.
find_ff([V|Vs], CM, S0, FF) :-
        (  
	    fd_term:size(V, S1),  S1 < S0 ->
	    find_ff(Vs, V, S1, FF)
	;
	    find_ff(Vs, CM, S0, FF)
	).

% TODO: implements ffc
% find_ffc([], Var, Var).
% find_ffc([V|Vs], Prev, FFC) :-
%         (   ffc_lt(V, Prev) ->
%             find_ffc(Vs, V, FFC)
%         ;   find_ffc(Vs, Prev, FFC)
%         ).


% ffc_lt(X, Y) :-
%         (   clpfd:fd_get(X, XD, XPs) ->
%             domain_num_elements(XD, n(NXD))
%         ;   NXD = 1, XPs = []
%         ),
%         (   clpfd:fd_get(Y, YD, YPs) ->
%             domain_num_elements(YD, n(NYD))
%         ;   NYD = 1, YPs = []
%         ),
%         (   NXD < NYD -> true
%         ;   NXD =:= NYD,
%             props_number(XPs, NXPs),
%             props_number(YPs, NYPs),
%             NXPs > NYPs
%         ).

min_lt(X,Y) :- fd_term:min(X,LX), fd_term:min(Y, LY), LX < LY.

max_gt(X,Y) :- fd_term:max(X,UX), fd_term:max(Y, UY), UX < UY.


delete_eq([], _, []).
delete_eq([X|Xs], Y, List) :-
        (   fd_term:is_singleton(X) -> delete_eq(Xs, Y, List)
        ;   X == Y -> List = Xs
        ;   List = [X|Tail],
            delete_eq(Xs, Y, Tail)
        ).

