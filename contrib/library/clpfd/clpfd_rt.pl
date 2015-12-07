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

:- module(clpfd_rt, [in/2], [assertions, regtypes, fsyntax, dcg]).

:- doc(title, "Finite domain solver runtime").

:- doc(author, "Emilio Jes@'{u}s Gallego Arias").
:- doc(author, "R@'{e}my Haemmerl@'{e}").
:- doc(copyright,"@include{DocCopyright.lpdoc}").

:- doc(module, "This module provides Finite Domain (FD) constraints and
enumerating predicates for FD variables. See @lib{clpfd_doc} for
more details about the Ciao FD solver.

This module provides two kinds of contraints: @em{basic constraints}
(such as @pred{domain/3}, @pred{all_different/1}, ...) that deal with
FD variables or lists of FD variables, and @em{meta-contraints} (such
as @pred{#=/2}, @pred{#=</2}, ...) that deal with arithmetic
expressions over FD variables, called in the following FD expressions.
Meta-constraints of this module behave as describes in package
@lib{clpfd}, except that their arguments are interpreted at run time.

").

:- use_package(library(clpfd/clpfd_debug)).

:- include(library(clpfd/clpfd_ops)).

:- use_module(library(clpfd/fd_term), [new/1, add_propag/3, var_id/2, in/2, dom_term/2]).
:- use_module(library(clpfd/fd_constraints)).

% TODO: Rename to clpfd_var_t
:- export(fdvar/1).
:- export(fd_range_expr/1).
:- export(fd_expr/1).
:- regtype fdvar(X) # "@var{X} is a variable or an integer.".

fdvar(X) :- var(X).
fdvar(X) :- int(X).

:- if((defined(optim_comp), backend(js_backend))).
int(X) :- integer(X). % TODO: define in the right place
:- endif.

:- regtype fd_range_expr(Expr) # "@var{Expr} is a range
expression.".

fd_range_expr(I) :- integer(I).
fd_range_expr(Min .. Max) :- integer(Min), integer(Max).
fd_range_expr(A \/ B) :- fd_range_expr(A), fd_range_expr(B).

:- doc(fd_range_expr/1,"A term denoting a range expression:
@includedef{fd_range_expr/1} an integer stands for a singleton range,
@var{Min..Max} for the closed interval from @var{Min} to @var{Max},
and @var{A\\/B} for the union of ranges @var{A} and @var{B}. Range
expressions are used by the @pred{in/2} predciate." ).

:- regtype fd_expr(Expr) # "@var{Expr} is an FD expression.".

fd_expr(Var):- var(Var).
fd_expr(I * Exp) :- int(I), fd_expr(Exp).
fd_expr(Exp * I) :- int(I), fd_expr(Exp).
fd_expr(Exp1 + Exp2) :- 
        fd_expr(Exp1), fd_expr(Exp2).
fd_expr(Exp1 - Exp2) :- 
        fd_expr(Exp1), fd_expr(Exp2).
fd_expr(- Exp) :- 
        fd_expr(Exp).

:- doc(fd_expr/1,"A term denoting an arithmetic expression over FD
 variables: @includedef{fd_expr/1} FD expressions are used by
 meta-constraints." ).


%% Basic predicates.

:- export('#='/2).
:- doc('#='/2, "Meta-constraint \"equal\".").
:- pred (A #= B) : fd_expr * fd_expr # "Constrains the interpretation of 
   @var{A} to be equal to the interpretation of @var{B}.".

A #= B :-
	linearize_fd(A, Al),
	linearize_fd(B, Bl),
	'a=b'(Al,Bl).

:- export('#\\='/2).  
:- doc('#\\='/2, "Meta-constraint \"not equal\".").
:- pred (A #\= B) : fd_expr * fd_expr # "Constrains the interpretation of 
   @var{A} to be different from the interpretation of  @var{B}".

A #\= B :-
	linearize_fd(A, Al),
	linearize_fd(B, Bl),
	'a<>b'(Al,Bl).

:- export('#<'/2). 
:- doc('#\<'/2, "Meta-constraint \"smaller than\".").

:- pred (A #< B) : fd_expr * fd_expr # "Constrains the interpretation of @var{A} to be
smaller than the interpretation of @var{B}.".
:- doc(fd_range_expr/1, "Meta-contraint").

A #< B :-
	linearize_fd(A, Al),
	linearize_fd(B, Bl),
	'a<b'(Al,Bl).

:- export('#=<'/2). 
:- doc('#=<'/2, "Meta-constraint \"smaller or equal\".").
:- pred (A #=< B) : fd_expr * fd_expr # "Constrains @var{A} to be
smaller or equal to @var{B}.".

A #=< B :-
	linearize_fd(A, Al),
	linearize_fd(B, Bl),
	fd_constraints:'a=<b'(Al,Bl).

:- export('#>'/2). 
:- doc('#>'/2, "Meta-constraint \"greater than\".").
:- pred (A #> B) : fd_expr * fd_expr # "Constrains the interpretation of 
   @var{A} to be greater than the interpretation of @var{B}.".

A #> B :-
	linearize_fd(A, Al),
	linearize_fd(B, Bl),
	'a<b'(Bl,Al).

:- export('#>='/2). 
:- doc('#>='/2, "Meta-constraint \"greater or equal\".").
:- pred (A #>= B) : fd_expr * fd_expr # "Constrains the interpretation of 
   @var{A} to be greater or equal than the interpretation of @var{B}.".

A #>= B :-
	linearize_fd(A, Al),
	linearize_fd(B, Bl),
	fd_constraints:'a=<b'(Bl,Al).


%% Just a var.
linearize_fd(A, X) :-
	var(A),!,
	wrapper(A, X).

linearize_fd(A, A) :-
	integer(A),
	!.

linearize_fd(A+B, C) :-
	linearize_fd(A, Al),
	linearize_fd(B, Bl),
	fd_term:new(C),
	fd_constraints:'a+b=c'(Al,Bl,C),
	!.

linearize_fd(A-B, C) :-
	linearize_fd(A, Al),
	linearize_fd(B, Bl),
	'a-b=c'(Al,Bl,C),
	!.

linearize_fd(A*B, C) :-
	linearize_fd(A, Al),
	linearize_fd(B, Bl),
	fd_term:new(C),
	fd_constraints:'a=b*c'(C, Al,Bl),
	!.


%% High level CP implementation.

:- export(domain/3).

:- pred domain(Vars, Min, Max) : list(fdvar) * int * int #
"Constrains each element of @var{Vars} to take its value between
@var{Min} and @var{Max} (included). This predicate is generally used
to set the initial domain of an interval".

domain([], _, _).
domain([X|R], N1, N2) :-
	fd_constraints:x_in_range(~wrapper(X), N1, N2),
	domain(R, N1, N2).

:- export(in/2).
:- pred (Var in Range) : fdvar * fd_range_expr # "Constrains @var{Var}
to take its value in the domain described by @var{Range}.".

in(X, Range):-
	fd_term:in(~wrapper(X), Range).

:- export(all_different/1).
:- pred all_different(Vars) : list(fdvar) # "Constrains all elements in
@var{Vars} to take distinct values. This is equivalent to posting an
inequality constraint for each pair of variables. This constraint is
triggered when a variable becomes ground, removing its value from the
domain of the other variables.".

all_different(L):-
	fd_constraints:fd_all_different(~wrapper_list(L)).

:- export(labeling/2).

:- pred labeling(Options, Vars) : list(Vars, fdvar) # "Assigns a value
to each variable in @var{Vars} according to the labeling options given
by @var{Options}. This predicate is re-executable on backtracking.

The different options are :
@begin{itemize}

@item @var{[]}: the leftmost variables is selected first. Its values
are enumerating from the smallest to the greatest.

@item @var{[ff]}: the variable with the smallest number of elements in
its domain is selected first. Its values are then enumerated from the
smallest to the greatest.

@item @var{[step]}: the variable with the smallest number of elements
in its domain is selected first. The minimal value of the domainis
assigned, on bactracking the value is pruned form the domain and a new
variable is selected.

@end{itemize}	
".

:- use_module(library(clpfd/fd_labeling), [labeling/2]).

:- export(indomain/1).
indomain(Var) :- 
	clpfd_rt:label([Var]).

:- export(label/1).
label(Vs) :- 
	clpfd_rt:labeling([], Vs).

:- export(labeling/2).
labeling(Options, Vars):-
	fd_labeling:labeling(Options, ~wrapper_list(Vars)).


:- use_module(library(clpfd/fd_optim), [fd_minimize/2, fd_maximize/2]).

:- export(minimize/2).
:- meta_predicate(minimize(:, ?)).
minimize(Goal, Var):-
	fd_optim:fd_minimize(Goal, ~wrapper(Var)).

:- export(maximize/2).
:- meta_predicate(maximize(:, ?)).
maximize(Goal, Var):-
	fd_optim:fd_maximize(Goal, ~wrapper(Var)).


:- use_package(attr).

:- export(wrapper/2).
wrapper(X, X):- integer(X), !.
wrapper(A, X):-	get_attr_local(A, X), !.
wrapper(A, X):- var(A), !,
	fd_term:new(X),
	put_attr_local(A, X),
	% Force instantiation of A when X represents an integer
	fd_term:add_propag(X, val, 'fd_term:integerize'(X, A)).

wrapper_list([]) := [].
wrapper_list([H|T]) := [~wrapper(H)| ~wrapper_list(T)].

attr_unify_hook(IdxVar, Other):-
        ( 
            nonvar(Other) ->
            (
                integer(Other) ->
                fd_constraints:'a=t'(IdxVar, Other)
            ; 
                clpfd_error("Unifying {_FDVAR~p} with non integer ~p~n", 
		            [~var_id(IdxVar), Other])
            )
        ; 
            get_attr_local(Other, IdxVar_) ->
            fd_constraints:'a=b'(IdxVar, IdxVar_)
        ;
            put_attr_local(Other, IdxVar)
        ).


attribute_goals(Var) -->
	[in(Var,~(fd_term:dom_term(~get_attr_local(Var))))].
