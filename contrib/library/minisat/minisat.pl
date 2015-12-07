%%============================================================================ 
%% The SWI-Prolog interface to MiniSat SAT solver
%% http://www.cs.chalmers.se/Cs/Research/FormalMethods/MiniSat/MiniSat.html
%%
%% Copyright (c) 2006, Michael Codish, Vitaly Lagoon, and Peter J. Stuckey
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to
%% permit persons to whom the Software is furnished to do so, subject to
%% the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
%% LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
%% OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
%% WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.



:- module(minisat,[sat/1],[foreign_interface]).

:- use_module(library(terms_vars), [varset/2]).

:- use_module(library(write)).


%%% sat/1
%%%
%%%
sat(F) :-
	minisat_new_solver,
	minisat_add_clause([-1]), % zero
	minisat_add_clause([2]), % one
	varset(F,FVars),
	\+ \+ ( bind2index(FVars,3), add_cnf_clauses(F)),
	Table = [0,1|FVars],
	minisat_solve(Model),
	assign_model(Table,Model),
	minisat_delete_solver, 
	!.
sat(_) :- 
	minisat_delete_solver, 
	fail.

add_cnf_clauses([]).
add_cnf_clauses([Cl|Cls]) :-
	to_minisat(Cl,MiniSatCl),
	minisat_add_clause(MiniSatCl),
	add_cnf_clauses(Cls).

to_minisat([],[]).
to_minisat([L|Ls],[N|Ns])  :- 
	minisat_translate(L,N), 
	to_minisat(Ls,Ns).

minisat_translate(0,1)    :- !.
minisat_translate(1,2)    :- !.
minisat_translate(-(1),1) :- !.
minisat_translate(-(0),2) :- !.
minisat_translate(N,NN) :- 
	NN is N.

bind2index([],_).
bind2index([N|Ns],N) :- 
	N1 is N+1, 
	bind2index(Ns,N1).

assign_model([],_).
assign_model([V|Vs],[N|Ns]) :- 
	( N<0 -> V=0 ; V=1), 
	  assign_model(Vs,Ns).


%%% Foreign interface predicates
%%%
%%%

:- extra_linker_opts('-L.').
:- use_foreign_library('minisat').


:- true pred minisat_new_solver_1(go(Success))
        :: int + (returns(Success), foreign(minisat_new_solver)).

minisat_new_solver  :- minisat_new_solver_1(1).


:- true pred minisat_delete_solver_1(go(Success))
        :: int + (returns(Success), foreign(minisat_delete_solver)).

minisat_delete_solver  :- minisat_delete_solver_1(1).


:- true pred minisat_add_clause_2(in(_),go(Success))
        :: any_term * int + (returns(Success), foreign(minisat_add_clause)).

minisat_add_clause(A) :- minisat_add_clause_2(A,1).


:- true pred minisat_solve_2(out(_),go(Success))
        :: any_term * int + (returns(Success), foreign(minisat_solve)).

minisat_solve(A) :- minisat_solve_2(A,1).


