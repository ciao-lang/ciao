:- module(_, [], [assertions, dcg]).

:- doc(title, "Pretty printing of solutions").

:- doc(author,"The Ciao Development Team").
:- doc(author,"Jose F. Morales").

% TODO: optimize, simplify, make check_cycles unnecessary

:- doc(module, "This module provides predicates to pretty print
   solutions (substitutions and constraints).

   Example:

@begin{verbatim}
?- ( read_term(user, Q, [dictionary(Dict)]),
     call(Q),
     dump_solution(Dict,Sol),
     display_solution(Sol), nl, nl,
     fail
   ; true
   ).
|: append(X,Y,[1,2,3]).
X = [],
Y = [1,2,3]

X = [1],
Y = [2,3]

X = [1,2],
Y = [3]

X = [1,2,3],
Y = []

@end{verbatim}
").

% ---------------------------------------------------------------------------

:- use_module(engine(runtime_control), [current_prolog_flag/2]).
:- use_module(library(dict), [dic_lookup/3, dic_get/3, dictionary/1]).
:- use_module(library(sort), [keysort/2]).

:- export(dump_solution/2).
:- pred dump_solution(Dict, Solution) : dictionary(Dict) => list(Solution)
   # "Given a @var{Dict} mapping variable names to their value (as
      obtained by @pred{read_term/3}, obtain a list of terms
      representing a @var{Solution} (sustitution and constraints).".

dump_solution(Dict, Solution) :-
	current_prolog_flag(check_cycles, CyclesFlag), % TODO: just check if the term is cyclic
	( CyclesFlag = on ->
	    dump_solution_cycles(Dict, Solution0)
	; dump_solution_nocycles(Dict, Solution0)
	),
	flat_solution(Solution0, Solution, []),
	prettyvars(Solution, Dict).

dump_solution_cycles(Dict, Solution) :-
	del_hidden_vars(Dict, Varqueue, Varqueue_),
	reverse_dict(Varqueue, Varqueue_, RevDict),
	uncycle_eqs(Varqueue, Varqueue_, 0, NewVarIdx, RevDict,
	    Varq_nc, Varq_nc_),
	answer_constraints((Varq_nc, Varq_nc_), (Varq_nc2, Varq_nc2_),
	    Constraints),
	uncycle_constraints(Constraints, NewVarIdx,
	    Constraints_nc_eqs),
	solution_eqs(Varq_nc2, Varq_nc2_, Solution, Constraints_nc_eqs).

dump_solution_nocycles(Dict, Solution) :-
	answer_constraints_nc(Dict, Dict2, Constraints),
	solution_vars(Dict2, Eqs, []),
	Solution = [Eqs|Constraints].

del_hidden_vars(Var, L, L_) :- var(Var), !, L = L_.
del_hidden_vars(dic(Var, [Val|_], Lft, Rgt), L, L_) :-
	del_hidden_vars(Lft, L, L1),
	( Var = "_"||_ ->
	    % Vars starting with "_" are not printed
	    L1 = L2
	; L1 = [(Var=Val)|L2]
	),
	del_hidden_vars(Rgt, L2, L_).

reverse_dict(Q, Q_, _) :- Q == Q_, !.
reverse_dict([(Var=Val)|Q], Q_, RevDict) :-
	dic_lookup(RevDict, Val, VarX),
	(var(VarX) -> VarX = Var ; true),
	reverse_dict(Q, Q_, RevDict).

uncycle_eqs(EqL, EqL_, N, Nlast, _RevDict, EqLnc, EqLnc_) :-
	EqL == EqL_, !,
	Nlast = N,
	EqLnc = EqLnc_.
uncycle_eqs([(Var=Val)|EqL], EqL_, N, Nlast, RevDict,
	    [(Var=NewVal)|EqLnc], EqLnc_) :-
	uncycle_val(Val, [], N, N1, RevDict, EqL_, EqL_2, NewVal),
	uncycle_eqs(EqL, EqL_2, N1, Nlast, RevDict, EqLnc, EqLnc_).

uncycle_val(Val, _Seen, N, N1, _RevDict, NewEqs, NewEqs_, NewVal) :-
	var(Val), !,
	N1 = N,
	NewEqs = NewEqs_,
	NewVal = Val.
uncycle_val(Val, _Seen, N, N1, _RevDict, NewEqs, NewEqs_, NewVal) :-
	atomic(Val), !,
	N1 = N,
	NewEqs = NewEqs_,
	NewVal = Val.
uncycle_val(Val, Seen, N, N1, RevDict, NewEqs, NewEqs_, NewVal) :-
	already_seen(Seen, Val), !,
	dic_lookup(RevDict, Val, Var),
	( var(Var) ->
	    new_varname(N, Var),
	    N1 is N+1,
	    NewEqs = [(Var=Val)|NewEqs_]
	;
	    N1 = N,
	    NewEqs_ = NewEqs
	),
	atom_codes(VarName, Var),
	NewVal = '$VAR'(VarName).
uncycle_val(Val, Seen, N, N1, RevDict, NewEqs, NewEqs_, NewVal) :-
	functor(Val,    F, A),
	functor(NewVal, F, A),
	uncycle_val_args(A, Val, [Val|Seen], N, N1, RevDict,
	    NewEqs, NewEqs_, NewVal).

uncycle_val_args(0, _,   _,    N, N,  _,       NewEqs, NewEqs,  _) :- !.
uncycle_val_args(A, Val, Seen, N, N_, RevDict, NewEqs, NewEqs_, NVal) :-
	A1 is A-1,
	arg(A, Val,  ValA),
	arg(A, NVal, NValA),
	uncycle_val(ValA, Seen, N, N1, RevDict, NewEqs, NewEqs1, NValA),
	uncycle_val_args(A1, Val, Seen, N1, N_, RevDict,
	    NewEqs1, NewEqs_, NVal).

already_seen([T|_], Term) :-
	T == Term, !.
already_seen([_|Ts], Term) :-
	already_seen(Ts, Term).

new_varname(N, Var) :-
	number_codes(N, NS),
	Var = "_"||NS.

% ---------------------------------------------------------------------------

flat_solution([], Xs, Xs0) :- !, Xs = Xs0.
flat_solution([C|Cs], Xs, Xs0) :- !,
	flat_solution(C, Xs, Xs1),
	flat_solution(Cs, Xs1, Xs0).
flat_solution((G1,G2), Xs, Xs0) :- !,
	flat_solution(G1, Xs, Xs1),
	flat_solution(G2, Xs1, Xs0).
flat_solution(true, Xs, Xs0) :- !, Xs = Xs0.
flat_solution(G, [G|Xs], Xs).

% ---------------------------------------------------------------------------

:- use_module(library(attrdump), [copy_extract_attr/3, copy_extract_attr_nc/3]).
:- multifile dump_constraints/3. % For CLP{Q,R} (DCG)

answer_constraints(Dict, Dict2, Constraints) :-
	dump_constraints(Dict, Dict2, Constraints), !.
answer_constraints(Dict, Dict2, Constraints) :-
	copy_extract_attr(Dict, Dict2, Constraints).

answer_constraints_nc(Dict, Dict2, Constraints) :-
	dump_constraints(Dict, Dict2, Constraints), !.
answer_constraints_nc(Dict, Dict2, Constraints) :-
	copy_extract_attr_nc(Dict, Dict2, Constraints).

% ---------------------------------------------------------------------------

uncycle_constraints(Cs, N, Cs_nc_eqs) :-
	uncycle_val(Cs, [], N, N1, RevDict, Varq, Varq_, Cs_nc),
	uncycle_eqs(Varq, Varq_, N1, _, RevDict, Varqn, []),
	Cs_nc_eqs = [Cs_nc|Varqn].

solution_eqs(EqL, EqL_, SolEqs, SolEqs_) :-
	EqL == EqL_, !,
	SolEqs = SolEqs_.
solution_eqs([(Var=Val)|EqL], EqL_, SolEqs, SolEqs_) :-
	( var(Val) ->
	    atom_codes(AtomVar, Var),
	    Val='$VAR'(AtomVar),
	    SolEqs = SolEqs1
	;
	    SolEqs = [(Var=Val)|SolEqs1]
	),
	solution_eqs(EqL, EqL_, SolEqs1, SolEqs_).

solution_vars(D) --> {var(D)}, !.
solution_vars(dic(Var, [Val|_], L, R)) -->
	solution_vars(L),
	solution_var(Var, Val),
	solution_vars(R).

solution_var([0'_|_], _) --> !. % Do not display vars starting with "_"
solution_var(Var, Val) --> {var(Val)}, !,
	{ atom_codes(AtomVar, Var), Val='$VAR'(AtomVar) }.
solution_var(Var, Val) -->
	[Var = Val].

% ---------------------------------------------------------------------------
% This is alike the one in library(write), except that variable names
% start with "_"
% TODO: do not duplicate code

prettyvars(Term, Dict) :-
	collect_vars(Term, Vars0, []),
	keysort(Vars0, Vars),
	pretty_vars(Vars, Dict, 0, _).

collect_vars(Var) -->
	{var(Var)}, !, [Var-[]].
collect_vars([X|Xs]) --> !,
	collect_vars(X),
	collect_vars(Xs).
collect_vars(X) -->
	{functor(X, _, A)},
	collect_vars_(0, A, X).

collect_vars_(A,  A, _) --> !.
collect_vars_(A0, A, X) -->
	{A1 is A0+1},
	{arg(A1, X, X1)},
	collect_vars(X1),
	collect_vars_(A1, A, X).

pretty_vars([], _Dict, N, N).
pretty_vars([X, Y|Xs], Dict, N0, N2):-
	X==Y, !,
	X = ('$VAR'(Name)-[]),
	free_name_var(Name, Dict, N0, N1),
	pretty_vars_(Xs, X, Dict, N1, N2).
pretty_vars(['$VAR'('_')-[]|Xs], Dict, N0, N1):-
	pretty_vars(Xs, Dict, N0, N1).

pretty_vars_([X|Xs], Y, Dict, N0, N1):-
	X==Y, !,
	pretty_vars_(Xs, Y, Dict, N0, N1).
pretty_vars_(Xs, _, Dict, N0, N1) :-
	pretty_vars(Xs, Dict, N0, N1).

free_name_var(Name, Dict, N0, N1) :-
	Letter is N0 mod 26 + 0'A,
	( N0>=26 ->
	    Rest is N0//26,
	    number_codes(Rest, Index)
	; Index = ""
	),
	StrName = [0'_, Letter|Index], 
	\+ dic_get(Dict,StrName,_), !,
	atom_codes(Name, StrName), 
	N1 is N0 + 1.
free_name_var(X, Dict, N0, N2) :-
	N1 is N0 + 1,
	free_name_var(X, Dict, N1, N2).

% ---------------------------------------------------------------------------

:- use_module(library(stream_utils), [write_string/1]).
:- use_module(library(write), [write/1, write_term/2]).
:- use_module(engine(io_basic), [display/1, nl/0]).
:- use_module(engine(stream_basic), [current_output/1, set_output/1]).

:- export(display_solution/1).
:- pred display_solution(Solution) : list(Solution)
   # "Pretty print @var{Solution}".

display_solution([]) :-
	display('true').
display_solution([C|Cs]) :-
	display_constraint(C),
	display_solution2(Cs).

display_solution2([]).
display_solution2([C|Cs]) :-
	display(','), nl,
	display_constraint(C),
	display_solution2(Cs).

display_constraint((Var=Val)) :- !,
	write_string(Var),
	display(' = '),
	write_term(Val, [quoted(true), portrayed(true), numbervars(true),
	                 priority(699)]).
display_constraint(attach_attribute(X, A)) :- !,
	write(X),
	display(' attributed '),
	write_term(A, [quoted(true), portrayed(true), numbervars(true)]).
display_constraint(G) :-
	write_term(G, [quoted(true), portrayed(true), numbervars(true)]).

