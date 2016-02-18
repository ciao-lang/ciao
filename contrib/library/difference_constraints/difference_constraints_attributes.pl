:- use_package(library(attr/attr_mono_compat)).

:- use_module(library(write),[write/1]).
% :- use_module(library(lists),[append/3]).
% :- use_module(library(dict)).

verify_attribute(dbm_id(_,Var), Int) :-
	integer(Int), 
%	Var #= Int.
	difference_constraints_LB(Var,Int),
	difference_constraints_UB(Var,Int).	

combine_attributes(dbm_id(_,Var1), dbm_id(_,Var2)) :-
%	Var1 #= Var2.
	difference_constraints_const(Var1,Var2,0),
	difference_constraints_const(Var2,Var1,0).

portray_attribute(dbm_id(Id,_),Var) :-
	write(Var), display(' in '),
	difference_constraints_print_variable(Id).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% RH: Deactivated because, it does not seems right. Term is a
% arbitrary term, the code does not seems to take that into account.
% dump_constraints(Term, Copy, Const) :-
% 	dump_aux(Term,Copy,Dict,LVars),
% 	(
% 	    var(Dict) ->
% 	    Const = []
% 	;
% 	    difference_constraints_do_canonical,
% 	    make_const(LVars,Const)
% 	), 
% 	message(dump_constraints(Term, Copy, Const)).

% dump_aux(V,V,_,[]) :- 
% 	type(V,var), !.
% dump_aux(dic([N],[T1|V],LN1,RN1), dic([N],[T2|V],LN2,RN2), Dict, LVars) :- !,
% 	type(T1,Type),
%   	ct_top(Type, T1, T2, Dict, HVars),
% 	dump_aux(LN1,LN2,Dict,LNVars),
% 	dump_aux(RN1,RN2,Dict,RNVars),
% 	append(LNVars,HVars,LVarsTmp),
% 	append(LVarsTmp,RNVars,LVars).
% dump_aux(V,_,_,[]) :- display(warning_difference_constraints_dump(V)),nl.

% ct_top(attv, Cva, Copy, Dict, LVars) :-
% 	dic_lookup(Dict, Cva, Copy, Old),
% 	(
% 	    Old = old ->
% 	    LVars = []
% 	;
% 	    LVars = [(Cva,Copy)]
% 	).
	    
% ct_top(var, V, V, _, []).
% ct_top(integer, I, I, _, []).
% ct_top(float, F, F, _, []).
% ct_top(atom, A, A, _, []).
% ct_top(list, [X|Xt], [Xc|Xct], Dict, LVars) :-
% 	type(X, Type),
% 	ct_top(Type, X, Xc, Dict, HVars),
% 	type(Xt, Typet),
% 	ct_top(Typet, Xt, Xct, Dict, RVars),
% 	append(HVars, RVars, LVars).
% ct_top(structure, Term, Copy, Dict, LVars) :-
%   functor(Term, N, A),
%   functor(Copy, N, A),
%   ct_top_args(A, Copy, Dict, Term, LVars).

% ct_top_args(0, _,    _,    _, []) :- !.
% ct_top_args(N, Copy, Dict, Term, LVars) :-
%   N1 is N-1,
%   arg(N, Copy, Ac),
%   arg(N, Term, At),
%   type(At,Type),
%   ct_top(Type, At, Ac, Dict, AVars),
%   ct_top_args(N1, Copy, Dict, Term, RVars),
%   append(AVars, RVars, LVars).

% make_const(LVars1,Const) :-
% 	make_fix_vars(LVars1,LVars2,Const1),
% 	make_const_aux(LVars2,LVars2,Const2),
% 	append(Const1,Const2,Const).

% make_fix_vars([],[],[]).
% make_fix_vars([(Cva,Hva)|R1],R2,['#='(Hva, Min)|Const]) :-
% 	difference_constraints_min(Cva,Min),
% 	difference_constraints_max(Cva,Min), !,
% 	make_fix_vars(R1,R2,Const).
% make_fix_vars([(Cva,Hva)|R1],[(Cva,Hva)|R2],Const) :-
% 	make_fix_vars(R1,R2,Const).

% make_const_aux([],_,[]).
% make_const_aux([(Cva,Hva)|R],LVars,Const) :-
% 	make_const_var(LVars,(Cva,Hva),HConst),
% 	make_const_aux(R,LVars,RConst),
% 	append(HConst,RConst,Const).

% make_const_var([],(Cva,Hva),Const) :- 
% 	(
% 	    difference_constraints_min(Cva,Min) ->
% 	    MinConst = ['#=<'(Min,Hva)]
% 	;
% 	    MinConst = []
% 	),
% 	(
% 	    difference_constraints_max(Cva,Max) ->
% 	    Const = ['#=<'(Hva, Max) | MinConst]
% 	;
% 	    Const = MinConst
% 	).

% make_const_var([(CvaD,_)|RVars],(CvaO,HvaO),Const) :- 
% 	get_attribute(CvaD,dbm_id(Id,_)),
% 	get_attribute(CvaO,dbm_id(Id,_)),
% 	!,
% 	make_const_var(RVars,(CvaO,HvaO),Const).

% make_const_var([(CvaD,HvaD)|RVars],(CvaO,HvaO),Const) :-
% 	(
% 	    difference_constraints_difference(CvaO,CvaD,N),
% 	    difference_constraints_max(CvaO,Max),
% 	    difference_constraints_min(CvaD,Min),
% 	    N > Max - Min ->
% 	    Const = ['#=<'(HvaO - HvaD, N) | RConst]
% 	;
% 	    Const = RConst
% 	),
% 	make_const_var(RVars,(CvaO,HvaO),RConst).
	




