%
% DUMP Scheme in total:
%   -) unmeta, remeta = copy term + constraints
%   -) activate copy, transform copy constraints (wrapped terms, ...)
%   -) unmeta, map to external representation (nf_to_sum/3 and other cosmetics)
%

% Intermediate terms produced by unmeta that have to be
% covered by remeta and tidy_constraint
%
%	float/1, eqs/3,
%       term_wrap/1, clpr_frozen/1
%       cva/2				% general case
%

:- set_prolog_flag(multi_arity_warnings, off).

:- use_module(library(lists), [length/2]).

clpqr_dump_constraints( Term, Copy, Constraints) :-
  dump_internal( Term, Copy, Cs),
  tidy_constraints( Cs, Constraints).

/*
dump_print( Term) :-
  unmeta( Term, _, Cs, _),
  tidy_constraints( Cs, Constr),
  pplan( Constr).
*/

dump_internal( Term, Copy, Cs2) :-
  unmeta( Term, Copy1, Cs1, RootCvas1),
  debug(['Dump, stage 1: ',RootCvas1,'\n',Cs1]),
  remeta( Cs1),
  trans_ineq( Cs1, RootCvas1),
  make_indep( RootCvas1, RootCvas1), % after all simplex mods
                                     % (creates 'infeasible' rows)
  unmeta( Copy1, Copy, Cs2, RootCvas2),
  debug(['Dump, stage 2: ',RootCvas2,'\n',Cs2]).

% Produce a copy of a (noncyclic) term with all CVA's replaced by fresh
% ordinary vars. 2-phase approach because it helps to know which (cva)
% vars are reachable from the Term
%
unmeta( Term, Copy, MetaPlan, Roots) :-
  ct_top( Term, Copy, Dict, root), 		% nl, pd( Dict, 0), nl,
  ct_eqs( Dict),
  closure2list( Dict, Roots, [], MetaPlan, []).

%
% ignore ordinary vars, drop cva id, fill in locs
%
closure2list( Tree,       Root1, Root1) --> {var(Tree)}, !, [].
closure2list( t(L,Key,R), Root4, Root1) -->
  ( {Key = cva(_,Copy,Loc,Constr)} ->
      { loc( Loc, Copy, Root2, Root1) },
      closure2list_cva( Constr, Copy)
  ; {Key = eqs(_,T,D,I)} ->
      { Root2 = Root1 },
      [ eqs(T,D,I) ]
  ;
      { Root2 = Root1 },
      []
  ),
  closure2list( L, Root3, Root2),
  closure2list( R, Root4, Root3).

closure2list_cva( irrelevant, _)   --> !, [].
closure2list_cva( Constr,     Var) --> [ cva(Var,Constr) ].

%
% arg1 might still be var
%
loc( closure, _, R,     R) :- !.
loc( root,    V, [V|R], R).

% -----------------------------------------------------------------------

ct_top( Term, Copy, Dict, Loc) :-
  type( Term, Type),
  ct_top( Type, Term, Copy, Dict, Loc).

ct_top( integer,   I,      I,        _,    _) :- !.
ct_top( float,     F,      F,        _,    _) :- !.
ct_top( atom,      A,      A,        _,    _) :- !.
ct_top( list,      [X|Xt], [Xc|Xct], Dict, Loc) :- !,
  ct_top( X, Xc, Dict, Loc),
  ct_top( Xt, Xct, Dict, Loc).
ct_top( structure, Term,   Copy,     Dict, Loc) :- !,
  functor( Term, N, A),
  functor( Copy, N, A),
  ct_top_args( A, Copy, Dict, Term, Loc).
ct_top( attv,       Cva,    Copy,     Dict, Loc) :- 
  get_attribute(Cva, Attrib), !, 
  dict_insert( Dict, cva(Cva,Copy,OLoc,Attcopy), Stat),
  ( Loc = closure
  ; Loc = root, OLoc = root
  ),
  ( Stat = old
  ; Stat = new, closure( Attrib, Dict, Attcopy)
  ).
% Default case: V is var without local attribute (_Type is var o attv.).
ct_top( _Type,     V,      Copy,     Dict, _) :-
  dict_insert( Dict, v(V,Copy), _).

ct_top_args( 0, _,    _,    _,    _) :- !.
ct_top_args( N, Copy, Dict, Term, Loc) :-
  N1 is N-1,
  arg( N, Copy, Ac),
  arg( N, Term, At),
  ct_top( At, Ac, Dict, Loc),
  ct_top_args( N1, Copy, Dict, Term, Loc).

% ---------------------------------- closure -----------------------------------

closure( float(F), _, float(F)) :- !.
closure( term_wrap(_,Term), Dict, term_wrap(Copy)) :- !,
  ct_top( Term, Copy, Dict, closure).
closure( eqn_var(_,_,_,Eqs,_), Dict, irrelevant) :- !,
  dict_insert( Dict, eqs(Eqs,_,_,_), _).
closure( clpr_frozen(_,S,U), Dict, clpr_frozen(Goals)) :- !,
  closure_nl( S, U, Dict, Goals).
closure( Attribute,          Dict, cva(Copy)) :-    % general case, last clause
  ct_top( Attribute, Copy, Dict, closure).

% --------------------------- second dump pass for eqs -------------------------

%
%  could use a Q instead of fixed point iteration
%
ct_eqs( Dict) :-
  ct_eqs( Dict, Dict, Applied),
  ( var(Applied) ->
      true
  ;
      ct_eqs( Dict)
  ).

ct_eqs( T,          _,    _) :- var( T), !.
ct_eqs( t(L,Key,R), Dict, Applied) :-
  ( Key = eqs(Eqs,T2,Dep,Indep), var(T2) ->			% a new one
      Applied = true,
      get_attribute( Eqs, p(_,De,_,In,_)),
      eqn_type_mask( v, T0),					% solve.pl
      closure_dep( De, Dict, Dep, T0, T1),
      closure_indep( In, Dict, Indep, T1, T2)
  ;
      true
  ),
  ct_eqs( L, Dict, Applied),
  ct_eqs( R, Dict, Applied).


%
% dependent vars are collected if
%
%   -)    they are constrained to be less (equal) zero
%   -) or they are constrained to be nonzero
%   -) or they have associated nonlinear constraints
%   -) or they appear directly in the term to be dumped
%   -) or they are reachable from 'outside' the eqs (wrapped terms)
%
closure_dep( De,     _,    [],  T1, T1) :- var( De), !.
closure_dep( [D|De], Dict, Res, T1, T3) :-
  ( get_attribute( D, eqn_var(_,T,Def,_,Nl)),
    dict_insert( Dict, cva(D,Copy,Loc,irrelevant), Stat),
    closure_nl_att( Nl, Dict, Nc),
    ( T = l(_)
    ; T = nz
    ; Nc = nl(_,_)
    ; Loc == root						% don't unify !!!
    ; Stat = old
    ) ->
      eqn_type_mask( T, Tm1),					% solve.pl
      eqn_type_mask( Nc, Tm2),
      join_eqn_types( [T1,Tm1,Tm2], T2),
      get_attribute( Def, I+H),
      closure_dep_hom( H, Dict, Hom),
      Res = [dep(Copy,T,I,Hom,Nc)|Rest],
      closure_dep( De, Dict, Rest, T2, T3)
  ;
      closure_dep( De, Dict, Res, T1, T3)
  ).

closure_indep( Is,     _,    [],                      T1, T1) :- var( Is), !.
closure_indep( [I|Is], Dict, [indep(Copy,T,Nc)|Rest], T1, T3) :-
  get_attribute( I, eqn_var(_,T,_,_,Nl)),
  dict_insert( Dict, cva(I,Copy,_,irrelevant), _),
  closure_nl_att( Nl, Dict, Nc),
  eqn_type_mask( T, Tm1),					% solve.pl
  eqn_type_mask( Nc, Tm2),
  join_eqn_types( [T1,Tm1,Tm2], T2),
  closure_indep( Is, Dict, Rest, T2, T3).

closure_dep_hom( [],       _,    []).
closure_dep_hom( [V*K|Vs], Dict, [Copy*K|Cs]) :-
  ct_top( V, Copy, Dict, closure),
  closure_dep_hom( Vs, Dict, Cs).

closure_nl_att( Nla, Dict, Goals) :-
  ( get_attribute( Nla, (S,U)) ->
      closure_nl( S, U, Dict, Goals)
  ;
      Goals = none
  ).

closure_nl( S, U, Dict, Goals) :-
  closure_nl_system( S, Cs, Dict),
  ct_top( U, Cu, Dict, closure),
  ( Cs = true,
    Cu = true ->
      Goals = none
  ;
      Goals = nl(Cs,Cu)
  ).

%
% dump and combine *pending* nonlinear goals
%
closure_nl_system( true,  true, _) :- !.
closure_nl_system( (A,B), Copy, Dict) :- !,
  closure_nl_system( A, Ac, Dict),
  closure_nl_system( B, Bc, Dict),
  join_goals( Ac, Bc, Copy).
closure_nl_system( Goal, Copy, Dict) :-
  arg( 1, Goal, Mutex),					% convention
  ( var( Mutex) ->
      %
      % The double mux/1 wrap is need because Mutex is also copied as part of Goal -
      % therefore it cannot also be used as key into the dictionary
      %
      dict_insert( Dict, mux(mux(Mutex),Copy), Stat),
      ( Stat = old
      ; Stat = new, ct_top( Goal, Copy, Dict, closure)
      )
  ;
      Copy = true
  ).

pending_sys_nl( true) :- !, fail.
pending_sys_nl( (A,B)) :- !,
    pending_sys_nl( A)
  ;
    pending_sys_nl( B).
pending_sys_nl( Goal) :-
  arg( 1, Goal, Mutex),
  var( Mutex).

/*
pplan( []).
pplan( [X|Xs]) :-
  tab(5), display(X), nl,
  pplan( Xs).
*/
% ------------------------------------ remeta ----------------------------------

% First create indep vars with the correct type ->
% don't have to patch this later
%
remeta( Cs) :-
  remeta_eqs( Cs),
  remeta_rest( Cs).

remeta_eqs( []).
remeta_eqs( [X|Xs]) :-
  ( X = eqs(Type,Dep,Indep) ->
     remeta_eqs_indep( Indep, Eqs, In, Int),
     remeta_eqs_dep( Dep, Eqs, De, Det),
     attach_attribute( Eqs, p(Type,De,Det,In,Int)),
     remeta_eqs( Xs)
  ;
     remeta_eqs( Xs)
  ).

remeta_eqs_indep( [],                      _,   T,          T).
remeta_eqs_indep( [indep(Var,Type,Nl)|Vs], Eqs, [Var|Rest], Tail) :-
  attach_attribute( Var, eqn_var(Var,Type,Lin,Eqs,NlAtt)),
  attach_attribute( Lin, 0+[Var*1]),
  remeta_nonlin( Nl, NlAtt),
  remeta_eqs_indep( Vs, Eqs, Rest, Tail).

remeta_eqs_dep( [],                        _,   T,          T).
remeta_eqs_dep( [dep(Var,Type,I,H,Nl)|Vs], Eqs, [Var|Rest], Tail) :-
  length( H, Len),
  remeta_hom( Len, H, [], Hom),
  attach_attribute( Var, eqn_var(Var,Type,Lin,Eqs,NlAtt)),
  attach_attribute( Lin, I+Hom),
  remeta_nonlin( Nl, NlAtt),
  remeta_eqs_dep( Vs, Eqs, Rest, Tail).

remeta_rest( []).
remeta_rest( [X|Xs]) :-
  ( X = cva(V,Attrib) ->
     remeta_rest( Attrib, V),
     remeta_rest( Xs)
  ;
     remeta_rest( Xs)
  ).

% here we (must) assume that Var is a CVA already !!!
%
remeta_rest( float(F), Var) :-
  attach_attribute( X, float(F)), X=Var.
remeta_rest( term_wrap(Exp), Var) :-
  normalize( Exp, X), X=Var.
remeta_rest( clpr_frozen(Goals), Var) :-
  ( Goals = none
  ; Goals = nl(S,U),
      attach_attribute( X, clpr_frozen(X,S,U)), X=Var
  ).
remeta_rest( cva(Attrib), Var) :-			% general case
  attach_attribute( X, Attrib), X=Var.

remeta_hom( 1, [X*K|T], T, [X*K]) :- !.
remeta_hom( 2, [A*Ka,B*Kb|T], T, Sorted) :- !,
  compare( Rel, A, B),
  ( Rel = =, throw(clp_error(equal_in_rel))
  ; Rel = <, Sorted = [B*Kb,A*Ka]
  ; Rel = >, Sorted = [A*Ka,B*Kb]
  ).
remeta_hom( N, Unsorted0, Unsorted2, Sorted) :-
  L is N>>1,
  R is N-L,
  remeta_hom( L, Unsorted0, Unsorted1, S1),
  remeta_hom( R, Unsorted1, Unsorted2, S2),
  add_linear_11( S1, S2, Sorted).

remeta_nonlin( none,    _).
remeta_nonlin( nl(S,U), Att) :-
  attach_attribute( Att, (S,U)).

% ---------------------------------- transformations --------------------------------

%
% turn roots into indep vars (as many as possible)
% TODO: merge with ineq normalization below
%

make_indep( [],     _).
make_indep( [V|Vs], Indep) :-
  ( get_attribute( V, eqn_var(_,_,Lin,_,_)) ->
     get_attribute( Lin, I+H),
     ( indep_var( H, I, V) ->
         true
     ; nonindep( H, Inr, Indep) ->
         swap( V, Inr, _)
     ;
         true
     )
  ;
     true
  ),
  make_indep( Vs, Indep).

nonindep( [V*_|Vs], Inr, Indep) :-
  ( memq( V, Indep) ->
      nonindep( Vs, Inr, Indep)
  ;
      Inr = V
  ).

indep_var( [V*K], I, Self) :-
  Self == V,
  arith_zero( I),
  arith_eval( 1=:=K).

memq( X, [Y|_]) :- X == Y, !.
memq( X, [_|L]) :- memq( X, L).

% ------------------------------- ineq transforms -------------------------

% This assumes that all eqs are reachable from Plan.
% If this does not hold any longer, remeta must be required to return
% a list of newly introduced CVA's
%
trans_ineq( [],     _).
trans_ineq( [P|Ps], Roots) :-
  ( P = eqs(Type,_,[indep(V,_,_)|_]), Type /\ 2'10 =:= 2'10  ->
      get_attribute( V, eqn_var(_,_,_,Eqs,_)),
      remove_redundancy( Eqs),
      trans_ineq_project( Eqs, Roots),
      trans_ineq( Ps, Roots)
  ;
      trans_ineq( Ps, Roots)
  ).

%
% eliminate vars which are not in the projection
% the vars to be eliminated are the indep nonroots
%
trans_ineq_project( Eqs, Roots) :-
  simplex_transform( Eqs),
  make_indep( Roots, Roots),
  get_attribute( Eqs, p(_,_,_,In,_)),
  otl2l( In, Indep),
  collect_elim( In, Roots, Elim),
  fm_elim( Elim, Eqs, Indep).

collect_elim( V,      _,     []) :- var( V), !.
collect_elim( [V|Vs], Roots, Res) :-
  ( memq( V, Roots) ->
      collect_elim( Vs, Roots, Res)
  ; get_attribute( V, eqn_var(_,_,_,_,Nl)),		% hands off if there nonlins
    get_attribute( Nl, (S,U)),
    ( U \== true
    ; pending_sys_nl( S)
    ) ->
      collect_elim( Vs, Roots, Res)
  ;
      Res = [V|R],
      collect_elim( Vs, Roots, R)
  ).

otl2l( Xs,     []) :- var( Xs), !.
otl2l( [X|Xs], [X|Rest]) :-
  otl2l( Xs, Rest).

% -------------------------------- transform --------------------------------

%   tidy: don't exec eqs that occur in user nonlins:
%         [Clp(R)] ?- A=B*B,clpr_freeze(A,test(A,2)).
%         A = B*B,
%         clpr_freeze(B*B,test(B*B,2)) ?

tidy_constraints( Cs, Constr) :-
  tidy_constraints( Cs, Dict, Constr, C2),
  tidy_eqs( Dict, C2, []).

tidy_constraints( [],     _) --> [].
tidy_constraints( [C|Cs], Dict) -->
  ( { C = eqs(_,Dep,Indep) },
      tidy_dep( Dep, Dict),
      tidy_indep( Indep, Dict)
  ; { C = cva(Var,Constr) },
      tidy_constraint( Constr, Var, Dict)
  ),
  tidy_constraints( Cs, Dict).

tidy_constraint( float(F), V, Dict) -->
  tidy_equation( V, F, Dict).
%
tidy_constraint( clpr_frozen(Goals), Var, Dict) -->
  tidy_nonlin( Goals, Var, Dict).
%
tidy_constraint( cva(Attrib), V, Dict) --> 		% general case
  { dict_insert( Dict, eq(V,_,no), _) },
  [ attach_attribute(V,Attrib) ].

tidy_indep( [],                      _) --> [].
tidy_indep( [indep(Var,Type,Nl)|Vs], Dict) -->
  tidy_constraint_indep( Type, Var),
  tidy_nonlin( Nl, Var, Dict),
  tidy_indep( Vs, Dict).

tidy_dep( [],                        _) --> [].
tidy_dep( [dep(Var,Type,I,H,Nl)|Vs], Dict) -->
  tidy_constraint_dep( Type, I, H, Var, Dict),
  tidy_nonlin( Nl, Var, Dict),
  tidy_dep( Vs, Dict).

tidy_nonlin( none,    _,   _)    --> [].
tidy_nonlin( nl(S,U), Var, Dict) -->
  tidy_frozen_system( S, Dict),
  tidy_frozen_user( U, Var, Dict).

tidy_constraint_indep(    v, _) --> [].
tidy_constraint_indep( l(t), V) --> [ V  .<. 0 ].
tidy_constraint_indep( l(e), V) --> [ V .=<. 0 ].
tidy_constraint_indep(   nz, V) --> [ nonzero(V) ].

%
% TODO: all integer format for linear (in) equations
%

tidy_constraint_dep( v,  I, H, V, Dict) -->
  { nf_to_sum( H, I, Sum) },
  tidy_equation( V, Sum, Dict).
tidy_constraint_dep( nz, I, H, V, Dict) -->
  { nf_to_sum( H, I, Sum) },
  tidy_equation( V, Sum, Dict),
  [ nonzero(V) ].
tidy_constraint_dep( l(Type), I, [V*K|Vs], V, _) -->
  {
     ( arith_eval( K > 0) ->
         Rel = <
     ;
         Rel = >
     ),
     arith_eval( -1/K, K1),
     arith_eval( I*K1, I1),
     mult_linear_factor( Vs, K1, H1),
     nf_to_sum( H1, I1, Sum)
  },
  tidy_inequality( Type, Rel, V, Sum).

tidy_inequality( e, <, L, R) --> [ L .=<. R ].
tidy_inequality( t, <, L, R) --> [ L  .<. R ].
tidy_inequality( e, >, L, R) --> [ L .>=. R ].
tidy_inequality( t, >, L, R) --> [ L .>.  R ].

tidy_frozen_system( true,  _) --> !, [].
tidy_frozen_system( (A,B), Dict) --> !,
  tidy_frozen_system( A, Dict),
  tidy_frozen_system( B, Dict).
tidy_frozen_system( Goal, Dict) -->
  ( {
      arg( 1, Goal, Mutex),				% convention
      var( Mutex),					% once only
      Mutex = dumped					% this is *our* copy anyway
    } ->
      tidy_frozen_system_op( Goal, Dict)
  ;
      []
  ).
%

%% The predicate below was previously shared in the SICStus version.  Now 
%% we need a version with (duplicated) clauses for the clp(Q) and clp(R)
%% cases, due to the module system.   See also ctidy_nonlin/3 in 
%% clpcompiler.pl.  DCG & MCL.

%% Code for CLPQ
tidy_frozen_system_op('solver_q:solve_abs'(_,A,B,_), Dict) -->
  tidy_equation( A, abs(B), Dict).
tidy_frozen_system_op('solver_q:solve_sign'(_,A,B,_), Dict) -->
  tidy_equation( A, sign(B), Dict).
tidy_frozen_system_op('solver_q:solve_mix'(_,Mix,A,B,C,_), Dict) -->
  { F =.. [Mix,B,C] },
  tidy_equation( A, F, Dict).
tidy_frozen_system_op('solver_q:solve_mult'(_,A,B,C,_), Dict) -->
  tidy_equation( A, B*C, Dict).
tidy_frozen_system_op('solver_q:solve_pow'(_,A,B,C,_), Dict) -->
  tidy_equation( A, pow(B,C), Dict).
tidy_frozen_system_op('solver_q:solve_trig'(_,Trig,A,B,_), Dict) -->
  { F =.. [Trig,B] },
  tidy_equation( A, F, Dict).

%% Code for CLPR
tidy_frozen_system_op('solver_r:solve_abs'(_,A,B,_), Dict) -->
  tidy_equation( A, abs(B), Dict).
tidy_frozen_system_op('solver_r:solve_sign'(_,A,B,_), Dict) -->
  tidy_equation( A, sign(B), Dict).
tidy_frozen_system_op('solver_r:solve_mix'(_,Mix,A,B,C,_), Dict) -->
  { F =.. [Mix,B,C] },
  tidy_equation( A, F, Dict).
tidy_frozen_system_op('solver_r:solve_mult'(_,A,B,C,_), Dict) -->
  tidy_equation( A, B*C, Dict).
tidy_frozen_system_op('solver_r:solve_pow'(_,A,B,C,_), Dict) -->
  tidy_equation( A, pow(B,C), Dict).
tidy_frozen_system_op('solver_r:solve_trig'(_,Trig,A,B,_), Dict) -->
  { F =.. [Trig,B] },
  tidy_equation( A, F, Dict).

% occur check: X=sin(Y), Y=cos(X).
%
tidy_equation( Lhs, Rhs, Dict) --> [],
  {
    dict_insert( Dict, eq(Lhs,Rh,_), Stat),
    ( Stat = old, app_tail( Rh, Rhs)
    ; Stat = new, Rh = [Rhs|_]
    )
  }.

tidy_eqs( Dict) --> {var(Dict)}, !, [].
%% Changed to display constraints as .=. (DCG)
% tidy_eqs( t(L,eq(Var,Rhs,Flag),R)) -->
%   ( {var(Var),Flag=yes,free_of_var(Var,Rhs)} ->
%       { Rhs = [Var|Tail] },			% execute first
%       tidy_eqs_l( Tail, Var)                   	% collect rest
%   ;
%       tidy_eqs_l(Rhs,Var) 			% collect all
%   ),
%   tidy_eqs( L),
%   tidy_eqs( R).
tidy_eqs( t(L,eq(Var,Rhs,_),R)) -->
  tidy_eqs_l(Rhs,Var),
  tidy_eqs( L),
  tidy_eqs( R).

tidy_eqs_l( L,      _) --> {var(L)}, !, [].
tidy_eqs_l( [R|Rs], Var) -->
  [ Var .=. R ],
  tidy_eqs_l( Rs, Var).

app_tail( Tail, El) :- var( Tail), !, Tail = [El|_].
app_tail( [_|L],El) :- app_tail( L, El).

tidy_frozen_user( true, _, _) --> !, [].
tidy_frozen_user( Goal, V, Dict) -->
  [ clpr_freeze(V,Goal) ],
  { dict_insert( Dict, eq(V,_,no), _) }.

% ------------------------------ dictionary operations -----------------------------

% insert a F(Key,...) elem into the binary tree

dict_insert( Tree, Key, Occ) :- var(Tree), !,
  Tree = t(_,Key,_),
  Occ = new.
dict_insert( t(L,Key0,R), Key, Occ) :-
  arg( 1, Key, Ak),
  arg( 1, Key0, Ak0),
  compare( Rel, Ak, Ak0),
  ( Rel = =, Occ = old, Key = Key0
  ; Rel = <, dict_insert( L, Key, Occ)
  ; Rel = >, dict_insert( R, Key, Occ)
  ).

% pd( Tree, _) :- var( Tree), !.
% pd( t(L,Data,R), Depth) :-
%   D1 is Depth+2,
%   pd( L, D1),
%   tab( Depth), print( Data), nl,
%   pd( R, D1).
/*
free_of_var(Variable, Term) :-
	Term == Variable,
	!,
	fail.
free_of_var(Variable, Term) :-
	nonvar( Term),
	functor(Term, _, Arity),
	Arity > 0,
	!,
	free_of_var(Arity, Term, Variable).
free_of_var(_, _).

free_of_var(1, Term, Variable) :- !,
	arg(1, Term, Argument),
	free_of_var(Variable, Argument).
free_of_var(N, Term, Variable) :-
	arg(N, Term, Argument),
	free_of_var(Variable, Argument),
	M is N-1,
	free_of_var(M, Term, Variable).
*/
% ----------------------------------- support ---------------------------------

nf_to_sum( Hk, Ik, Sum) :-
  ( arith_zero( Ik) ->
      ( Hk = [Var*Kid|Fs] ->
         ( arith_eval( Kid=:=  1) ->
             New = Var
         ; arith_eval( Kid=:= -1) ->
             New = -Var
         ;
             New = Kid*Var
         ),
         nf_to_sum1( Fs, New, Sum)
      ;
         Sum = 0
      )
  ;
      nf_to_sum1( Hk, Ik, Sum)
  ).

nf_to_sum1( [], Term, Term).
nf_to_sum1( [Var*K|Rest], Sofar, Term) :-
  ( arith_eval( K=:=1) ->
      Next = Sofar + Var
  ; arith_eval( K=:= -1) ->
      Next = Sofar - Var
  ; arith_eval( K < 0) ->
      arith_eval( -K, Ka),
      Next = Sofar - Ka*Var
  ;
      Next = Sofar + K*Var
  ),
  nf_to_sum1( Rest, Next, Term).
