%
% quantifier elimination for sets of linear inequalities
%

% :- op(550, xfy, (:)).

/*
	elim seq X:
		min X: pos x neg occ heuristic
		generate crossproduct for x
        	detach pos, neg
		recompute pos, neg for X\x
*/


fm_elim( [],     _,   _).
fm_elim( [V|Vs], Eqs, Indeps) :-
  select_min_occ( [V|Vs], Eqs, Indeps, Min, Rest),
  elim_min( Min, Eqs),
  fm_elim( Rest, Eqs, Indeps).

/*
select_min_occ( [V|Vs], Eqs, Indeps, V-Pos-Neg, Vs) :- !,	% without heuristic
  make_indep( Indeps, Indeps),
  occurences_v( V, Pos, Neg).
*/
select_min_occ( Vs, Eqs, Indeps, Min, Rest) :- 			% with heuristic
  make_indep( Indeps, Indeps),
  occurences( Vs, Eqs, Occs),					% nl, pp_occ( Occs),
  Occs = [O|Os],
  sel_min( Os, O, Min),
  Min = Sel-_-_,
  delsel( Vs, Sel, Rest).

delsel( [],          _,  []).
delsel( [Head|Tail], El, Res) :-
  ( Head == El ->
      Res = Tail
  ;
      Res = [Head|Ntail],
      delsel( Tail, El, Ntail)
  ).

% pp_occ( []).
% pp_occ( [V-Pos-Neg|Vs]) :-
%   length( Pos, Lp),
%   length( Neg, Ln),
%   write( V-Lp-Ln), nl,
%   pp_occ( Vs).

elim_min( _-Pos-Neg, Eqs) :-
  get_attribute( Eqs, p(EqsType,DeOld,Det,In,Int)),
  crossproduct( Pos, Neg, New, []),
  elim_detach( Pos),
  elim_detach( Neg),
  strip_dep( DeOld, DeRemain),
  activate_crossproduct( New, Eqs, De, DeRemain),
  update_attribute( Eqs, p(EqsType,De,Det,In,Int)),
  simplex_restore_ineqs( Eqs).

simplex_restore_ineqs( Eqs) :-
  simplex_reconsider( 1, Eqs),
  remove_redundancy( Eqs).

elim_detach( []).
elim_detach( [':'(Ineq,_)|Ineqs]) :-
  detach_attribute( Ineq),
  elim_detach( Ineqs).

%       -) bulk addition without sat check (guaranteed) but
%          with redundancy check
% TODO  -) remove syntactic redundancy first ?!!
%       -) avoid the construction of the crossproduct list
%
activate_crossproduct( [],                   _,   Vs1, Vs1).
activate_crossproduct( [l(Type,Inh,Hom)|Ls], Ref, Vs2, Vs1) :-
  ( Hom = [] ->
     activate_crossproduct( Ls, Ref, Vs2, Vs1)
  ;
     attach_attribute( Var, eqn_var(Var,Type,Lin,Ref,_)),
     attach_attribute( Lin, Inh+Hom),
     activate_crossproduct( Ls, Ref, Vs2, [Var|Vs1])
  ).

crossproduct( [], _) --> [].
crossproduct( [A|As], Bs) -->
  crossproduct_one( Bs, A),
  crossproduct( As, Bs).

crossproduct_one( [], _) --> [].
crossproduct_one( [':'(B,Kb)|Bs], ':'(A,Ka)) -->
  {
    arith_eval( -Kb/Ka, Coeff),
    new_ineq( Coeff, A, B, New)
  },
  [ New ],
  crossproduct_one( Bs, ':'(A,Ka)).

new_ineq( K, A, B, l(Tnew,I,H)) :-
  get_attribute( A, eqn_var(_,l(Ta),Av,_,_)),
  get_attribute( Av, Ia+Ha),
  get_attribute( B, eqn_var(_,l(Tb),Bv,_,_)),
  get_attribute( Bv, Ib+Hb),
  arith_eval( K*Ia+Ib, I),
  add_linear_1f( Hb, Ha, K, H),
  new_ineq_jt( Ta, Tb, Tnew).

new_ineq_jt( e, e, l(e)) :- !.
new_ineq_jt( _, _, l(t)).

% ------------------------------- heuristic ---------------------------------

sel_min( [], Min, Min).
sel_min( [X|Xs], Y, Min) :-
  compare_crossp( Rel, X, Y),
  ( Rel = <, sel_min( Xs, X, Min)
  ; Rel = =, sel_min( Xs, Y, Min)
  ; Rel = >, sel_min( Xs, Y, Min)
  ).

compare_crossp( Rel, _-Pa-Na, _-Pb-Nb) :-
  length( Pa, Pal),
  length( Na, Nal),
  length( Pb, Pbl),
  length( Nb, Nbl),
  Cpa is Pal*Nal,
  Cpb is Pbl*Nbl,
  compare( Rel, Cpa, Cpb).

% ------------------------------------------------------------------------------
/*
% produce lists of positive and negative ineq occurences
% of the indep variable V
%
occurences_v( V, Pos, Neg) :-
  get_attribute( V, eqn_var(_,v,_,Eqs,_)),
  get_attribute( Eqs, p(_,De,_,_,_)),
  occurences_v( De, V, Pos, Neg).

occurences_v( De,     _, [],  []) :- var( De), !.
occurences_v( [D|De], V, Pos, Neg) :-
  ( get_attribute( D, eqn_var(_,l(_),Lin,_,_)),
    get_attribute( Lin, _+H),
    nf_coeff_of( H, V, K) ->
      ( arith_eval( K<0) ->
          Pos = Post, Neg = [':'(D,K)|Negt]
      ;
          Pos = [':'(D,K)|Post], Neg = Negt
      ),
      occurences_v( De, V, Post, Negt)
  ;
      occurences_v( De, V, Pos, Neg)
  ).
*/

%
% does the same as the above predicate for N variables in parallel
%
occurences( Vs, Eqs, Occs1) :-
  get_attribute( Eqs, p(_,De,_,_,_)),
  nf_order( Vs, Vss), prepare_occs( Vss, Occs0),
  occurences_map( De, Occs0, Occs1).

occurences_map( De,     O1, O1) :- var( De), !.
occurences_map( [D|De], O1, O3) :-
  ( get_attribute( D, eqn_var(_,l(_),Lin,_,_)),
    get_attribute( Lin, _+H) ->
      occurences_merge( O1, H, D, O2),
      occurences_map( De, O2, O3)
  ;
      occurences_map( De, O1, O3)
  ).

prepare_occs( [],     []).
prepare_occs( [V|Vs], [V-[]-[]|Occs]) :-
  prepare_occs( Vs, Occs).

occurences_merge( [],             _,         _,   []) :- !.
occurences_merge( A,              [],        _,   A) :- !.
occurences_merge( [A-Pos-Neg|As], [B*Kb|Bs], Dep, NewA) :-
  compare( Rel, A, B),
  ( Rel = <, occurences_merge( [A-Pos-Neg|As], Bs, Dep, NewA)
  ; Rel = >, NewA = [A-Pos-Neg|NewAtail],
             occurences_merge( As, [B*Kb|Bs], Dep, NewAtail)
  ; Rel = =, ( arith_eval( Kb < 0) ->
		 NewA = [A-Pos-[':'(Dep,Kb)|Neg] | NewAtail]
             ;
                 NewA = [A-[':'(Dep,Kb)|Pos]-Neg | NewAtail]
             ),
	     occurences_merge( As, Bs, Dep, NewAtail)
  ).

% ------------------------------------------------------------------------------

%
% entry
%
simplex_transform( Eqs) :-
  get_attribute( Eqs, p(_,De,_,In,_)),
  simplex_transform_ineqs( De, In, Eqs).

% find an indep le var, find a dep v var which depends on le, swap,
% iterate until no more le's in Indep
%
simplex_transform_ineqs( De, In, Eqs) :-
  ( indep_le( In, I) ->
      ( dep_v_le( De, I, D) ->
	  true
      ; 					% this may happen if redundant ineqs are eliminated
	  var_with_def( D, v, 1, 0, [I*1])
      ),
      swap( D, I, De1, In1),
      simplex_transform_ineqs( De1, In1, Eqs)
  ;
      true
  ).

indep_le( In,     _) :- var(In), !, fail.
indep_le( [I|In], Res) :-
  ( get_attribute( I, eqn_var(_,l(_),_,_,_)) ->
      Res = I
  ;
      indep_le( In, Res)
  ).

dep_v_le( De,     _, _) :- var( De), !, fail.
dep_v_le( [D|De], I, Res) :-
  ( get_attribute( D, eqn_var(_,v,Lin,_,_)),
    get_attribute( Lin, _+H),
    nf_coeff_of( H, I, _) ->
      Res = D
  ;
      dep_v_le( De, I, Res)
  ).
