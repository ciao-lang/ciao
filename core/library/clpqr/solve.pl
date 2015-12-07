:- use_module(engine(hiord_rt), ['$meta_call'/1]).

% ---------------------------- compiler interface -----------------------------

/*
% generic version
%
wrap_term(MaybeVar, Term) :-
  attach_attribute(X, term_wrap(X,Term)),
  MaybeVar = X.
*/


wrap_term(X, Term) :-
  type(X, Xt),
  wrap_term(Xt, X, Term).

wrap_term(var, X, Term) :- !,
  attach_attribute(X, term_wrap(X,Term)).
wrap_term(Xt, X, Term) :-
  normalize(Xt, X, Kx, Ix, Hx),
  type(Term, Tt), normalize(Tt, Term, Kt, It, Ht),
  arith_eval(Kx*Ix - Kt*It, Inh),
  arith_eval(-Kt, Ktm),
  add_linear_ff(Hx, Kx, Ht, Ktm, Hom),
  solve_lin(Hom, Inh).

/*
solve_lazy_eq(A, B) :-
  normalize(A-B, _, I, H),
  solve_lin(H, I).

solve_lazy_lt(T1, T2) :-
  normalize(T1-T2, I, H),
  solve_ineq_lt(H, I).

solve_lazy_le(T1, T2) :-
  normalize(T1-T2, I, H),
  solve_ineq_le(H, I).
*/

%
% solve_one, solve_two are the interface between nonlinear and linear solver
%

solve_one(V, Const) :-
  factor(V, I,H),
  arith_eval(I-Const, I1),
  solve_lin(H, I1).

solve_two(V1, V2, C) :-	% V1 = V2*C
  (arith_zero(C) ->
      solve_one(V1, 0)
  ;
    factor_kk(V1,-1, V2,C, I,H),
    solve_lin(H, I)
  ).

% ----------------------------------- lin solver ----------------------------------

%
% indeps are free of constants, deps may contain some
% move the pivot variable to the head of deps
%

solve_lin([],    I) :- arith_zero(I).
solve_lin([H|T], I) :- solve_lin_1(T, I, H).

solve_lin_1([],     I, H1) :- solve_lin_1(H1, I).
%
% Pivot is the single member of an eqs, solve for Pivot
%   -> no backsubst required
%   -> no groundings
%
solve_lin_1([H2|T], I, H1) :-
  Hom = [H1,H2|T],
  join_eqs(Hom, Ref, 0,EqsType, De,Det, In,Int, Fresh),
  (Fresh = fresh(Pivot),
      attach_attribute(Ref, p(EqsType,FinDe,Det,In,Int)),
      FinDe = [Pivot|De],
      delete_factor_ordered(Pivot, Hom, Rest, Coeff),
      arith_eval(-I/Coeff, Inh1),
      arith_eval(-1/Coeff, K),
      mult_linear_factor(Rest, K, Hom1),
      get_attribute(Pivot, eqn_var(_,Tm,Lin,_,_)),
      update_attribute(Lin, Inh1+Hom1),
      (Tm = l(_) ->
          ph1(Pivot, FinDe, _)				% simplex
      ;
          true
      )
  ; Fresh = none,
      attach_attribute(Ref, p(EqsType,FinDe,Det,In1,Int)),
      FinDe = [Pivot|De],
      H1 = Pivot*Coeff, Rest = [H2|T],			% take first
      delete_otl(In, Pivot, In1),
      arith_eval(-I/Coeff, Inh1),
      arith_eval(-1/Coeff, K),
      mult_linear_factor(Rest, K, Hom1),
      get_attribute(Pivot, eqn_var(_,Tm,Lin,_,_)),
      (EqsType = 2'00,                    		% our best client
          bs_00(De, Pivot, Inh1, Hom1),
          update_attribute(Lin, Inh1+Hom1)
      ; EqsType = 2'01,
          bs_01(De, Pivot, Inh1, Hom1, G0, (true,true)),
          update_attribute(Lin, Inh1+Hom1),
          '$meta_call'(G0)
      ; EqsType = 2'10,
          bs_10(De, Pivot, Inh1, Hom1, InfN),
          update_attribute(Lin, Inh1+Hom1),
          (Tm = l(_),
            arith_eval(Inh1>=0) ->
              Inf is 1\/InfN
          ;   Inf = InfN
          ),
          simplex_reconsider(Inf, Ref)
      ; EqsType = 2'11,
          bs_11(De, Pivot, Inh1, Hom1, G0, (true,true), InfN),
          update_attribute(Lin, Inh1+Hom1),
          (Tm = l(_),
            arith_eval(Inh1>=0) ->
              Inf is 1\/InfN
          ;   Inf = InfN
          ),
          simplex_reconsider(Inf, Ref),
          '$meta_call'(G0)
      )
  ).

% the use of strip_dep/2 is not too expensive here as this
% is is not called so frequently ...
%
solve_lin_1(B*C, A) :-
        get_attribute(B, eqn_var(_,V,_,D,S)),
           get_attribute(D, p(F,K, N,L,O)),
        update_attribute(D, p(F,Kf,N,H,O)),
        delete_otl(L, B, H),
        arith_eval(-(A/C), P),
        (  F=0,
            bs_00(K, B, P, []),
	    strip_dep(K, Kf),
            ground_meta(B, P)
        ;   F=1,
            bs_01(K, B, P, [], Q, (true,true)),
	    strip_dep(K, Kf),
            collect_nls(S, Q, T),
            ground_meta(B, P),
            '$meta_call'(T)
        ;   F=2,
            guard_slack(V, P),
            bs_10(K, B, P, [], U),
	    strip_dep(K, Kf),
            ground_meta(B, P),
            simplex_reconsider(U, D)
        ;   F=3,
            guard_slack(V, P),
            bs_11(K, B, P, [], X, (true,true), Y),
	    strip_dep(K, Kf),
            collect_nls(S, X, B1),
            ground_meta(B, P),
            simplex_reconsider(Y, D),
            '$meta_call'(B1)
        ).

strip_dep(De, De) :- var(De), !.
strip_dep([D|De], Fix) :-
  (get_attribute(D, eqn_var(_,_,_,_,_)) ->
      Fix = [D|F],
      strip_dep(De, F)
  ;
      strip_dep(De, Fix)
  ).

% solve_eq_nbs(+,+,-,+)
%
% Var is not only the single member of an eqs - it is even a free variable
%
solve_eq_nbs([], I, Var, Type) :- !,
  guard_slack(Type, I),
  (float(I) ->
      attach_attribute(Var, float(I))
  ;
      Var = I
  ).
solve_eq_nbs(Hom, Inh, Var, Type) :-
  eqn_type_mask(Type, Mask),
  collect_equate_eqs(Hom, Ref, Mask,EqsType, De,Det, In,Int),
  Dep = [Var|De],
  attach_attribute(Ref, p(EqsType,Dep,Det,In,Int)),
  attach_attribute(Var, eqn_var(Var,Type,Lin,Ref,_)),
  attach_attribute(Lin, Inh+Hom),
  (Type = l(_) ->
      ph1(Var, Dep, _)					% simplex
  ;
      true
  ).

bs_00(V, _, _, _) :- var(V), !.
bs_00([V|Vs], Mark, Inh, Hom) :-
        (  get_attribute(V, eqn_var(_,_,Lin,_,_)),
            get_attribute(Lin, K+L),
	    nf_substitute(Mark, Inh, Hom, K, L, O, P) ->
            (  P=[] ->
                ground_meta(V, O),
                bs_00(Vs, Mark, Inh, Hom)
            ;
  		update_attribute(Lin, O+P),
                bs_00(Vs, Mark, Inh, Hom)
            )
        ;   bs_00(Vs, Mark, Inh, Hom)
        ).

/*
bs_first_00([], Inh, V) :- !,
  ground_meta(V, Inh).
bs_first_00(Hom, Inh, V) :-
  get_attribute(V, eqn_var(_,_,Lin,_,_)),
  update_attribute(Lin, Inh+Hom).
*/

bs_01(V,      _,    _,   _,   G1, G1) :- var(V), !.
bs_01([V|Vs], Mark, Inh, Hom, G3, G1) :-
        (  get_attribute(V, eqn_var(_,_,Lin,_,Nl)),
            get_attribute(Lin, K+L),
	    nf_substitute(Mark, Inh, Hom, K, L, O, P) ->
            (  P=[] ->
                ground_meta(V, O),
		collect_nls(Nl, G1, G2),
                bs_01(Vs, Mark, Inh, Hom, G3, G2)
            ;
  		update_attribute(Lin, O+P),
                bs_01(Vs, Mark, Inh, Hom, G3, G1)
            )
        ;   bs_01(Vs, Mark, Inh, Hom, G3, G1)
        ).

/*
bs_first_01([], Inh, V, G2, G1) :- !,
  get_attribute(V, eqn_var(_,_,_,_,Nl)),
  collect_nls(Nl, G1, G2),
  ground_meta(V, Inh).
bs_first_01(Hom, Inh, V, G1, G1) :-
  get_attribute(V, eqn_var(_,_,Lin,_,_)),
  update_attribute(Lin, Inh+Hom).
*/

collect_nls(Nl, Sofar, Goal) :-
  (get_attribute(Nl, (Gs,Gu)) ->
      Sofar = (Gsr,Gur),
      join_goals(Gsr, Gs, Gsystem),
      join_goals(Gur, Gu, Guser),
      Goal = (Gsystem,Guser)
  ;
      Goal = Sofar
  ).

join_goals(true, G, G) :- !.
join_goals(G, true, G) :- !.
join_goals(A, B, (A,B)).

bs_10(V,      _,    _,   _,   Infeasible) :- var(V), (Infeasible = 0 ; Infeasible = 1), !.
bs_10([V|Vs], Mark, Inh, Hom, Infeasible) :-
        (  get_attribute(V, eqn_var(_,T,Lin,_,_)),
            get_attribute(Lin, K+L),
	    nf_substitute(Mark, Inh, Hom, K, L, O, P) ->
            (  P=[] ->
                guard_slack(T, O),
                ground_meta(V, O),
                bs_10(Vs, Mark, Inh, Hom, Infeasible)
	    ;   var(Infeasible),
		T = l(_),
 		arith_eval(O >= 0) ->
 		  Infeasible = 1,
  		  update_attribute(Lin, O+P),
                  bs_10(Vs, Mark, Inh, Hom, Infeasible)
            ;
  		update_attribute(Lin, O+P),
                bs_10(Vs, Mark, Inh, Hom, Infeasible)
            )
        ;   bs_10(Vs, Mark, Inh, Hom, Infeasible)
        ).
/*
bs_first_10([], Inh, V, 0) :- !,
  get_attribute(V, eqn_var(_,T,_,_,_)),
  guard_slack(T, Inh),
  ground_meta(V, Inh).
bs_first_10(Hom, Inh, V, Infeasible) :-
  get_attribute(V, eqn_var(_,Tm,Lin,_,_)),
  update_attribute(Lin, Inh+Hom),
  (Tm = l(_),
    arith_eval(Inh >= 0) ->
      Infeasible = 1
  ;
      Infeasible = 0
  ).
*/
bs_11(V,      _,    _,   _,   G1, G1,  Infeasible) :- var(V), (Infeasible = 0 ; Infeasible = 1), !.
bs_11([V|Vs], Mark, Inh, Hom, G3, G1, Infeasible) :-
        (  get_attribute(V, eqn_var(_,T,Lin,_,Nl)),
            get_attribute(Lin, K+L),
	    nf_substitute(Mark, Inh, Hom, K, L, O, P) ->
            (  P=[] ->
                guard_slack(T, O),
		collect_nls(Nl, G1, G2),
                ground_meta(V, O),
                bs_11(Vs, Mark, Inh, Hom, G3, G2, Infeasible)
	    ;   var(Infeasible),
                T = l(_),
 		arith_eval(O >= 0) ->
 		  Infeasible = 1,
  		  update_attribute(Lin, O+P),
                  bs_11(Vs, Mark, Inh, Hom, G3, G1, Infeasible)
            ;
  		update_attribute(Lin, O+P),
                bs_11(Vs, Mark, Inh, Hom, G3, G1, Infeasible)
            )
        ;   bs_11(Vs, Mark, Inh, Hom, G3, G1, Infeasible)
        ).
/*
bs_first_11([], Inh, V, G2, G1, 0) :- !,
  get_attribute(V, eqn_var(_,T,_,_,Nl)),
  guard_slack(T, Inh),
  collect_nls(Nl, G1, G2),
  ground_meta(V, Inh).
bs_first_11(Hom, Inh, V, G1, G1, Infeasible) :-
  get_attribute(V, eqn_var(_,Tm,Lin,_,_)),
  update_attribute(Lin, Inh+Hom),
  (Tm = l(_),
    arith_eval(Inh >= 0) ->
      Infeasible = 1
  ;
      Infeasible = 0
  ).
*/
% exchange a dependent against an independent variable
% Dep = f(Indep) -> 0 = f(Indep) - Dep
%
swap(A, B, C) :- swap(A, B, C, _).
%
swap(Dep, Indep, Deps, Ins) :-
  get_attribute(Dep, eqn_var(_,_,Lin,Ref,_)),
  get_attribute(Lin, I+H),
  Deps = [Indep|De1], Ins = [Dep|In1],
     get_attribute(Ref, p(Et,De,  Det,In, Int)),
  update_attribute(Ref, p(Et,Deps,Det,Ins,Int)),
  % fix crossref
  delete_otl(De, Dep, De1),
  delete_otl(In, Indep, In1),
  % and now solve for Indep
  add_linear_11(H, [Dep* -1], H1),
  delete_factor_ordered(Indep, H1, Rest, Koeff),
  arith_eval(-I/Koeff, Inh1),
  arith_eval(-1/Koeff, K),
  mult_linear_factor(Rest, K, Hom1),
  swap_bs(De, Indep, Inh1, Hom1),
  get_attribute(Indep, eqn_var(_,_,ILin,_,_)),
  update_attribute(ILin, Inh1+Hom1).

swap_bs(V, _, _, _) :- var(V), !.
swap_bs([V|Vs], Mark, Inh, Hom) :-
        (  get_attribute(V, eqn_var(_,_,Lin,_,_)),
            get_attribute(Lin, K+L),
	    nf_substitute(Mark, Inh, Hom, K, L, O, P) ->
  		update_attribute(Lin, O+P),
                swap_bs(Vs, Mark, Inh, Hom)
        ;
                swap_bs(Vs, Mark, Inh, Hom)
        ).

% ----------------------------------- crossref --------------------------------------

% iterates over hom of the new def, joins the otlists,
% computes the resulting eqs_type
%
collect_equate_eqs([],       _,     Et,  Et,  De,De,     In,In).
collect_equate_eqs([V*_|Vs], Final, Et1, Et2, De,DeTail, In,InTail) :-
  get_attribute(V, eqn_var(_,_,_,Ref,_)),
  (Final == Ref ->
      collect_equate_eqs(Vs, Final, Et1, Et2, De,DeTail, In,InTail)
  ;
      get_attribute(Ref, p(EqsType,De,Det,In,Int)),
      detach_attribute(Ref),
      Ref = Final,
      Et11 is Et1 \/ EqsType,
      collect_equate_eqs(Vs, Final, Et11, Et2, Det,DeTail, Int,InTail)
  ).

% like above, but finds leftmost fresh variable too
%
join_eqs([],       _,     Et,  Et,  De,De,     In,In,     none).
join_eqs([V*_|Vs], Final, Et1, Et2, De,DeTail, In,InTail, Fresh) :-
  get_attribute(V, eqn_var(_,_,_,Ref,_)),
  (Final == Ref ->
      join_eqs(Vs, Final, Et1, Et2, De,DeTail, In,InTail, Fresh)
  ;
      get_attribute(Ref, p(EqsType,Dev,Det,Inv,Int)),
      Et11 is Et1 \/ EqsType,
      detach_attribute(Ref),
      Ref = Final,
      (var(Dev) ->
         Fresh = fresh(V),
         collect_equate_eqs(Vs, Final, Et11, Et2, De,DeTail, In,InTail)
      ;
         Dev = De, Inv = In,
         join_eqs(Vs, Final, Et11, Et2, Det,DeTail, Int,InTail, Fresh)
      )
  ).

delete_otl(L,      _,   _) :- var(L), !, fail.	% safety
% delete_otl(L,      _,   L) :- var(L), !. 		% safety, 2nd interpretation
delete_otl([X|Xs], Var, Xs) :- Var == X, !.		% determinism
delete_otl([X|Xs], Var, [X|Xs1]) :- delete_otl(Xs, Var, Xs1).

% --------------------------------- inequalities ------------------------------

% solve_ineq_le/2 and solve_ineq_lt/2
% are in simplex.pl now

guard_slack(v,      _).
guard_slack(l(T),   I) :- (T = e, arith_eval(I =< 0)
                           ; T = t, arith_eval(I  < 0)
                           ).
guard_slack(nz,     I) :- \+ arith_zero(I).


% ------------------------------------------------------------------------------

% assert: Var is free
%
var_with_def(Var, Type, K, I, H) :-
  arith_eval(I*K, Inh),
  mult_linear_factor(H, K, Hom),
  solve_eq_nbs(Hom, Inh, Var, Type).

% ----------------------------------  var ops -------------------------------

% for exact arithmetic ...

% This version will unify the resulting values so is straightforward
% to use such values in procedures not implemented with clpqr -- EMM.

% ground_meta(M, Val) :-
% 	detach_attribute(M),
% 	M = Val.

% This version will show the solutions as restrictions, but only for
% clpr.  For clpq it have the same effect that the version above
% because float(Val) fail. To use results outside use as_float/2 --EMM.

ground_meta(M, Val) :-
	(
	    float(Val) ->
	    update_attribute(M, float(Val))
	;
	    detach_attribute(M),
	    M = Val
	).

% eqn_var_new(EqType, Self)
% Eqs types:       	0 plain equations
%                       1 with pending nl_goals
%                      10 with inequalities		guard_slack
%
eqn_var_new( v, X) :-
  attach_attribute(Self, eqn_var(Self, v,Lin,Ref,_)),
  attach_attribute(Lin, 0+[Self*1]),
  attach_attribute(Ref, p(2'00,D,D,[Self|It],It)),
  Self = X.
eqn_var_new(l(Strict), X) :-
  attach_attribute(Self, eqn_var(Self,l(Strict),Lin,Ref,_)),
  attach_attribute(Lin, 0+[Self*1]),
  attach_attribute(Ref, p(2'10,D,D,[Self|It],It)),
  Self = X.
eqn_var_new(nz, X) :-
  attach_attribute(Self, eqn_var(Self,nz,Lin,Ref,_)),
  attach_attribute(Lin, 0+[Self*1]),
  attach_attribute(Ref, p(2'10,D,D,[Self|It],It)),
  Self = X.

eqn_type_mask(      v, 2'00).
eqn_type_mask(   l(_), 2'10).
eqn_type_mask(     nz, 2'10).
eqn_type_mask(   none, 2'00).				% nonlin stuff in dumper
eqn_type_mask(nl(_,_), 2'01).

join_eqn_types([T|Ts], Type) :-
  join_eqn_types(Ts, T, Type).

join_eqn_types([],     T,  T).
join_eqn_types([T|Ts], T1, T3) :-
  T2 is T1 \/ T,
  join_eqn_types(Ts, T2, T3).
