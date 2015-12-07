:- use_module(engine(internals), [term_to_meta/2]).


nonzero(T) :- % = arithm. dif, T=t1-t2
  normalize(T, K, I, H),
  var_with_def(_, nz, K, I, H).

% See system_freeze/2 in nl_eval.pl

:- meta_predicate clpr_freeze(?,goal).

clpr_freeze(Var, Goal) :-
  nonvar(Goal),
  term_to_meta(G,Goal),
  attach_attribute(V, clpr_frozen(V,true,G)),
  Var = V.

% ---------------- curried stuff 0..N

%
% eq_nbs(V): V is unbound, unconstrained
% nz(V):     V can be anything
%
solve_generic_0(eq,          I) :- arith_zero(I).
solve_generic_0(lt,          I) :- arith_eval(I  < 0).
solve_generic_0(le,          I) :- arith_eval(I =< 0).
solve_generic_0(nz(Var),     I) :- \+ arith_zero(I), Var = I.
solve_generic_0(eq_nbs(Var), I) :-
  (float(I) ->
      attach_attribute(Var, float(I))
  ;
      Var = I
  ).

solve_generic_1(Type, I0, V,C) :-
  factor_k(V,C, I1,H),
  arith_eval(I0+I1, I),
  solve_generic(Type, H, I).

solve_generic_2(Type, In, V1,K1, V2,K2) :-
  factor_kk(V1,K1, V2,K2, I,H),
  arith_eval(I+In, Inh),
  solve_generic(Type, H, Inh).

solve_generic_3(Type, In, D,E, H,I, J,K) :-
  factor_k(D, E, F, G),
  factor_kk(H, I, J, K, L, M),
  arith_eval(F+L+In, A),
  add_linear_11(G, M, B),
  solve_generic(Type, B, A).

solve_generic_4(Type, In, C,D, F,G, J,K, L,M) :-
  factor_kk(C, D, F, G, H, I),
  factor_kk(J, K, L, M, N, O),
  arith_eval(H+N+In, A),
  add_linear_11(I, O, B),
  solve_generic(Type, B, A).

solve_generic_5(Type, In, C,D, F,G, K,L, O,P, Q,R) :-
  factor_kk(C, D, F, G, H, I),
  factor_k(K, L, M, N),
  factor_kk(O, P, Q, R, S, T),
  add_linear_11(N, T, V),
  arith_eval(H+M+S+In, A),
  add_linear_11(I, V, B),
  solve_generic(Type, B, A).

% ---------------- curried stuff for M > N

solve_generic_n(N, Type, I, H) :-
  add_f_log(N, H, I1,H1, []),
  arith_eval(I1+I, I2),
  solve_generic(Type, H1, I2).

%
% eq_nbs(V): V is unbound, unconstrained
% nz(V):     V can be anything
%
solve_generic(eq,        H, I) :-     solve_lin(H, I).
solve_generic(le,        H, I) :- solve_ineq_le(H, I).
solve_generic(lt,        H, I) :- solve_ineq_lt(H, I).
solve_generic(nz(V),     H, I) :- var_with_def(New, nz, 1, I, H), New=V.
solve_generic(eq_nbs(V), H, I) :- solve_eq_nbs(H, I, V, v).


