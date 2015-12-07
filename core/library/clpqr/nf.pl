%
% operations on/with clp(r,q) normalforms
%

:- use_module(library(lists)).

% -------------------------------- normalization ------------------------------

% normalize(+Term, -Var)
% compute linear nf of Term and equate with free Var
% Part of eval/1
%
normalize(Term, Var) :-
  type(Term, Type),
  normalize(Type, Term, K, I, H),
  var_with_def(Var, v, K, I, H).

normalize(Term, Inhom, Hom) :-
  type(Term, Type),
  normalize(Type, Term, K, I, H),
  arith_eval(K*I, Inhom),
  mult_linear_factor(H, K, Hom).

% normalize(Term, Koeff, Inhom, Hom)
% left in for compatibility --- most calls are unfolded to normalize/5
%
normalize(Term, K, I, H) :-
  type(Term, Type),
  normalize(Type, Term, K,I,H).

normalize(list,      _, _, _, _) :- !, fail.
normalize(integer,   I, 1, I, []) :- !.
normalize(float,     I, 1, I, []) :- !.
normalize(atom,      Term, 1, 0, [Term*1]) :- !.		% constants: (e.g. eps)
normalize(structure, Term, K, I, H) :- !, normalize_structure(Term, K, I, H).
normalize(attv,      X, K, I, H) :-  get_attribute(X, M), !, normalize_meta(M, K, I, H).
% Default case: X is a variable without local attribute (_Type is var o attv.).
normalize(_Type,     X, 1, 0, [X*1]) :- eqn_var_new(v, X).
 
normalize_structure(rat(N,D), 1, rat(N,D), []).
%
normalize_structure(A+B, 1, Inhom, Hom) :-
  type(A,At), normalize(At, A, Ka, Ia, Ha),
  type(B,Bt), normalize(Bt, B, Kb, Ib, Hb),
  arith_eval(Ka*Ia + Kb*Ib, Inhom),
  add_linear_ff(Ha, Ka, Hb, Kb, Hom).
normalize_structure(A-B, 1, Inhom, Hom) :-
  type(A,At), normalize(At, A, Ka, Ia, Ha),
  type(B,Bt), normalize(Bt, B, Kb, Ib, Hb),
  arith_eval(Ka*Ia - Kb*Ib, Inhom),
  arith_eval(-Kb, Kbm),
  add_linear_ff(Ha, Ka, Hb, Kbm, Hom).
%
normalize_structure(A*B, K, Inhom, Hom) :-
  normalize_mult(A, B, K, Inhom, Hom).
normalize_structure(A/B, K, Inhom, Hom) :- !,
  normalize_div(A, B, K, Inhom, Hom).
%
normalize_structure(-A, K, Inhom, Hom) :-
  type(A,At), normalize(At, A, Ka, Inhom, Hom),
  arith_eval(-Ka, K).
normalize_structure(+A, K, Inhom, Hom) :-
  type(A,At), normalize(At, A, K, Inhom, Hom).
%
normalize_structure(abs(A),   K, Inhom, Hom) :-
  normalize_abs(A, K, Inhom, Hom).
%
normalize_structure(sign(A),  K, Inhom, Hom) :-
  normalize_sign(A, K, Inhom, Hom).
%
normalize_structure(min(A,B), K, Inhom, Hom) :-
  normalize_mix(min, A, B, K, Inhom, Hom).
normalize_structure(max(A,B), K, Inhom, Hom) :-
  normalize_mix(max, A, B, K, Inhom, Hom).
%
normalize_structure(pow(A,B), K, Inhom, Hom) :-
  normalize_pow(A, B, K, Inhom, Hom).
normalize_structure(A**B, K, Inhom, Hom) :-
  normalize_pow(A, B, K, Inhom, Hom).
normalize_structure(sqrt(A), K, Inhom, Hom) :-
  normalize_pow(A, 1/2, K, Inhom, Hom).
%
normalize_structure(sin(A),   K, Inhom, Hom) :-
  normalize_trig(sin, A, K, Inhom, Hom).
normalize_structure(cos(A),   K, Inhom, Hom) :-
  normalize_trig(cos, A, K, Inhom, Hom).
normalize_structure(tan(A),   K, Inhom, Hom) :-
  normalize_trig(tan, A, K, Inhom, Hom).
normalize_structure(asin(A),   K, Inhom, Hom) :-
  normalize_trig(asin, A, K, Inhom, Hom).
normalize_structure(acos(A),   K, Inhom, Hom) :-
  normalize_trig(acos, A, K, Inhom, Hom).
normalize_structure(atan(A),   K, Inhom, Hom) :-
  normalize_trig(atan, A, K, Inhom, Hom).

normalize_meta(clpr_frozen(V,_,_),   1, 0, [X*1]) :- eqn_var_new(v, X), X=V.
normalize_meta(eqn_var(_,_,Lin,_,_), 1, I, H) :- get_attribute(Lin, I+H).
normalize_meta(float(I),             1, I, []).
normalize_meta(term_wrap(_Self,T),   K, I, H) :-
  type(T,Tt), normalize(Tt, T, K, I, H).

% like normalize, but does not see any terms, i.e., this is the runtime version
%
factor(V, I, H) :-
  type(V, Type),
  factor(Type, V, I, H).

factor(list,      _, _, []) :- !, fail.
factor(integer,   N, N, []) :- !.
factor(float,     N, N, []) :- !.
factor(atom,      A, 0, [A*1]) :-!.
factor(structure, S, S, []) :- !, S=rat(_,_).
factor(attv,      V, I, H) :-
  get_attribute(V, M), !,
  normalize_meta(M, K0, I0, H0),
  arith_eval(K0*I0, I),
  mult_linear_factor(H0, K0, H).
% Default case: V is a variable without local attribute (_Type is var o attv.).
factor(_Type,      V, 0, [V*1]) :- eqn_var_new(v, V).

factor_k(V, K, I, H) :-
  type(V, Type),
  factor(Type, V, K, I, H).

factor(list,      _, _, _, _) :- !, fail. 
factor(integer,   N, K, I, []) :- !, arith_eval(K*N, I).
factor(float,     N, K, I, []) :- !, arith_eval(K*N, I).
factor(atom,      A, K, 0, [A*K]).
factor(structure, S, K, I, []) :- !, S=rat(_,_), arith_eval(K*S, I).
factor(attv,      V, K, I, H) :- 
  get_attribute(V, M), !,
  normalize_meta(M, K0, I0, H0),
  arith_eval(K*K0, K1),
  arith_eval(K1*I0, I),
  mult_linear_factor(H0, K1, H).
% Default case: V is a variable without local attribute (_Type is var o attv.).
factor(_Type,     V, K, 0, [V*K]) :- eqn_var_new(v, V).

factor_kk(A,Ka, B,Kb, I,H) :-
  factor_k(A,Ka, Ia,Ha),
  factor_k(B,Kb, Ib,Hb),
  arith_eval(Ia+Ib, I),
  add_linear_11(Ha, Hb, H).

add_f_log(1, [D*E|C], A, B, C) :- !,
        factor_k(D, E, A, B).
add_f_log(2, [D*E,F*G|C], A, B, C) :- !,
        factor_kk(D, E, F, G, A, B).
add_f_log(3, [D*E,F*G,H*I|C], A, B, C) :- !,
        factor_k(D, E, J, K),
        factor_kk(F, G, H, I, L, M),
        add_linear_11(K, M, B),
        arith_eval(J+L, A).
add_f_log(4, [D*E,F*G,H*I,J*K|C], A, B, C) :- !,
        factor_kk(D, E, F, G, L, M),
        factor_kk(H, I, J, K, N, O),
        arith_eval(L+N, A),
        add_linear_11(M, O, B).
add_f_log(5, [D*E,F*G,H*I,J*K,L*M|C], A, B, C) :- !,
        factor_kk(D, E, F, G, N, O),
        factor_k(H, I, P, Q),
        factor_kk(J, K, L, M, R, S),
        add_linear_11(Q, S, T),
        arith_eval(N+P+R, A),
        add_linear_11(O, T, B).
add_f_log(A, B, C, D, E) :-
        F is A>>1,
        G is A-F,
        add_f_log(F, B, H, I, J),
        add_f_log(G, J, K, L, E),
        arith_eval(H+K, C),
        add_linear_11(I, L, D).

% ------------------------ lower level linear term ops -------------------------
%

% replace V in H by its new definition, Vh+Vi
%
nf_substitute(V, Vi, Vh, I, H, I1, H1) :-
  delete_factor_ordered(V, H, Hmv, K),
  arith_eval(K*Vi+I, I1),
  % to test for K=1 and calling add_linear_11 is not faster
  add_linear_1f(Hmv, Vh, K, H1).

delete_factor_ordered(Vid, [Car|Cdr], RCdr, RKoeff) :-
  Car = Var*Koeff,
  compare(Rel, Var, Vid),
  ( Rel= =,
    RCdr = Cdr, RKoeff=Koeff
  ; Rel= >,
    RCdr = [Car|RCdr1],
    delete_factor_ordered(Vid, Cdr, RCdr1, RKoeff)
  ).

% nf_coeff_of(Nf, X, Coeff)
% determine the coeff of variable X in Nf
% fails if X is not a member of the Nf
%
nf_coeff_of([Var*K|Vs], Vid, Coeff) :-
  compare(Rel, Var, Vid),
  ( Rel= =, Coeff = K
  ; Rel= >, nf_coeff_of(Vs, Vid, Coeff)
  ).
/*
% nf_coeffs(Nf, Xs, Cs)
% determine the coeffs of all Xs (ordered as Nf)
%
nf_coeffs([], Xs, Cs) :- !, nf_coeffs(Xs, Cs).
nf_coeffs(_,  [], []) :- !.
nf_coeffs([V*C|Vs], [X|Xs], Cs) :-
  compare(Rel, V, X),
  ( Rel = =, Cs = [C|Csr], nf_coeffs(Vs, Xs, Csr)
  ; Rel = <, Cs = [0|Csr], nf_coeffs([V*C|Vs], Xs, Csr)
  ; Rel = >,               nf_coeffs(Vs, [X|Xs], Cs)
  ).

nf_coeffs([],     []).
nf_coeffs([_|Xs], [0|Cs]) :-
  nf_coeffs(Xs, Cs).

delete_ordered(Vid, [Car|Cdr], RCdr) :-
  compare(Rel, Car, Vid),
  ( Rel= =,
    RCdr = Cdr
  ; Rel= >,
    RCdr = [Car|RCdr1],
    delete_ordered(Vid, Cdr, RCdr1)
  ).
*/

%
% sort a list (of vars) into nf order
%
nf_order([], []) :- !.
nf_order(L, Ls) :-
  length(L, N),
  nf_order_log(N, L, [], Ls).

nf_order_log(1, [X|T], T, [X]) :- !.
nf_order_log(N, L1, L3, S3) :-
  P is N>>1,
  Q is N-P,
  nf_order_log(P, L1, L2, S1),
  nf_order_log(Q, L2, L3, S2),
  nf_order_merge(S1, S2, S3).

nf_order_merge([], L, L) :- !.
nf_order_merge(L, [], L) :- !.
nf_order_merge([A|As], [B|Bs], Res) :-
  compare(Rel, A, B),
  ( Rel = =, Res = [A|Rest], nf_order_merge(As, Bs, Rest)
  ; Rel = <, Res = [B|Rest], nf_order_merge([A|As], Bs, Rest)
  ; Rel = >, Res = [A|Rest], nf_order_merge(As, [B|Bs], Rest)
  ).

/* Unused - bardo
% delete_factors(+Nf, +Vars, -Nf1, -Koeffs)
%
%
delete_factors(Set1, [],    Set1, []) :- !.
delete_factors([],   Vars,  [],   Ks) :- !, delete_factors(Vars, Ks).
delete_factors([Head1|Tail1], [V2|Tail2], Difference, Ks) :-
        Head1 = V1*_,
	compare(Order, V2, V1),
	delete_factors(Order, Head1, Tail1, V2, Tail2, Difference, Ks).

delete_factors(=, _*K,     Tail1, _,     Tail2, Difference, [K|Ks]) :-
	delete_factors(Tail1, Tail2, Difference, Ks).
delete_factors(<, Head1, Tail1, Head2, Tail2, [Head1|Difference], Ks) :-
	delete_factors(Tail1, [Head2|Tail2], Difference, Ks).
delete_factors(>, Head1, Tail1, _,     Tail2, Difference, [0|Ks]) :-
	delete_factors([Head1|Tail1], Tail2, Difference, Ks).
*/

add_linear_ff([], _, A, B, C) :- !,
        mult_linear_factor(A, B, C).
add_linear_ff(A, B, [], _, C) :- !,
        mult_linear_factor(A, B, C).
add_linear_ff([E*F|D], A, [H*I|G], B, C) :-
        compare(J, H, E),
        (  J= =,
	    arith_eval(A*F,K1),
	    arith_eval(B*I,K2),
	    arith_eval(K1+K2, K),
            (  arith_eval(K1=:= -K2) ->
                C=L
            ;   C=[E*K|L]
            ),
            add_linear_ff(D, A, G, B, L)
        ;   J= <,
            C=[E*N|M],
            arith_eval(A*F, N),
            add_linear_ff(D, A, [H*I|G], B, M)
        ;   J= >,
            C=[H*P|O],
            arith_eval(B*I, P),
            add_linear_ff([E*F|D], A, G, B, O)
        ).

% specialized versions thereof:
% add_linear_11(A, B, Res) :- add_linear_ff(A, 1, B, 1, Res).
%
add_linear_11([], A, A) :- !.
add_linear_11(A, [], A) :- !.
add_linear_11([E*F|D], [H*I|G], C) :-
        compare(J, H, E),
        (  J= =,
	    arith_eval(F, K1),
	    arith_eval(I, K2),
            arith_eval(K1+K2, K),
            (  arith_eval(K1=:= -K2) ->
                C=L
            ;   C=[E*K|L]
            ),
            add_linear_11(D, G, L)
        ;   J= <,
            C=[E*F|M],
            add_linear_11(D, [H*I|G], M)
        ;   J= >,
            C=[H*I|O],
            add_linear_11([E*F|D], G, O)
        ).

% add_linear_1f(A, B, K, Res) :- add_linear_ff(A, 1, B, K, Res).
%
add_linear_1f([], A, B, C) :- !,
        mult_linear_factor(A, B, C).
add_linear_1f(A, [], _, A) :- !.
add_linear_1f([E*F|D], [H*I|G], B, C) :-
        compare(J, H, E),
        (  J= =,
            arith_eval(F, K1),
	    arith_eval(B*I, K2),
            arith_eval(K1+K2, K),
            (  arith_eval(K1=:= -K2) ->
                C=L
            ;   C=[E*K|L]
            ),
            add_linear_1f(D, G, B, L)
        ;   J= <,
            C=[E*F|M],
            add_linear_1f(D, [H*I|G], B, M)
        ;   J= >,
            C=[H*P|O],
            arith_eval(B*I, P),
            add_linear_1f([E*F|D], G, B, O)
        ).

mult_linear_factor([], _, []).		% quite common
mult_linear_factor([H|T],  K, L ) :-
  ( arith_eval(K=:=1) ->        		% avoid to copy
       L = [H|T]
  ;
       H = A*Fa,
       arith_eval(K*Fa, Fan),
       L = [A*Fan|T1],
       mult_linear_factor1(T, K, T1)
  ).

mult_linear_factor1([], _, []).
mult_linear_factor1([A*Fa|As], F, [A*Fan|Afs]) :-
  arith_eval(F*Fa, Fan),
  mult_linear_factor1(As, F, Afs).
