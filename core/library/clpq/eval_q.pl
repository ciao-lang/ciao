:- module(eval_q, [
        arith_eps/1, arith_zero/1, arith_eval/2, arith_eval/1,
	as_float/2, float_rat/2,
        'arith_zero-1'/1,
        'arith_zero-1'/2,
        'arith_zero-*1'/2,
        'arith_eval<1'/1,
        'arith_eval<1'/2,
        'arith_eval>1'/1,
        'arith_eval=<1'/1,
        'arith_eval>=1'/1,
        'arith_eval+1'/3,
        'arith_eval+1'/4,
        'arith_eval++1'/4,
        'arith_eval+++1'/5,
        'arith_eval+*1'/4,
        'arith_eval+*1'/5,
        'arith_eval-1'/2,
        'arith_eval-1'/3,
        'arith_eval-*1'/4,
        'arith_eval-*1'/5,
        'arith_eval-/1'/3,
        'arith_eval-/2'/3,
        'arith_eval*1'/3,
        'arith_eval*1'/4,
        'arith_eval/-11'/2
                ], []).

:- use_module(library(clpqr/arith_extra)).
% For rationals

% low level arithmetic for clp(r,q,z)
%
% arith_zero(+Exp)
% arith_eval(+Exp <rel> +Exp)
% arith_eval(+Exp, -Res)

:- set_prolog_flag(multi_arity_warnings, off).

arith_zero(Exp) :-
  rat(Exp, N, _),
  N = 0.

arith_eval(Exp, Ans) :-
  rat(Exp, N, D),
  ratint(D, N, Ans).

arith_eval( X=:=Y) :- arith_zero(X-Y).
arith_eval(X >  Y) :- rat(X, An,Ad), rat(Y, Bn,Bd), comq(An,Ad, Bn,Bd, >).
arith_eval(X =< Y) :- rat(X, An,Ad), rat(Y, Bn,Bd), comQ(An,Ad, Bn,Bd, D), D =<  0.
arith_eval(X <  Y) :- rat(X, An,Ad), rat(Y, Bn,Bd), comq(An,Ad, Bn,Bd, <).
arith_eval(X >= Y) :- rat(X, An,Ad), rat(Y, Bn,Bd), comQ(An,Ad, Bn,Bd, D), D >= 0.

arith_eps(0).

% rational arithm.
% rat(Nom,Denom)
% Sign is the sign of Nom, i.e., Denom > 0
%

% use an integer to represent the rational - if possible

ratint(1, N, N) :- !.
ratint(D, N, rat(N,D)).

/*
% Rational number recognition predicate
rational(V,    _, _) :- var(V), !, fail.
rational(rat(N,D), N, D) :- !.
rational(N,    N, 1) :- integer(N).
*/

rat_runtime(X, N, D) :-
  type(X, Type),
  rt(Type, X, N, D).

rt(structure, rat(N,D), N, D).
rt(integer,   N,    N, 1).
rt(float,   F,    N, D) :- float_rat(F, App), rat(App, N, D).

rat(Var,    _, _) :- var(Var), !, fail.
rat(N,    N, 1) :- integer(N), !.
rat(F,    N, D) :- float(F), !, float_rat(F, App), rat(App, N, D).
rat(rat(N,D), N, D).
%
rat(X+Y, Cn,Cd)     :- rat(X, An,Ad), rat(Y, Bn,Bd), addq(An,Ad, Bn,Bd, Cn,Cd).
rat(X-Y, Cn,Cd)     :- rat(X, An,Ad), rat(Y, Bn,Bd), subq(An,Ad, Bn,Bd, Cn,Cd).
rat(-Y, Cn,Cd)    :- rat(Y, An,Cd), Cn is -An.
rat(X*Y, Cn,Cd)     :- rat(X, An,Ad), rat(Y, Bn,Bd), mulq(An,Ad, Bn,Bd, Cn,Cd).
rat(X/Y, Cn,Cd)     :- rat(X, An,Ad), rat(Y, Bn,Bd), divq(An,Ad, Bn,Bd, Cn,Cd).
rat(exp(X), N, D) :- rat(X, An,Ad), F is exp(An/Ad), float_rat(F, App), rat(App, N, D).
rat(log(X), N, D) :- rat(X, An,Ad), F is log(An/Ad), float_rat(F, App), rat(App, N, D).
rat(sin(X), N, D) :- rat(X, An,Ad), F is sin(An/Ad), float_rat(F, App), rat(App, N, D).
rat(cos(X), N, D) :- rat(X, An,Ad), F is cos(An/Ad), float_rat(F, App), rat(App, N, D).
rat(atan(X), N, D) :- rat(X, An,Ad), F is atan(An/Ad), float_rat(F, App), rat(App, N, D).
rat(**(X,Y), N, D) :- rat(X, An,Ad), rat(Y, Bn,Bd), F is **(An/Ad,Bn/Bd), float_rat(F, App), rat(App, N, D).
% % These are not in and-prolog (yet) *DCG*
% rat(tan(X), N, D)   :- rat(X, An,Ad), F is tan(An/Ad), float_rat(F, App), rat(App, N, D).
% rat(asin(X), N, D)  :- rat(X, An,Ad), F is asin(An/Ad), float_rat(F, App), rat(App, N, D).
% rat(acos(X), N, D)  :- rat(X, An,Ad), F is acos(An/Ad), float_rat(F, App), rat(App, N, D).
rat(tan(X), N, D)   :- rat(X, An,Ad), tan(An/Ad, F), float_rat(F, App), rat(App, N, D).
rat(asin(X), N, D) :- rat(X, An,Ad), asin(An/Ad, F), float_rat(F, App), rat(App, N, D).
rat(acos(X), N, D) :- rat(X, An,Ad), acos(An/Ad, F), float_rat(F, App), rat(App, N, D).
rat(abs(X),  Cn,Cd)   :- rat(X, An,Cd), Cn is abs(An).
rat(min(X,Y), Cn,Cd)  :- rat(X, An,Ad), rat(Y, Bn,Bd),
  ( comq(An,Ad, Bn,Bd, <) -> Cn=An,Cd=Ad; Cn=Bn,Cd=Bd ).
rat(max(X,Y), Cn,Cd)  :- rat(X, An,Ad), rat(Y, Bn,Bd),
  ( comq(An,Ad, Bn,Bd, >) -> Cn=An,Cd=Ad; Cn=Bn,Cd=Bd ).

mulq(Na,Da, Nb,Db, Nc,Dc) :-
  Gcd1 is gcd(Na,Db),
  ( Gcd1 = 1 -> Na1=Na,Db1=Db; Na1 is Na//Gcd1,Db1 is Db//Gcd1 ),
  Gcd2 is gcd(Nb,Da),
  ( Gcd2 = 1 -> Nb1=Nb,Da1=Da; Nb1 is Nb//Gcd2,Da1 is Da//Gcd2 ),
  Nc is Na1 * Nb1,
  Dc is Da1 * Db1.


divq(Na,Da, Nb,Db, Nc,Dc) :-
  Gcd1 is gcd(Na,Nb),
  ( Gcd1 = 1 -> Na1=Na,Nb1=Nb; Na1 is Na//Gcd1,Nb1 is Nb//Gcd1 ),
  Gcd2 is gcd(Da,Db),
  ( Gcd2 = 1 -> Da1=Da,Db1=Db; Da1 is Da//Gcd2,Db1 is Db//Gcd2 ),
  ( Nb1 < 0 ->           % keep denom positive !!!
   Nc is -(Na1 * Db1),
   Dc is Da1 * (-Nb1)
  ;
   Nc is Na1 * Db1,
   Dc is Da1 * Nb1
  ).

addq(Na,Da, Nb,Db, Nc,Dc) :-
  Gcd1 is gcd(Da,Db),
  ( Gcd1 = 1 ->          % This is the case (for random input) with
                         % probability 6/(pi**2).
   Nc is Na*Db + Nb*Da,
   Dc is Da*Db
  ;
   T is Na*(Db//Gcd1) + Nb*(Da//Gcd1),
   Gcd2 is gcd(T,Gcd1),
   Nc is T//Gcd2,
   Dc is Da//Gcd1 * Db//Gcd2
  ).

subq(Na,Da, Nb,Db, Nc,Dc) :-
  Gcd1 is gcd(Da,Db),
  ( Gcd1 = 1 ->          % This is the case (for random input) with
                         % probability 6/(pi**2).
   Nc is Na*Db - Nb*Da,
   Dc is Da*Db
  ;
   T is Na*(Db//Gcd1) - Nb*(Da//Gcd1),
   Gcd2 is gcd(T,Gcd1),
   Nc is T//Gcd2,
   Dc is Da//Gcd1 * Db//Gcd2
  ).


comQ(Na,Da, Nb,Db, X) :-  % return a numeric value
  comq(Na,Da, Nb,Db, S),
  rel_to_sign(S, X).

rel_to_sign(<, -1).
rel_to_sign(=,  0).
rel_to_sign(>,  1).

comq(Na,Da, Nb,Db, S) :-  % todo: avoid multiplication by looking a signs first !!!
  Xa is Na * Db,
  Xb is Nb * Da,
  compare(S, Xa, Xb).


% ----------------------------- float -> rational -----------------------------

float_rat(F, Rat) :-
  float_rat(30, F, F, [], Rat).  	% at most 30 iterations

float_rat(0, _, _,  Cf, Res) :- !, ecf( Cf, Res).
float_rat(N, F, Fo, Cf, Res) :-
  N1 is N-1,
  I is integer(F),
  Cf1 = [I|Cf],
  ecf(Cf1, App), as_float( App, Appf),
  ( Fo =:= Appf -> % 0.0 =:= abs(Fo-Appf)
   % It is 30 - N, format( "~d iterations~n", [It]),
   Res = App
  ;
   R is F-I,
   ( 0.0 =:= R ->
    Res = App
   ;
    F1 is 1/R,
    N1 is N-1,
    float_rat(N1, F1, Fo, Cf1, Res)
   )
  ).

ecf([N|Ns], Res) :-
  ecf(Ns, N, 1, Res).

ecf([], N, D, Res) :- arith_eval(N/D, Res).
ecf([N1|Ns], N, D, Res) :-
  arith_eval(N1*N+D, T),
  ecf(Ns, T, N, Res).

as_float(Exp, Float) :-
  rat(Exp, N, D),
  Float is N/D.

% ------------------------------- PE patterns ---------------------------------

% arith_zero(A-B) :- 'arith_zero-1'(A, B).
% arith_zero(A*B-1) :- 'arith_zero-*1'(A,B).
% arith_zero(A-1) :- 'arith_zero-1'(A).

% arith_eval(A<0)	:-'arith_eval<1'(A).
% arith_eval(A>0)	:-'arith_eval>1'(A).
% arith_eval(A<B)	:-'arith_eval<1'(A,B).
% arith_eval(A=<0):-'arith_eval=<1'(A).
% arith_eval(A>=0):-'arith_eval>=1'(A).
% arith_eval(A-B,C):-'arith_eval-1'(A,B,C).
% arith_eval(A*B,C):-'arith_eval*1'(A,B,C).
% arith_eval(A*B-C,D):-'arith_eval-*1'(A,B,C,D).
% arith_eval(A*B+C*D,E):-'arith_eval+*1'(A,B,C,D,E).
% arith_eval(A*B-C*D,E):-'arith_eval-*1'(A,B,C,D,E).
% arith_eval(A+B,C):-'arith_eval+1'(A,B,C).
% arith_eval(A+B+C,D):-'arith_eval++1'(A,B,C,D).
% arith_eval(A+B+C+D,E):-'arith_eval+++1'(A,B,C,D,E).
% arith_eval(-(A/B),C):-'arith_eval-/1'(A,B,C).
% arith_eval(-1/A,B):-'arith_eval/-11'(A,B).
% arith_eval(-(A/B),C):-'arith_eval-/2'(A,B,C).
% arith_eval(-(A),B):-'arith_eval-1'(A,B).
% arith_eval(A*rat(B,C),D):-'arith_eval*1'(A,B,C,D).
% arith_eval(A*B+C,D):-'arith_eval+*1'(A,B,C,D).
% arith_eval(A+B*C,D):-'arith_eval+1'(A,B,C,D).

% 'arith_zero-1'(A,B):-arith_zero(A-B)
'arith_zero-1'(A, B) :-
    rat_runtime(A, C, D),
    rat_runtime(B, E, F),
    subq(C, D, E, F, G, _),
    G=0.

% 'arith_zero-*1'(A,B):-arith_zero(A*B-1)
'arith_zero-*1'(A, B) :-
    rat_runtime(A, C, D),
    rat_runtime(B, E, F),
    mulq(C, D, E, F, G, H),
    subq(G, H, 1, 1, I, _),
    I=0.

% 'arith_zero-1'(A):-arith_zero(A-1)
'arith_zero-1'(A) :-
    rat_runtime(A, B, C),
    subq(B, C, 1, 1, D, _),
    D=0.

% 'arith_eval<1'(A):-arith_eval(A<0)
'arith_eval<1'(A) :-
    rat_runtime(A, B, _),
    B < 0.

% 'arith_eval>1'(A):-arith_eval(A>0)
'arith_eval>1'(A) :-
    rat_runtime(A, B, _),
    B > 0.

% 'arith_eval<1'(A,B):-arith_eval(A<B)
'arith_eval<1'(A, B) :-
    rat_runtime(A, C, D),
    rat_runtime(B, E, F),
	C*F < D*E.

% 'arith_eval=<1'(A):-arith_eval(A=<0)
'arith_eval=<1'(A) :-
    rat_runtime(A, B, _),
    B =< 0.

% 'arith_eval>=1'(A):-arith_eval(A>=0)
'arith_eval>=1'(A) :-
    rat_runtime(A, B, _),
    0 =< B.

% 'arith_eval-1'(A,B,C):-arith_eval(A-B,C)
'arith_eval-1'(A, B, C) :-
    rat_runtime(A, D, E),
    rat_runtime(B, F, G),
    subq(D, E, F, G, H, I),
    ratint(I, H, C).

% 'arith_eval*1'(A,B,C):-arith_eval(A*B,C)
'arith_eval*1'(A, B, C) :-
    rat_runtime(A, D, E),
    rat_runtime(B, F, G),
    mulq(D, E, F, G, H, I),
    ratint(I, H, C).

% 'arith_eval-*1'(A,B,C,D):-arith_eval(A*B-C,D)
'arith_eval-*1'(A, B, C, D) :-
    rat_runtime(A, E, F),
    rat_runtime(B, G, H),
    mulq(E, F, G, H, I, J),
    rat_runtime(C, K, L),
    subq(I, J, K, L, M, N),
    ratint(N, M, D).

% 'arith_eval+*1'(A,B,C,D,E):-arith_eval(A*B+C*D,E)
'arith_eval+*1'(A, B, C, D, E) :-
    rat_runtime(A, F, G),
    rat_runtime(B, H, I),
    mulq(F, G, H, I, J, K),
    rat_runtime(C, L, M),
    rat_runtime(D, N, O),
    mulq(L, M, N, O, P, Q),
    addq(J, K, P, Q, R, S),
    ratint(S, R, E).

% 'arith_eval-*1'(A,B,C,D,E):-arith_eval(A*B-C*D,E)
'arith_eval-*1'(A, B, C, D, E) :-
    rat_runtime(A, F, G),
    rat_runtime(B, H, I),
    mulq(F, G, H, I, J, K),
    rat_runtime(C, L, M),
    rat_runtime(D, N, O),
    mulq(L, M, N, O, P, Q),
    subq(J, K, P, Q, R, S),
    ratint(S, R, E).

% 'arith_eval+1'(A,B,C):-arith_eval(A+B,C)
'arith_eval+1'(A, B, C) :-
    rat_runtime(A, D, E),
    rat_runtime(B, F, G),
    addq(D, E, F, G, H, I),
    ratint(I, H, C).

% 'arith_eval++1'(A,B,C,D):-arith_eval(A+B+C,D)
'arith_eval++1'(A, B, C, D) :-
    rat_runtime(A, E, F),
    rat_runtime(B, G, H),
    addq(E, F, G, H, I, J),
    rat_runtime(C, K, L),
    addq(I, J, K, L, M, N),
    ratint(N, M, D).

% 'arith_eval+++1'(A,B,C,D,E):-arith_eval(A+B+C+D,E)
'arith_eval+++1'(A, B, C, D, E) :-
    rat_runtime(A, F, G),
    rat_runtime(B, H, I),
    addq(F, G, H, I, J, K),
    rat_runtime(C, L, M),
    addq(J, K, L, M, N, O),
    rat_runtime(D, P, Q),
    addq(N, O, P, Q, R, S),
    ratint(S, R, E).

% 'arith_eval-/1'(A,B,C):-arith_eval(-(A/B),C)
'arith_eval-/1'(A, B, C) :-
    rat_runtime(A, D, E),
    rat_runtime(B, F, G),
    divq(D, E, F, G, H, I),
    J is-(H),
    ratint(I, J, C).

% 'arith_eval/-11'(A,B):-arith_eval(-1/A,B)
'arith_eval/-11'(A, B) :-
    rat_runtime(A, C, D),
    divq(-1, 1, C, D, E, F),
    ratint(F, E, B).

% 'arith_eval-/2'(A,B,C):-arith_eval(-(A/B),C)
'arith_eval-/2'(A, B, C) :-
    rat_runtime(A, D, E),
    rat_runtime(B, F, G),
    divq(D, E, F, G, H, I),
    J is-(H),
    ratint(I, J, C).

% 'arith_eval-1'(A,B):-arith_eval(-(A),B)
'arith_eval-1'(A, B) :-
    rat_runtime(A, C, D),
    E is-(C),
    ratint(D, E, B).

% 'arith_eval*1'(A,B,C,D):-arith_eval(A*rat(B,C),D)
'arith_eval*1'(A, B, C, D) :-
    rat_runtime(A, E, F),
    mulq(E, F, B, C, G, H),
    ratint(H, G, D).

% 'arith_eval+*1'(A,B,C,D):-arith_eval(A*B+C,D)
'arith_eval+*1'(A, B, C, D) :-
    rat_runtime(A, E, F),
    rat_runtime(B, G, H),
    mulq(E, F, G, H, I, J),
    rat_runtime(C, K, L),
    addq(I, J, K, L, M, N),
    ratint(N, M, D).

% 'arith_eval+1'(A,B,C,D):-arith_eval(A+B*C,D)
'arith_eval+1'(A, B, C, D) :-
    rat_runtime(A, E, F),
    rat_runtime(B, G, H),
    rat_runtime(C, I, J),
    mulq(G, H, I, J, K, L),
    addq(E, F, K, L, M, N),
    ratint(N, M, D).
