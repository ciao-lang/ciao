:- include(library(clpqr/clpqr_ops)).

:- set_prolog_flag(multi_arity_warnings, off).

clpqr_meta(A) :- var(A), !, fail.
clpqr_meta((A,B)) :- !, clpqr_meta(A), clpqr_meta(B).
clpqr_meta((A;B)) :- !, ( clpqr_meta(A) ; clpqr_meta(B) ).
clpqr_meta([]) :- !.
clpqr_meta(X) :- X = [_|_], !, clpqr_meta_list(X).
clpqr_meta(A) :-
        translate_meta_clp(A).

% (just for backwards compatibility)
clpqr_meta_list([]) :- !.
clpqr_meta_list([A|As]) :- !,
        clpqr_meta(A),
        clpqr_meta_list(As).

translate_meta_clp(A.=.B) :- !,
        translate_meta_clp_aux(.=.,  A-B).
translate_meta_clp(A.<>.B) :- !,
        translate_meta_clp_aux(.<>., A-B).
translate_meta_clp(A.<.B) :- !,
        translate_meta_clp_aux(.<.,  A-B).
translate_meta_clp(A.=<.B) :- !,
        translate_meta_clp_aux(.=<., A-B).
translate_meta_clp(A.>.B) :- !,
        translate_meta_clp_aux(.<.,  B-A).
translate_meta_clp(A.>=.B) :- !,
        translate_meta_clp_aux(.=<., B-A).

translate_meta_clp_aux(Type, Diff) :-
        normalize(Diff, K, I, H),
        translate_meta_clp_aux_aux(Type, K, I, H).

translate_meta_clp_aux_aux(.<>., K, I, H) :- var_with_def(_, nz, K, I, H).
translate_meta_clp_aux_aux(.=. , _, I, H) :- solve_lin(H, I).
translate_meta_clp_aux_aux( .<., _, I, H) :- solve_ineq_lt(H, I).
translate_meta_clp_aux_aux(.=<., _, I, H) :- solve_ineq_le(H, I).


/*
clpqr_entailed(+B)    : succeeds if the store implies B

 We want to check if the store A entails the list (conjunction) of constraints
 B=[b_1,...,b_m]

   - cheaking "(A ==> B) <==> true" can be done by checking that
     "~(A ==> B) <==> false"

   - ~(A ==> B) = (A /\ ~B) = (A /\ ~b_1) \/  ... \/ (A /\ ~b_m)

   - in order to conclude that "~(A ==> B)" is false, it's enough to check that
     A/\~b_i is false for i=1..m
*/


clpqr_entailed(B) :-
        all_neg_fail(B).

all_neg_fail([]).
all_neg_fail([C|_Cs]) :-
        neg_c(C,Neg_C),
        clpqr_meta([Neg_C]),
        !,
        fail.
all_neg_fail([_C|Cs]) :-
        all_neg_fail(Cs).

neg_c(A .>.  B,  A .=<. B).
neg_c(A .<.  B,  A .>=. B).
neg_c(A .=.  B,  A .<>. B).
neg_c(A .<>. B,  A .=.  B).
neg_c(A .>=. B,  A .<.  B).
neg_c(A .=<. B,  A .>.  B).

