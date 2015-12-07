:- module(expand_q, [expand/2], []).
% Expansion for clp(Q).

expand(Goal, Code) :-
        ( Goal = arith_eval(_)
        ; Goal = arith_eval(_,_)
        ; Goal = arith_zero(_)
        ),
        pattern(Pattern, Code),
        pm(Pattern, Goal),
        Pattern = Goal.

pm(Pattern, Data) :-
        var(Pattern), !,
        leaf(Data).
pm(Pattern, Data) :-
        nonvar(Data),
        functor(Pattern, N, A),
        functor(Data,    N, A),
        pm_args(A, Pattern, Data).

pm_args(0, _, _) :- !.
pm_args(N, P, D) :-
        arg(N, P, Pa),
        arg(N, D, Da),
        pm(Pa, Da),
        N1 is N-1,
        pm_args(N1, P, D).

leaf(V) :- var(V).
leaf(N) :- number(N).
leaf(rat(_,_)).

pattern( arith_zero(A), (A=0)).
pattern( arith_zero(A-B), 'arith_zero-1'(A,B)).
pattern( arith_zero(A*B-1), 'arith_zero-*1'(A, B)).
pattern( arith_zero(A-1), 'arith_zero-1'(A)).
pattern( arith_eval(A<0), 'arith_eval<1'(A)).
pattern( arith_eval(A>0), 'arith_eval>1'(A)).
pattern( arith_eval(A<B), 'arith_eval<1'(A,B)).
pattern( arith_eval(A>B), 'arith_eval<1'(B,A)).
pattern( arith_eval(A=<0), 'arith_eval=<1'(A)).
pattern( arith_eval(A>=0), 'arith_eval>=1'(A)).
pattern( arith_eval(B-C, A), 'arith_eval-1'(B, C, A)).
pattern( arith_eval(B*C, A), 'arith_eval*1'(B, C, A)).
pattern( arith_eval(C*D-B, A), 'arith_eval-*1'(C, D, B, A)).
pattern( arith_eval(B*C+D*E, A), 'arith_eval+*1'(B, C, D, E, A)).
pattern( arith_eval(B*C-D*E, A), 'arith_eval-*1'(B, C, D, E, A)).
pattern( arith_eval(B+C, A), 'arith_eval+1'(B, C, A)).
pattern( arith_eval(C+D+B, A), 'arith_eval++1'(C, D, B, A)).
pattern( arith_eval(D+E+C+B, A), 'arith_eval+++1'(D, E, C, B, A)).
pattern( arith_eval(-(B/C), A), 'arith_eval-/1'(B, C, A)).
pattern( arith_eval(-1/B, A), 'arith_eval/-11'(B, A)).
pattern( arith_eval(-(B/C), A), 'arith_eval-/2'(B, C, A)).
pattern( arith_eval(-(B), A), 'arith_eval-1'(B, A)).
pattern( arith_eval(B*rat(C,D), A), 'arith_eval*1'(B, C, D, A)).
pattern( arith_eval(C*D+B, A), 'arith_eval+*1'(C, D, B, A)).
pattern( arith_eval(B+C*D, A), 'arith_eval+1'(B, C, D, A)).
