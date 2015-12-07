:- module(_, [bftr/3],[assertions]).

:- data bf_pred/3.

bftr(end_of_file, L, M) :- !,
	(
	    bf_pred(M, _, _) ->
	    L= [(:- include(library(bf/bf_rt))), end_of_file]
	;
	    L = end_of_file, 
	    warning('bf package: no breadth-first predicate defined')
	),
        retractall_fact(bf_pred(M,_,_)).
bftr(<-(H,B), TrCls, M) :- !,
        clausetr(H, B, M, TrCls).
bftr(<-(H), TrCls, M) :- !,
        clausetr(H, true, M, TrCls).

clausetr(Head, Body, M, TrCls) :-
        functor(Head, F, A),
        ( bf_pred(M, F, A) -> TrCls = TrCl
        ; asserta_fact(bf_pred(M, F, A)),
          functor(P, F, A),
          TrCls = ['$bfpred'(P),(P :- '$bf'([u([P],P)|L],L,P))| TrCl]
        ),
        body_to_dl(Body, Bodylist, Bodyrest),
        TrCl = ['$bfcl'(Head, Bodylist, Bodyrest)].
        
body_to_dl((A, B), [A|Bs], R):- !, body_to_dl(B, Bs, R).
body_to_dl(true, R, R) :- !.
body_to_dl(A, [A|R], R).
