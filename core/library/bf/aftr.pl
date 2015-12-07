:- module(_, [aftr/3],[]).

:- data bf_pred/3.

aftr(end_of_file, L, M) :- !,
	(
	    bf_pred(M, _, _) ->
	    L= [(:- include(library(bf/af_rt))), end_of_file]
	;
	    L = end_of_file, 
	    warning('bf/af package: no breadth-first predicate defined')
	),
        retractall_fact(bf_pred(M,_,_)).
aftr(<-(H,B), TrCls, M) :- !,
        clausetr(H, B, M, TrCls).
aftr(<-(H), TrCls, M) :- !,
        clausetr(H, true, M, TrCls).

clausetr(Head, Body, M, TrCls) :-
        functor(Head, F, A),
        ( bf_pred(M, F, A) -> TrCls = TrCl
        ; asserta_fact(bf_pred(M, F, A)),
          functor(P, F, A),
          TrCls = ['$bfpred'(P),(P :- '$bf'([u([P|T],T,P)|L],L,P))| TrCl]
        ),
        body_to_dl(Body, Bodylist, Bodyrest),
        TrCl = '$bfcl'(Head, Bodylist, Bodyrest).
        
body_to_dl((A, B), [A|Bs], R):- !, body_to_dl(B, Bs, R).
body_to_dl(true, R, R) :- !.
body_to_dl(A, [A|R], R).
