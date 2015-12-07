:- module(_,_,[assertions]).

:- data debugpred/3.

:- data debugpredstatus/2.

% debugpred_term_tr(OrigTerm, NewTerm, M) :-
% 	display('Checking term:'),display(OrigTerm),nl,
% 	fail.

debugpred_goal_tr(OrigGoal, NewGoal, M) :-
	!,
	expand_goal(OrigGoal, NewGoal, M),
	OrigGoal\==NewGoal.

expand_goal(OrigGoal, NewGoal, M) :-
%	display('Now checking goal:'), display(OrigGoal),nl,
	functor(OrigGoal, Name, Arity),
	(   debugpred(Name, Arity, M) ->
	    (
		debugpredstatus(off,M) ->
%		display('Goal cutted: '), display(OrigGoal),nl,
		NewGoal = true
	    ;
		NewGoal = OrigGoal
	    )
	;
	    NewGoal = OrigGoal
	).

debugpred_sentence_tr(0, _, M) :-
	retractall_fact(debugpred(_,_,M)),
	retractall_fact(debugpredstatus(_,M)).
debugpred_sentence_tr((:- debugpred((P1,P2))), [], M) :-
	debugpred_sentence_tr((:- debugpred(P1)), [], M),
	debugpred_sentence_tr((:- debugpred(P2)), [], M).
debugpred_sentence_tr((:- debugpred(Name/Arity)), [], M) :-
	assertz_fact(debugpred(Name,Arity,M)).
debugpred_sentence_tr((:- debugpredstatus(Status)), [], M) :-
	retractall_fact(debugpredstatus(_,M)),
	assertz_fact(debugpredstatus(Status,M)).
debugpred_sentence_tr((:- debugsentence(S)), S, M) :-
	debugpredstatus(on,M),!.
debugpred_sentence_tr((:- debugsentence(_S)), [], M) :-
	debugpredstatus(off,M),!.
debugpred_sentence_tr((:- Body), (:- Body), _M) :- !.
debugpred_sentence_tr(Clause, [], M) :-
	( Clause = (Head :- _Body) -> true ; Clause = Head ),
%	display(_Body), display('***'),nl,
	functor(Head,Name,Arity),
	debugpred(Name,Arity,M),
	debugpredstatus(off,M),
%	display('Pred cutted: '), display(Name), display('/'), display(Arity), nl,
	!.
debugpred_sentence_tr((Head :- Body), (Head :- NewBody), M) :-
	slice_body(Body, NewBody, M).

slice_body((A,B),C,M) :-
	slice_body(A,A0,M),
	slice_body(B,B0,M),
	(   A0==true ->
	    C = B0
	;
	    (   B0==true ->
		C = A0
	    ;
		C = (A0, B0)
	    )
	).

slice_body(A,B,M) :-
	expand_goal(A,B,M) -> true
 ;
	A = B.
