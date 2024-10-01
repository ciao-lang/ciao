:- use_package(hiord).

:- use_package(datafacts).

:- multifile '$more_sol'/1.

:- data '$more_sol'/1.

'$iterdeep'(Goal,Formul,MaxDepth) :-
	Goal =.. [_,_,_,ActualGoal|_],        
	retractall_fact('$more_sol'(ActualGoal)),
	!,
	'$ideep'(Goal,Formul,MaxDepth,-1).

'$ideep'(Goal,_,_,OldCut) :-
	Goal =.. [_,_,_,_,DepthSol|_],
	call(Goal),
	DepthSol > OldCut.



'$ideep'(Goal,Formul,MaxDepth,_) :-
	Goal =.. [Name,Depth,Cut,ActualGoal|R],
	'$more_sol'(ActualGoal),
	retractall_fact('$more_sol'(ActualGoal)),
	Formul(Cut,NCut),
	(MaxDepth = unlimited ; NCut =< MaxDepth),
	NGoal =.. [Name,Depth,NCut,ActualGoal|R],        
	!,
	'$ideep'(NGoal,Formul,MaxDepth,Cut).




'$max_depthsol'(L,X):-
	'$$max'(L,0,X).

'$$max'([],X,X):-
	!.
'$$max'([H|B],M,R):-
	(H > M -> NM = H
	; NM = M),
	!,  
	'$$max'(B,NM,R).
