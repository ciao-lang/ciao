:- module( contextual_rt, []).

:- multifile '$_context_method'/5.

% ======================================================================

invoke_method( Call, In, Out):-
	nonvar( Call), callable( Call),
	Call=.. [F|A],
	(  In=  [Sig|_], !
	;  Out= [Sig|_], !
	;  Sig= []
	),
	'$_context_method'( F, Sig, A, In, Out).

:- export( invoke_method/3).
 
% ======================================================================

signature( X, []):- var( X), !.
signature( X, []):- atomic( X), !.
signature( X, Y):-
	X =.. [FX, A1 | As],
	signature( A1, A1C),
	Y =.. [FX, A1C | As].

:- export( signature/2).

% ======================================================================

supersignature( X, []):- var( X), !.
supersignature( X, []):- atomic( X), !.
supersignature( X, []):-
	X =.. [ _, A | _ ],
	( var( A); atom( A) )
	, !.
supersignature( X, Y):-
	X=.. [ XF, A | As ],
	supersignature( A, S),
	Y=.. [ XF, S | As ].

:- export( supersignature/2).

% ======================================================================

siglist( [], []):- !.
siglist( Sig, [Sig|R]):-
	supersignature( Sig, SS),
	siglist( SS, R).

:- export( siglist/2).

% ======================================================================


	


