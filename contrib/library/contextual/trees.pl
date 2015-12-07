:- module( trees, [], [contextual]).

:- push_prolog_flag(unused_pred_warnings, no).
:- types.
:- pop_prolog_flag(unused_pred_warnings).

constructor( 'Tree'( E, e)) // -'EmptyTree'( E) :- true.
constructor( 'Tree'( E, t(L, P, R))) // -'TreeNode'( E) :-
	constructor( 'Tree'( E, L)) // -left,
	constructor( 'Tree'( E, R)) // -right,
	set( P) / pivot.

member( _) // +'EmptyTree'( _) :- fail.
member( X) // +'TreeNode'( _) :-
	(  member( X) // +left
	;  get( X) / pivot
	;  member( X) // +right
	).

ins( X) // ('EmptyTree'( E) -> 'TreeNode'( E)) :-
	constructor( 'Tree'( E, t(e, X, e))) // -'TreeNode'( E).
ins( X) // 'TreeNode'( _) :-
	get( Y) / pivot,
	(  X @< Y
	-> ins( X) // left
	;  ins( X) // right
	).

interpolate // (+'EmptyTree'( _), difflist) :- true.
interpolate // (+'TreeNode'( _), difflist):-
	interpolate // (+left, difflist),
	get( X) / pivot,
	[X] / difflist,
	interpolate // (+right, difflist).

