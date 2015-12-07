:- module( trees_demo, _, [contextual]).

:- types( trees).

% ======================================================================
% Some examples of OO-programming with (unballanced) binary trees

% ----------------------------------------------------------------------
% Construct an empty tree.

goal1( Answer) / (*):-
	constructor( 'Tree'( term, e)) // -t,
	get( Answer) / t.

% ?- goal1( Answer).
% Answer = 'Tree'('EmptyTree'([]),term) ? 
% yes

% ----------------------------------------------------------------------
% Construct a tree node.

goal2( Answer) / (*):-
	constructor( 'Tree'( term, t(e, 2, e))) // -t,
	get( Answer) / t.

% ?- goal2( Answer).
% Answer = 'Tree'('TreeNode'([],n('Tree'('EmptyTree'([]),term),2,'Tree'('EmptyTree'([]),term))),term) ? 
% yes


% ----------------------------------------------------------------------
% Start from an empty tree, then insert some elements, and convert the
% result into list.

goal3( Answer) / (*):-
	constructor( 'Tree'( integer, e)) // -t,
	ins( 2) // t,
	ins( 1) // t,
	ins( 6) // t,
	interpolate // (+t, +Answer, -[]).

% ?- goal3( Answer).
% Answer = [1,2,6] ? 
% yes

% ----------------------------------------------------------------------
% Start from an empty tree, then insert some elements, and enumerate
% members.

goal4( Answer) / (*):-
	constructor( 'Tree'( integer, e)) // -t,
	ins( 2) // t,
	ins( 1) // t,
	ins( 6) // t,
	member( Answer) // +t.

% ?- goal4( Answer).
% Answer = 1 ? ;
% Answer = 2 ? ;
% Answer = 6 ? ;
% no

