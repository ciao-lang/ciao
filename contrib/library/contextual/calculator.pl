:- module( calculator, _, [contextual]).
:- use_module(library(lists), [append/3]).

% ======================================================================
% This module illustrates concepts of contextual notation, as
% implemented by the package `contextual', and especially its use to
% implement object-oriented design.

% ======================================================================
% An example of query, that creates an empty stack machine, compiles
% and runs a simple program (a list of stack machine operations),
% returning the resulting stack machine.

% ?- make__StackMachine( [], SM), parse_program( [1, 2, add, dup,
% sto(x), dup, mul], P), run_program( P, SM, SM_Result).
% P = ['Operation'('Const'([],1)),'Operation'('Const'([],2)),'Operation'('Addition'([])),'Operation'('Dup'([])),'Operation'('Store'([],x)),'Operation'('Dup'([])),'Operation'('Mul'([]))],
% SM = m([],[]),
% SM_Result = m([9],[x=3]) ? 
% yes

% ======================================================================
% First, let us define notions of (1) a stack, (2) a variable binding,
% (3) an environment (a list of variable bindings), and (4) a stack
% machine, which is a stack with an associated environment.  We give
% context macros capitalized names and quote them to distinguish them
% from atoms denoting contextual variables.

:- def_context         'Stack' = stack::(default( []), list( number)).
:- def_context       'Binding' = {name:: atom = value::number}.
:- def_context           'Env' = env::(default( []), list( 'Binding')).
:- def_context  'StackMachine' = {m( 'Stack', 'Env')}.

% ----------------------------------------------------------------------
% In the above context definitions, we have used typing of form
% cvar::default(Y), where cvar is an atom denoting a contextual
% variable, and Y is its default value.  Let us now define default( X,
% Y) to denote that X has term Y for its default value.

default( X, Y):- ( nonvar( X); var( X), X=Y ).

% ----------------------------------------------------------------------
% Let us also define some useful predicates to work with lists as
% stacks.

push( X, A, [X|A]).
pop( X, [X|A], A).

% ----------------------------------------------------------------------
% Now, we define an abstract root class Operation for all operations
% on a stack machine.

:- def_context abstract: 'Operation'::(*).

% ----------------------------------------------------------------------
% Next, we define and apply an interface that declares two methods
% that need to be implemented for all subclasses of Operation.

:- def_interface op_interface( Class) = 
	[ 
	    parse/1 // -Class, 
	    virtual: do/0 // (+Class, 'StackMachine')
	].

:- apply_interface op_interface( 'Operation').

% ----------------------------------------------------------------------
% Some more additional context macros.

:- def_context        'Program' = program::list( 'Operation').
:- def_context   'EmptyProgram' = [].

% ----------------------------------------------------------------------
% Predicate parse_program/2 takes a list (1st arg) of terms and
% converts it into a list of Operations (2nd arg).

parse_program( []) / (* -> 'EmptyProgram') :- true.
parse_program( [Word|Rest]) / (* -> 'Program') :-
	parse_program( Rest) / -'Program',
	parse( Word) // -Op,
	push( Op) / 'Program'.

% ----------------------------------------------------------------------
% Predicate run_program/3 takes a program and an initial stack
% machine, and returns the stack machine that is the result of the
% execution of the program on the initial stack machine.

run_program / (+[], m::'StackMachine') :- true.
run_program / (+p::'Program', m::'StackMachine') :-
	pop( Operation) / p,
	do // (+Operation, m),
	run_program / (+p, m).

% ======================================================================
% Everything that is left is to define subclasses of Operation and
% implement the two methods defined by the op_inteface( 'Operation').

% ----------------------------------------------------------------------

:- def_context 'Addition'::'Operation'.

do // (+'Addition', 'StackMachine') :-
	pop( X) / stack,
	pop( Y) / stack,
	Z is X+Y,
	push( Z) / stack.

parse( add) // -'Addition' :- true.

% ----------------------------------------------------------------------

:- def_context 'Subtraction'::'Operation'.

do // (+'Subtraction', 'StackMachine') :-
	pop( X) / stack,
	pop( Y) / stack,
	Z is Y-X,
	push( Z) / stack.

parse( sub) // -'Subtraction' :- true.

% ----------------------------------------------------------------------

:- def_context 'Const'::'Operation' + const::number.

do // (+'Const', 'StackMachine') :-
	get( X) / const,
	push( X) / stack.

parse( N) // -'Const' :-
	number( N),
	set( N)/ const.

% ----------------------------------------------------------------------

:- def_context 'Store'::'Operation' + var::atom.

do // (+'Store', 'StackMachine') :-
	get( Var) / var,
	pop( Val) / stack,
	get( Env0) / env,
	(  append( A, [Var=_|B], Env0)
	-> append( A, [Var=Val|B], Env1)
	;  Env1= [Var=Val|Env0]
	),
	set( Env1) / env.

parse( sto( Var)) // -'Store' :-
	atom( Var),
	set( Var) / var.

% ----------------------------------------------------------------------

:- def_context 'Recall'::'Operation' + var::atom.

do // (+'Recall', 'StackMachine') :-
	get( Var) / var,
	get( Env) / env,
	member( Var=Val, Env), 
	push( Val) / stack.

parse( rcl( Var)) // -'Recall' :-
	atom( Var),
	set( Var) / var.

% ----------------------------------------------------------------------

:- def_context 'Dup'::'Operation'.

do // (+'Dup', 'StackMachine') :-
	pop( X) / stack,
	push( X) / stack,
	push( X) / stack.

parse( dup) // -'Dup' :- true.

% ----------------------------------------------------------------------

:- def_context 'Mul'::'Operation'.

do // (+'Mul', 'StackMachine') :-
	pop( X) / stack,
	pop( Y) / stack,
	Z is X*Y,
	push( Z) / stack.

parse( mul) // -'Mul' :- true.

% ----------------------------------------------------------------------

:- def_context 'Swap'::'Operation'.

do // (+'Swap', 'StackMachine') :-
	pop( X) / stack,
	pop( Y) / stack,
	push( X) / stack,
	push( Y) / stack.

parse( swap) // -'Swap' :- !, true.

% ----------------------------------------------------------------------

:- def_context 'Drop'::'Operation'.

do // (+'Drop', 'StackMachine') :-
	pop( _) / stack.

parse( drop) // -'Drop' :- true.

% ----------------------------------------------------------------------

:- def_context 'Plus1'::'Operation' + op::'Operation'.

do // (+'Plus1', 'StackMachine') :-
	do // (+op, 'StackMachine'),
	pop( X) / stack,
	X1 is X+1,
	push( X1) / stack.

parse( plus1( Op)) // -'Plus1' :-
	parse( Op) // -op.

% ----------------------------------------------------------------------

:- def_context 'Double'::'Operation'.

parse( double) // -'Double' :- true.

do // (+'Double','StackMachine'):-
	pop( X) / stack,
	Y is 2*X,
	push( Y) / stack.

% ======================================================================
