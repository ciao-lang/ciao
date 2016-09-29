%%---------------------------------------------------------------------
%%
%% DANCING SHAPES CANVAS
%%
%%---------------------------------------------------------------------

:- class(disco_class,[],[objects]).

:- inherit_class(library(class/examples/geometry/canvas_class)).
:- use_class(library(class/examples/geometry/shape_class)).
:- use_class(library(class/examples/geometry/mobile)).

:- use_module(library(random)).
:- use_module(library(system)).
:- use_module(library(aggregates)).
:- use_module(library(concurrency)).

%%---------------------------------------------------------------------
%% OVERRIDEN ADD_ITEM
%%---------------------------------------------------------------------

:- concurrent dancing/3.
:- inheritable dancing/3.

:- export(add_item/1).

add_item(Shape) :-
	inherited add_item(Shape),
	wanna_dance(Shape).

wanna_dance(Shape) :-
	Shape instance_of shape_class,
	Shape interface mobile,
	( dancing(Shape,_,_) -> true ; 
            initial_movement(X,Y),
	    asserta_fact(dancing(Shape,X,Y))
	),
	true.

initial_movement(X,Y) :-
	random(3,7,Xi),
	random(3,7,Yi),
        X is Xi * 2,
        Y is Yi * 2.

next_movements(X,Y,Ix,Iy,Nx,Ny) :-
	(( X<0 ; X>290 ) -> Change = true, Nx is -Ix ; Nx = Ix),
	(( Y<0 ; Y>190 ) -> Change = true, Ny is -Iy ; Ny = Iy),
	\+ Change = false,
	!.
next_movements(X,Y,Ix,Iy,NIx,NIy) :-
	current_fact(dancing(Shape,_,_)),
%	  \+ Shape = Self,
	  Shape:get_spot(Sx,Sy),
	  Nx is X+Ix, Dx is Nx-Sx,
	  Ny is Y+Iy, Dy is Ny-Sy,
	  abs(Dx) < 20, abs(Dy) < 20, !,
	NIx is sign(Dx+0.5)*abs(Ix),
	NIy is sign(Dy+0.5)*abs(Iy).
next_movements(_,_,Ix,Iy,Ix,Iy).


%%---------------------------------------------------------------------
%% DANCING CAPABILITIES
%%---------------------------------------------------------------------

:- export(lets_dance/1).

lets_dance(Shape) :-
	retract_fact(dancing(Shape,IncX,IncY)),
        my_pause(5000),
	Shape:move(IncX,IncY),
	Shape:get_spot(X,Y),
	next_movements(X,Y,IncX,IncY,NX,NY),
	assertz_fact(dancing(Shape,NX,NY)),
        !,
        lets_dance(Shape).

:- concurrent dancing/1.
:- export(dammed_dance/0).


dammed_dance:-
        findall(Shape, dancing(Shape, _, _), Shapes),
        all_dancing(Shapes).

all_dancing([]).
all_dancing([S|Ss]):-
        eng_call(lets_dance(S), create, create),
        all_dancing(Ss).


%%---------------------------------------------------------------------

my_pause(0):- !.
my_pause(Delay) :- 
	Delay > 0,
 	NDelay is Delay - 1,
 	my_pause(NDelay).
	


%%---------------------------------------------------------------------
%% CONSTRUCTORS
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

disco_class :-
	canvas_class.

disco_class(L) :-
	canvas_class(L),
	fail.

disco_class([]).
disco_class([Item|Next]) :-
	( wanna_dance(Item) ; true ),
	!,
	disco_class(Next).

%%---------------------------------------------------------------------

destructor :-
 %% 	dancing(Goal),
 %% 	eng_kill(Goal).
 eng_killothers.
