%%---------------------------------------------------------------------
%%
%% RECTANGLE CLASS
%%
%%---------------------------------------------------------------------

:- class(rectangle_class).

:- inherit_class(library(class/examples/geometry/shape_class)).
:- implements(library(class/examples/geometry/mobile)).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([creation_options/1,tcl_name/1]).

:- data        coordinates/4.
:- inheritable coordinates/4.

tcl_name(rectangle).

creation_options([" ",X," ",Y," ",XX," ",YY," "|BasicOptions]) :-
	coordinates(X,Y,XX,YY),
	inherited creation_options(BasicOptions).

%%---------------------------------------------------------------------
%% COORDINATES
%%---------------------------------------------------------------------

:- export(set_coordinates/2).

set_coordinates((X,Y),(XX,YY)) :-
	number(X),
	number(Y),
	number(XX),
	number(YY),
	!,
	set_fact(coordinates(X,Y,XX,YY)),
	notify_changes.

:- export(get_coordinates/2).

get_coordinates((X,Y),(XX,YY)) :-
	coordinates(X,Y,XX,YY).

:- export(width/1).

width(W) :-
	coordinates(X,_,XX,_),
	W is abs(XX-X).

:- export(height/1).

height(H) :-
	coordinates(_,Y,_,YY),
	H is abs(YY-Y).

move(IncX,IncY) :-
	number(IncX),
	number(IncY),
	retract_fact(coordinates(X,Y,XX,YY)),
	NX is IncX+X,
	NY is IncY+Y,
	NXX is IncX+XX,
	NYY is IncY+YY,
	asserta_fact(coordinates(NX,NY,NXX,NYY)),
	notify_changes.

get_spot(SX,SY) :-
	coordinates(X,Y,XX,YY),
	SX is (X+XX)/2,
	SY is (Y+YY)/2.

%%--------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

rectangle_class :-
	shape_class.

rectangle_class(Canvas) :-
	shape_class(Canvas).

rectangle_class(Corner1,Corner2) :-
	shape_class,
	set_coordinates(Corner1,Corner2).

rectangle_class(Corner1,Corner2,Canvas) :-
	shape_class(Canvas),
	set_coordinates(Corner1,Corner2).

:- set_prolog_flag(multi_arity_warnings,on).
