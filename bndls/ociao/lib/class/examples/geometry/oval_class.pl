%%---------------------------------------------------------------------
%%
%% OVAL CLASS
%%
%%---------------------------------------------------------------------

:- class(oval_class).

:- inherit_class(library(class/examples/geometry/shape_class)).
:- implements(library(class/examples/geometry/mobile)).

%%---------------------------------------------------------------------
%% COORDINATES
%%---------------------------------------------------------------------

:- data        coord/4.
:- inheritable coord/4.

coord(0,0,0,0).

:- export([set_width/1,set_height/1,set_center/2]).

set_width(W) :-
	number(W),
	W >= 0,
	retract_fact(coord(X,Y,_,H)),
	asserta_fact(coord(X,Y,W,H)),
	notify_changes.

set_height(H) :-
	number(H),
	H >= 0,
	retract_fact(coord(X,Y,W,_)),
	asserta_fact(coord(X,Y,W,H)),
	notify_changes.

set_center(X,Y) :-
	number(X),
	number(Y),
	retract_fact(coord(_,_,W,H)),
	asserta_fact(coord(X,Y,W,H)),
	notify_changes.

:- export([get_width/1,get_height/1,get_center/2]).

get_width(W) :-
	coord(_,_,W,_).

get_height(H) :-
	coord(_,_,_,H).

get_center(X,Y) :-
	coord(X,Y,_,_).

move(IncX,IncY) :-
	number(IncX),
	number(IncY),
	retract_fact(coord(X,Y,W,H)),
	NX is IncX+X,
	NY is IncY+Y,
	asserta_fact(coord(NX,NY,W,H)),
	notify_changes.

get_spot(X,Y) :-
	get_center(X,Y).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

tcl_name(oval).

creation_options([" ",X1," ",Y1," ",X2," ",Y2," "|Other]) :-
	coord(X,Y,W,H),
	W2 is W / 2,
	H2 is H / 2,
	X1 is X-W2,
	X2 is X+W2,
	Y1 is Y-H2,
	Y2 is Y+H2,
	inherited creation_options(Other).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

oval_class.
oval_class(Owner) :-
	shape_class(Owner).

oval_class((X,Y),W,H,Owner) :-
	shape_class(Owner),
	set_width(W),
	set_height(H),
	set_center(X,Y).

oval_class((X,Y),W,H) :-
	set_width(W),
	set_height(H),
	set_center(X,Y).

:- set_prolog_flag(multi_arity_warnings,on).
