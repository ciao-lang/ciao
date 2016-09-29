%%---------------------------------------------------------------------
%%
%% POLYGON CLASS
%%
%%---------------------------------------------------------------------

:- class(poly_class).

:- inherit_class(library(class/examples/geometry/shape_class)).
:- implements(library(class/examples/geometry/mobile)).

:- use_module(library(lists), [append/3]).

%%---------------------------------------------------------------------
%% POINT LIST
%%---------------------------------------------------------------------

:- data        point_list/1.
:- inheritable point_list/1.
:- data        spot/2.

validate_points([]).

validate_points([(X,Y)|N]) :-
	number(X),
	number(Y),
	!,
	validate_points(N).

:- export(set_vertices/1).

set_vertices(L) :-
	validate_points(L),
	compute_spot(L,0,0,0,(SX,SY)),
	set_fact(spot(SX,SY)),
	set_fact(point_list(L)),
	notify_changes.

:- export(get_vertices/1).

get_vertices(L) :-
	point_list(L).

move(IncX,IncY) :-
	number(IncX),
	number(IncY),
	point_list(OldV),
	move_points(OldV,(IncX,IncY),V),
	set_vertices(V).

move_points([],_,[]).
move_points([(X,Y)|Np],(IncX,IncY),[(NX,NY)|Ni]) :-
	NX is X+IncX,
	NY is Y+IncY,
	move_points(Np,(IncX,IncY),Ni).

compute_spot([],X,Y,N,(SpotX,SpotY)) :-
	SpotX is X/N,
	SpotY is Y/N.

compute_spot([(X,Y)|Np],AccX,AccY,N,Spot) :-
	NX is X+AccX,
	NY is Y+AccY,
	M is N+1,
	compute_spot(Np,NX,NY,M,Spot).

get_spot(X,Y) :-
	spot(X,Y).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

points2options([],[" "]).

points2options([(X,Y)|Np],[" ",X," ",Y|No]) :-
	points2options(Np,No).

:- export([tcl_name/1,creation_options/1]).

creation_options(Options) :-
	point_list(Points),
	points2options(Points,Opts),
	inherited creation_options(Other),
	append(Opts,Other,Options).

tcl_name(polygon).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

poly_class.

poly_class(PointList) :-
	validate_points(PointList),
	!,
	set_vertices(PointList).

poly_class(Owner) :-
	shape_class(Owner).

poly_class(PointList,Owner) :-
	shape_class(Owner),
	set_vertices(PointList).

:- set_prolog_flag(multi_arity_warnings,on).
