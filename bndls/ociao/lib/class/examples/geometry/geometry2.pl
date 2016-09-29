:- module(geometry2,[main/0],[objects]).

:- use_class(library(class/examples/geometry/oval_class)).
:- use_class(library(class/examples/geometry/canvas_class)).
:- use_class(library(class/examples/geometry/poly_class)).

:- left_eye  instance_of oval_class((100,50),50,25).
:- right_eye instance_of oval_class((190,50),50,25).
:- lips      instance_of poly_class([(100,120),(120,150),(170,150),(190,130)]).
:- face      instance_of canvas_class(
	[
	oval_class(right_eye),
	oval_class(left_eye),
	poly_class(lips)
	]).

main :-
	left_eye:set_border_width(3),
	right_eye:set_border_width(3),
	left_eye:set_bg_color(lightblue),
	right_eye:set_bg_color(lightblue),
	left_eye:set_fg_color(maroon),
	right_eye:set_fg_color(maroon),

	lips:set_border_width(5),
	lips:set_fg_color(red),
	lips:set_bg_color(black),

	face:show,

	hit_enter,
	destroy oval_class(face),
	destroy oval_class(left_eye),
	destroy oval_class(right_eye).

hit_enter :-
	display('Hit ENTER to continue...'),
	nl,
	get_code(_).
