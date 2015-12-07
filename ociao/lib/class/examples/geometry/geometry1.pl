:- module(geometry1,[main/0],[objects]).

:- use_class(library(class/examples/geometry/oval_class)).
:- use_class(library(class/examples/geometry/canvas_class)).
:- use_class(library(class/examples/geometry/poly_class)).

%:- disable_optimization.

main :-
	Left_eye new oval_class((100,50),50,25),
	Right_eye new oval_class((190,50),50,25),

	Left_eye:set_border_width(3),
	Right_eye:set_border_width(3),
	Left_eye:set_bg_color(lightblue),
	Right_eye:set_bg_color(lightblue),
	Left_eye:set_fg_color(maroon),
	Right_eye:set_fg_color(maroon),
	
	Lips new poly_class([(100,130),(120,150),(170,150),(190,120)]),
	Lips:set_border_width(5),
	Lips:set_fg_color(red),
	Lips:set_bg_color(black),
	
	Face1 new canvas_class([Left_eye,Right_eye,Lips]),
	Face1:show,
	hit_enter,
	Lips:move(2,2),
	Left_eye:move(-40,2),
	Right_eye:move(40,2),
	Face1:show,
	hit_enter,
	destroy Face1,
	destroy Left_eye,
	destroy Right_eye.

hit_enter :-
	display('Hit ENTER to continue...'), nl,
	get_code(_).
