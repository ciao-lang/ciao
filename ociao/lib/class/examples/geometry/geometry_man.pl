
:- module(geometry_man,[main/0],[objects]).

:- use_module(library(concurrency)).

:- use_class(library(class/examples/geometry/oval_class)).
:- use_class(library(class/examples/geometry/canvas_class)).
:- use_class(library(class/examples/geometry/poly_class)).

main :- Delay = 10,
	
	Left_eye new oval_class((100,50),50,25),
	Right_eye new oval_class((190,50),50,25),

	Left_eye:set_border_width(3),
	Right_eye:set_border_width(3),
	Left_eye:set_bg_color(blue),
	Right_eye:set_bg_color(blue),

	Lips new poly_class([(100,150),(120,170),(170,170),(190,150)]),
	
	Face1 new canvas_class([Left_eye,Right_eye,Lips]),
	Face1:show,
	eng_call(move(100,Left_eye, right,2,Delay), create, create),
	eng_call(move(100,Right_eye,down,2,Delay), create, create),
	hit_enter,
	destroy Face1,
	destroy Left_eye,
	destroy Right_eye.

hit_enter :-
	display('Hit ENTER to continue...'),
	nl,
	get_code(_).

move(0,_O,_Direction,_Increment,_Delay).
move(Total,O,Direction,Increment,Delay) :-
	Total > 0,
	MyDelay is Delay * 10000,
	my_pause(MyDelay),
	O:get_center(X,Y),
	compute_move(Direction,Increment,X,Y,NX,NY),
	O:set_center(NX,NY),
	NTotal = Total-Increment,
	move(NTotal,O,Direction,Increment,Delay).
	
compute_move(right,Amount,X,Y,NX,Y) :- NX is X+Amount.
compute_move(left, Amount,X,Y,NX,Y) :- NX is X-Amount.
compute_move(up,   Amount,X,Y,X,NY) :- NY is Y-Amount.
compute_move(down, Amount,X,Y,X,NY) :- NY is Y+Amount.

my_pause(0).
my_pause(Delay) :- 
	Delay > 0,
	NDelay is Delay - 1,
	my_pause(NDelay).
