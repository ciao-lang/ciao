%----------------------------------------------------------------------------
%
% O'CIAO EXAMPLE
% 
%----------------------------------------------------------------------------

:- module(dancing_shapes,[main/0],[objects]).

:- use_class(disco_class).
:- use_class(library(class/examples/geometry/oval_class)).
:- use_class(library(class/examples/geometry/circle_class)).
:- use_class(library(class/examples/geometry/poly_class)).
:- use_class(library(class/examples/geometry/box_class)).
:- use_class(library(class/examples/geometry/rectangle_class)).

:- use_module(library(system)).

%----------------------------------------------------------------------------

main :-
	%
	A new rectangle_class((80,20),(110,60)),
	A:set_bg_color(purple),
	%
	B new circle_class((60,170),25),
	B:set_border_width(8),
	B:set_fg_color(red),
	%
 %% 	C new box_class((30,90),15),
 %% 	C:set_bg_color(yellow),
 %% 	%
 %% 	D new oval_class((60,30),20,40),
 %% 	D:set_bg_color(blue),
	%
	E new poly_class([(90,120),(80,150),(120,160)]),
	E:set_fg_color(maroon),
	E:set_bg_color(green),
	%
	Disco new disco_class([A,B,E]),
	Disco:show,
        pause(5),
	Disco:dammed_dance,
 	%
 	hit_enter,
 	destroy Disco.	

hit_enter :-
	display('Hit ENTER to continue...'),
	nl,
	get_code(_).
