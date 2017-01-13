:- module(color_space, [], [assertions]).

:- doc(title, "Color Space Transformations").
:- doc(author, "Jose F. Morales").
% :- doc(stability, experimental).

:- doc(module, "This module defines color space transformations.").

:- doc(bug, "This library is far from complete.").
:- doc(bug, "Regular types are not enough to describe the color space types accurately.").

% TODO: (This is not a regular type)
:- export(rgb/1).
:- prop rgb(C) # "@var{C} is a color in RGB space (Red-Green-Blue)".
rgb(rgb(R,G,B)) :-
	0 =< R, R =< 1.0,
	0 =< G, G =< 1.0,
	0 =< B, B =< 1.0.

% TODO: (This is not a regular type)
:- export(hsv/1).
:- prop hsv(C) # "@var{C} is a color in HSV space (Hue-Saturation-Value)".
hsv(hsv(H,S,V)) :-
	0 =< H, H < 360,
	0 =< S, S =< 1.0,
	0 =< V, V =< 1.0.

% TODO: (This is not a regular type)
:- export(hsv/1).
:- prop hsl(C) # "@var{C} is a color in HSL space (Hue-Saturation-Lightness)".
hsl(hsl(H,S,L)) :-
	0 =< H, H < 360,
	0 =< S, S =< 1.0,
	0 =< L, L =< 1.0.

% TODO: (This is not a regular type)
:- export(color/1).
:- prop color(C) # "@var{C} is a color in any color space".
color(C) :- rgb(C).
color(C) :- hsv(C).
color(C) :- hsl(C).

:- export(hex/1).
:- prop hex(Hex) # "@var{Hex} is a color in @tt{#RRGGBB} hexadecimal 
   representation".
hex(Hex) :- atm(Hex).

:- export(hsv_to_rgb/2).
:- pred hsv_to_rgb(HSV, RGB) :: hsv * rgb # "Transform the color
   @var{HSV} in @pred{hsv/1} space to @var{RGB} in @pred{rgb/1} space".

hsv_to_rgb(HSV, RGB) :-
	HSV = hsv(H,S,V),
	RGB = rgb(R,G,B),
	( S = 0 ->
	    % handle greyscale case
	    RGB = rgb(R,G,B)
	; %
          H1 is H / 60, % convert to sector between 0 and 5
	  I is integer(H1),
	  F is H1 - I,
	  P is V * (1 - S),
	  Q is V * (1 - S * F),
	  T is V * (1 - S * (1 - F)),
	  ( I = 0 -> RGB = rgb(V,T,P)
	  ; I = 1 -> RGB = rgb(Q,V,P)
	  ; I = 2 -> RGB = rgb(P,V,T)
	  ; I = 3 -> RGB = rgb(P,Q,V)
	  ; I = 4 -> RGB = rgb(T,P,V)
	  ; RGB = rgb(V,P,Q)
	  )
	).

:- export(hsl_to_hsv/2).
:- pred hsl_to_rgb(HSL, HSV) :: hsl * hsv # "Transform the color
   @var{HSL} in @pred{hsl/1} space to @var{HSV} in @pred{hsv/1} space".

hsl_to_hsv(HSL, HSV) :-
	HSL = hsl(H,S,L),
	%
	( L =< 0.5 ->
	    S1 is S * L
	; S1 is S * (1 - L)
	),
	V is L + S1,
	( V = 0 ->
	    S2 = 0
	; S2 is 2 * S1 / (L + S1)
	),
	HSV = hsv(H,S2,V).

:- export(hsl_to_rgb/2).
:- pred hsl_to_rgb(HSL, RGB) :: hsl * rgb # "Transform the color
   @var{HSL} in @pred{hsl/1} space to @var{RGB} in @pred{rgb/1} space".

hsl_to_rgb(HSL, RGB) :-
	% TODO: Use a direct method
	hsl_to_hsv(HSL, HSV),
	hsv_to_rgb(HSV, RGB).

:- export(rgb_to_hex/2).
:- pred rgb_to_hex(RGB, Hex) :: rgb * hex # "Transform the color
   @var{RGB} in @pred{rgb/1} space to @var{Hex} in @pred{hex/1}
   representation".

rgb_to_hex(rgb(R,G,B), Hex) :-
	N is (integer(R * 255) << 16) \/
             (integer(G * 255) << 8) \/
             (integer(B * 255)) \/
             0x1000000, % trick to have 6 digits, 1 will be removed later
	number_codes(N, 16, [_|N1]),
	atom_codes(Hex, [0'#|N1]).

:- export(color_to_hex/2).
:- pred color_to_hex(Color, Hex) :: color * hex # "Obtain the atom 
   @var{Hex} representation (@pred{hex/1}) of color @var{Color}".

color_to_hex(Color, Hex) :- Color = hsv(_,_,_), !,
	hsv_to_rgb(Color, Color2),
	rgb_to_hex(Color2, Hex).
color_to_hex(Color, Hex) :- Color = hsl(_,_,_), !,
	hsl_to_rgb(Color, Color2),
	rgb_to_hex(Color2, Hex).
color_to_hex(Color, Hex) :- Color = rgb(_,_,_), !,
	rgb_to_hex(Color, Hex).

