:- module(canvas,[test/0,main/0]).

:- use_module(library(tcltk)). 
:- use_module(library(system)).

:- export(test_aux/1).

% Example to probe that the returning values to prolog is correct in the third argument of the predicate tcl_eval.

main:- test.

test:-
	tcl_new(Interp),
	test_aux(Interp),
        tcl_delete(Interp).


test_aux(Interp):-
	tcl_eval(Interp,[canvas, '.c -height 140 -width 140 -background white'],_),
	tcl_eval(Interp,[pack, '.c'],_),
%	display(0), nl,
	tcl_eval(Interp,['.c', 'create oval 7 7 133 133 -outline black -fill gray80 -width 2'],_ID),
%	display(a), nl,
	tcl_eval(Interp,['.c', 'create oval 39 49 53 63 -outline black -fill black'],_ID1),
%	display(b), nl,
	tcl_eval(Interp,['.c', 'create oval 102 63 88 49 -outline black -fill black'],ID2),
	tcl_eval(Interp,['.c', 'create polygon 70 67 74 81 69 77 67 81  -fill black'],_ID3 ),
	tcl_eval(Interp,['.c', 'create arc 21 21 119 119 -start 225 -extent 95 -style arc -outline black -width 3'],_ID4),
	display('Hit enter to continue'),
	nl,
	get_code(_),
	tcl_eval(Interp,['.c','move', ID2,10,10],_),
        pause(2).
