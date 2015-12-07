:- use_package(fd).

main(X,Y) :-
	X .>. 0,
	Y .>. 0,
	X + Y .=. 9,
	2*X + 4*Y .=. 24.
%	labeling([X,Y]).

main2(X,Y) :-
	[X,Y] in -9..9,
	X .=. 9 + Y,
	2*X .=. 24 + 4*Y.
	
main3(X,Y) :-
	[X,Y] in 0..9,
%	X -(4-X) .=. 2.
	X - Y .=. 2,
 	X + Y .=. 4,
%	display('End of Propagation'), nl,
	labeling([X,Y]).
 
main4(X,Y) :-
	[X,Y] in 0..9,
	X - Y .=. 1,
	X + Y .=. 3.
main5(X,Y) :-
	[X,Y] in 0..9,
	X - Y .=. 2,
	X .>. 8.
