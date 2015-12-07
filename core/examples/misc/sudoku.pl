:- module(sudoku,_,[assertions]).

:- doc(author, "Edison Mera").

:- doc(module, "

Sudoku, also known as Number Place, is a logic-based placement
puzzle.

The aim of the puzzle is to enter a numerical digit from 1 through 9
in each cell of a 9×9 grid made up of 3×3 subgrids (called
\"regions\"), starting with various digits given in some cells (the
\"givens\").

Each row, column, and region must contain only one instance of each
numeral. Completing the puzzle requires patience and logical ability.

Although first published in a U.S. puzzle magazine in 1979, Sudoku
initially caught on in Japan in 1986 and attained international
popularity in 2005.

(From Wikipedia, the free encyclopedia)

").

%:- entry sudoku_square_solve(M) : term(M).

digit(1).
digit(2).
digit(3).
digit(4).
digit(5).
digit(6).
digit(7).
digit(8).
digit(9).

same_square(1,2).
same_square(1,3).
same_square(2,1).
same_square(2,3).
same_square(3,1).
same_square(3,2).
same_square(4,5).
same_square(4,6).
same_square(5,4).
same_square(5,6).
same_square(6,4).
same_square(6,5).
same_square(7,8).
same_square(7,9).
same_square(8,7).
same_square(8,9).
same_square(9,7).
same_square(9,8).

horizontal_colission(Row, X, Elm) :-
	digit(A),
	A \== X,
	arg(A, Row, Elm0),
	Elm == Elm0.

vertical_colission(Mtx, X, Y, Elm) :-
	digit(B),
	B \== Y,
	arg(B, Mtx, Row0),
	arg(X, Row0, Elm1),
	Elm == Elm1.

square_colission(Mtx, X, Y, Elm) :-
	same_square(X,C),
	same_square(Y,D),
	arg(D, Mtx, Row1),
	arg(C, Row1, Elm2),
	Elm == Elm2.

verify_sudoku_square_pos(Mtx, pos(X,Y)) :- 
%	digit(X),
%	digit(Y),
	arg(Y, Mtx, Row),
	arg(X, Row, Elm),
	digit(Elm),
	\+(horizontal_colission(Row, X, Elm)),
	\+(vertical_colission(Mtx, X, Y, Elm)),
	\+(square_colission(Mtx, X, Y, Elm)).

sudoku_square_solve(Mtx) :-
	sudoku_square_solve_posx([1,2,3,4,5,6,7,8,9], Mtx).

sudoku_square_solve_posx([],    _Mtx).
sudoku_square_solve_posx([Y|Ys], Mtx) :-
	sudoku_square_solve_posy([1,2,3,4,5,6,7,8,9], Mtx, Y),
	sudoku_square_solve_posx(Ys, Mtx).

sudoku_square_solve_posy([],    _Mtx, _Y).
sudoku_square_solve_posy([X|Xs], Mtx,  Y) :-
	verify_sudoku_square_pos(Mtx, pos(X,Y)),
	sudoku_square_solve_posy(Xs, Mtx, Y).

test1 :-
	Mt =
	c(
	  c( _, _, _,  _, _, 3,  _, 5, 4),
	  c( _, 4, 5,  8, 1, 7,  _, _, 3),
	  c( _, 2, _,  5, _, _,  1, 8, 7),
		  
	  c( _, 9, _,  _, _, 5,  4, 7, _),
	  c( _, 3, _,  _, _, 6,  _, _, _),
	  c( 7, 5, _,  4, 9, _,  _, _, 6),
	  
	  c( _, _, 9,  6, _, _,  _, 4, 1),
	  c( 3, _, 4,  7, _, _,  8, _, _),
	  c( 8, 6, 2,  _, _, 1,  7, _, 5)
        ),
	sudoku_square_solve(Mt),
	display(Mt),nl,fail ; true.
	   
test2 :-
	Mt = 
	c(
	  c( _, _, _,  _, _, _,  _, _, _),
	  c( _, _, _,  _, _, _,  _, _, _),
	  c( _, _, _,  _, _, _,  _, _, _),
		  
	  c( _, _, _,  _, _, _,  _, _, _),
	  c( _, _, _,  _, _, _,  _, _, _),
	  c( _, _, _,  _, _, _,  _, _, _),
	  
	  c( _, _, _,  _, _, _,  _, _, _),
	  c( _, _, _,  _, _, _,  _, _, _),
	  c( _, _, _,  _, _, _,  _, _, _)
        ),
	sudoku_square_solve(Mt),
	display(Mt).

test3 :-
	Mt =
	c(
	  c( _, _, _,  _, 9, 4,  8, 5, 3),
	  c( 5, _, _,  _, _, _,  _, _, 2),
	  c( 3, 6, 8,  7, _, _,  1, _, _),
		  
	  c( 8, _, _,  9, _, _,  _, 3, _),
	  c( _, _, _,  _, 7, _,  _, _, _),
	  c( _, 7, _,  _, _, 2,  _, _, 5),
	  
	  c( _, _, 5,  _, _, 7,  9, 6, 1),
	  c( 2, _, _,  _, _, _,  _, _, 7),
	  c( 7, 3, 9,  _, 1, _,  _, _, _)
        ),
	sudoku_square_solve(Mt),
	display(Mt).


%% This is a difficult one (MCL)

test4 :-
	Mt =
	c(
	  c( 1, _, _,  _, _, _,  _, _, _),
	  c( _, _, 2,  7, 4, _,  _, _, _),
	  c( _, _, _,  5, _, _,  _, _, 4),
		  
	  c( _, 3, _,  _, _, _,  _, _, _),
	  c( 7, 5, _,  _, _, _,  _, _, _),
	  c( _, _, _,  _, _, 9,  6, _, _),
	  
	  c( _, 4, _,  _, _, 6,  _, _, _),
	  c( _, _, _,  _, _, _,  _, 7, 1),
	  c( _, _, _,  _, _, 1,  _, 3, _)
        ),
	sudoku_square_solve(Mt),
	display(Mt).

 
% ?- test3,fail;true.
% c(c(1,2,7,6,9,4,8,5,3),c(5,9,4,1,8,3,6,7,2),c(3,6,8,7,2,5,1,9,4),c(8,5,2,9,4,1,7,3,6),c(6,4,3,5,7,8,2,1,9),c(9,7,1,3,6,2,4,8,5),c(4,8,5,2,3,7,9,6,1),c(2,1,6,8,5,9,3,4,7),c(7,3,9,4,1,6,5,2,8))
% yes
