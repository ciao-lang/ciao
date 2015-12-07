 %% knights.pl -- improved solution to the knight's tour
 %% AFSID           : $__Header$
 %% Author          : Manuel Carro Li~nares
 %% Created On      : Wed Jul 20 17:50:28 1994
 %% Last Modified By: 
 %% Last Modified On: Thu Feb 24 19:53:34 2005
 %% Update Count    : 296
 %% Status          : Unknown, Use with caution!

:- module(knights, [main/1], []).

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(prolog_sys)).

 %% knights_print(5) performs 12385816 resolution steps (and fails...)

 %% NxN board, a knight has to traverse it.

main(X):-
	knights(X, _).

wtime(T):- statistics(runtime, [_,T]).

knights(N, Board):-
        wtime(_),
        make_empty_board(N, Board),
        Moves is N * N,
        access_cell(1, 1, Board, First_Cell),
        traverse_board(Moves, First_Cell, Moves),
        !.
knights(_N, _Board):-        %% Failed!
        fail.


 %% A board is a structure board/N, where each element is a structure
 %% row/N. Each member of the structure has a list of the cells the
 %% knight can jump to and an identifier which reflects the fact that
 %% the knight has been there.

make_empty_board(N, Board):-
        functor(Board, board, N),
        make_empty_board_(N, N, Board),
        link_cells(N, N, N, Board).

make_empty_board_(0, _N, _Board).
make_empty_board_(Act, Size, Board):-
        Act > 0,
        arg(Act, Board, Row),
        functor(Row, row, Size),
        Next is Act - 1,
        make_empty_board_(Next, Size, Board).

link_cells(0, _Y, _Size, _Board).
link_cells(Act_X, 0, Size, Board):-
        Act_X > 0,
        New_X is Act_X - 1,
        link_cells(New_X, Size, Size, Board).
link_cells(Act_X, Act_Y, Size, Board):-
        Act_X > 0,
        Act_Y > 0,
        New_Y is Act_Y - 1,
        access_cell(Act_X, Act_Y, Board, cell(_Id, Neighbours)),
        findall(coord(Nx, Ny), 
                next_step(Act_X, Act_Y, Size, Nx, Ny), 
                Coord_Neighbours),
        link_neighbours(Coord_Neighbours, Board, Neighbours),
        New_Y is Act_Y - 1,
        link_cells(Act_X, New_Y, Size, Board).

link_neighbours([], _B, []).
link_neighbours([coord(X, Y)|Coords], Board, [Cell|Cells]):-
        access_cell(X, Y, Board, Cell),
        link_neighbours(Coords, Board, Cells).                      

access_cell(X, Y, Board, Cell):-
        arg(X, Board, Row),
        arg(Y, Row, Cell).


 %% Traverse the board: assign a step number to each position,
 %% incrementally filling in the board. The next step is non
 %% deterministically chosen. At the end of the tour, the knight must
 %% be again on the first position.

traverse_board(0, cell(Act, _N), Act).
traverse_board(Act, cell(Act, Neighbours), Size):-
        Act > 0,
        NewAct is Act - 1,
        member(NewCell, Neighbours),
        traverse_board(NewAct, NewCell, Size).

 %% 8 possible movements, given a position and a board size.

next_step(X, Y, _Size, Nx, Ny):-
        X > 1, Y > 2,                 Nx is X - 1, Ny is Y - 2.
next_step(X, Y, _Size, Nx, Ny):-
        X > 2, Y > 1,                 Nx is X - 2, Ny is Y - 1.
next_step(X, Y, Size, Nx, Ny):-
        X > 2, Y < Size,              Nx is X - 2, Ny is Y + 1.
next_step(X, Y, Size, Nx, Ny):-
        X > 1, Y  < Size - 1,         Nx is X - 1, Ny is Y + 2.
next_step(X, Y, Size, Nx, Ny):-
        X < Size, Y < Size - 1,       Nx is X + 1, Ny is Y + 2.
next_step(X, Y, Size, Nx, Ny):-
        X < Size - 1, Y < Size,       Nx is X + 2, Ny is Y + 1.
next_step(X, Y, Size, Nx, Ny):-
        X < Size - 1, Y > 1,          Nx is X + 2, Ny is Y - 1.
next_step(X, Y, Size, Nx, Ny):-
        X < Size, Y  > 2,             Nx is X + 1, Ny is Y - 2.