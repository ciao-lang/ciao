
:- class(queen).

:- use_package(objects).

:- export([ attacks/2, next/0, solution/1, solve/0 ]).

% attributes
:- data column/1.
:- data line/1.
:- data n/1.
:- data neightbour/1.

% constructor
queen(Line,N,Neightbour):- 
	asserta_fact(line(Line)),
	asserta_fact(n(N)),
	asserta_fact(neightbour(Neightbour)).

solve:- 
	neightbour(Queen),
	Queen:solve, 
	line(Line),
	asserta_fact(column(1)),
	check_column(Queen,Line,1).

check_column(Queen,Line,Column):-
	Queen:attacks(Line,Column), !,
	forward(Column,Queen,NewColumn),
	check_column(Queen,Line,NewColumn).
check_column(_Queen,_Line,Column):-
	retract_fact(column(_)),
	asserta_fact(column(Column)).
	
forward(N,Queen,NewColumn):-
	n(N), !,
	Queen:next,
	NewColumn=1.
forward(Column,_Queen,NewColumn):-
	NewColumn is Column+1.

next:-
	column(Column),
	neightbour(Queen),
	forward(Column,Queen,NewColumn),
	line(Line),
	check_column(Queen,Line,NewColumn).

attacks(ThisLine,ThisColumn):-
	line(Line),
	column(Column),
	attacks_(ThisLine,ThisColumn,Line,Column).
attacks(ThisLine,ThisColumn):-
	neightbour(Queen),
	Queen:attacks(ThisLine,ThisColumn).

attacks_(_ThisLine,Column,_Line,Column).
attacks_(ThisLine,ThisColumn,Line,Column):-
	D is ThisLine+ThisColumn,
	D is Line+Column.
attacks_(ThisLine,ThisColumn,Line,Column):-
	D is ThisLine-ThisColumn,
	D is Line-Column.

solution([(Line,Column)|More]):-
	line(Line),
	column(Column),
	neightbour(Queen),
	Queen:solution(More).
