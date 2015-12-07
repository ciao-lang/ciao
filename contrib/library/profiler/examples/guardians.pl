%% guardians.pl -- 
%% AFSID           : $__Header$
%% Author          : Manuel Carro Li~nares
%% Created On      : Sun May  8 17:28:20 1994
%% Last Modified By: MCL
%% Last Modified On: Tue Jun  8 19:11:18 1999
%% Update Count    : 52
%% Status          : Correct

 %%  Prison guards are playing a game. The Nth guard turns the key in every
 %%  Nth cell door, either unlocking or locking the door. A simulated run
 %%  with five guards and five cells would appear thus:
 %% 
 %%  1) Cells 1 2 3 4 5 unlocked
 %%  2) Cells 1 3 5 unlocked
 %%  3) Cells 1 5 unlocked
 %%  4) Cells 1 4 5 unlocked
 %%  5) Cells 1 4 unlocked
 %%  etc...
 %% 
 %%  At the end of the 'game', the program must return a list of unlocked
 %%  Cells, given N Guards and M Cells. 
 %% 
 %%  The jail should be represented as a list of pairs, ordered by cell
 %%  number, in the form [State, Cell], where State is either locked or
 %%  unlocked. To construct an ordered list, Jail, of cells, the relation 
 %%  make_cells(Cells,Start,State,Jail) should be used. 

:- module(guardians, [guardians/3]).


guardians(NGuardians, NLocks, OpenCells):-
        make_jail(1, NLocks, OpenJail),
        make_round(NGuardians, NLocks, OpenJail, Jail),
        extract(Jail, OpenCells).

make_round(0, _Locks, Jail, Jail).
make_round(Guardian, Locks, Jail, FinalJail):-
        Guardian > 0,
        cells_to_close(Guardian, Locks, To_Close),
        turn_key(To_Close, Jail, NewJail),
        NextGuardian is Guardian - 1,
        make_round(NextGuardian, Locks, NewJail, FinalJail).


make_jail(Locks, Locks, [cell(Locks, closed)]).
make_jail(Current, Locks, [cell(Current, closed)|Cells]):-
        Current < Locks,
        NextCell is Current + 1,
        make_jail(NextCell, Locks, Cells).

turn_key([], Jail, Jail).
turn_key([Curr|Turn], [cell(Curr, St)|Rest], [cell(Curr, NSt)|NewRest]):-
        change_state(St, NSt),
        turn_key(Turn, Rest, NewRest).
turn_key([Curr|Turn], [cell(Curr1, St)|Rest], [cell(Curr1, St)|NewRest]):-
        Curr \== Curr1,
        turn_key([Curr|Turn], Rest, NewRest).

change_state(open, closed).
change_state(closed, open).


cells_to_close(Guardian, Locks, List):- 
        cells_to_close_(Guardian, Guardian, Locks, List).

cells_to_close_(Current, _Guardian, Locks, []):- Current > Locks.
cells_to_close_(Current, Guardian, Locks, [Current|Rest]):-
        Current =< Locks,
        Next is Current + Guardian,
        cells_to_close_(Next, Guardian, Locks, Rest).
        

extract([], []).
extract([cell(N, open)|Cells], [N|RestOpen]):-
        extract(Cells, RestOpen).
extract([cell(_N, closed)|Cells], RestOpen):-
        extract(Cells, RestOpen).