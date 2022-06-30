:- module(guardians, [test_guardians/1], []).

test_guardians(Unlocked) :-
%     write('Solving the guardians and locks problem...'), nl,
    guardians(1000,200,Unlocked).
%     write('Unlocked cells: '),
%     write(Unlocked),
%     write('.'), nl.

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
    Curr =\= Curr1,
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

