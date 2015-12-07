:- module(compatible,_,[fuzzy]).

compatible(T1,T2,M):~ 
	correct_turn(T1),
	correct_turn(T2),
	disjoint(T1,T2),
	append(T1,T2,T),
	days(T,D),
	few_days(D,M1),
	intermediate_time(T,H),
	without_gaps(H,M2).

correct_turn(T):-
	correct_turn_aux(T,4), %% Number of hours of each turn
	without_repetitions(T).

correct_turn_aux([],0).
correct_turn_aux([Class|Turn],Time):-
	correct_class(Class),
	Time1 .=. Time - 1,
	correct_turn_aux(Turn,Time1).

correct_class((Day,Hour)):-
	member(Day,[l,m,x,j,v]),
	Hour .>=. 8,
	Hour .=<. 20.

% Not constructive version
without_repetitions([]):- !.
without_repetitions([X|L]):-
	is_not_member(L,X),!,
	without_repetitions(L).

% % Constructive version
% without_repetitions([]).
% without_repetitions([X|L]):-
% 	neg(member(L,X)),
% 	without_repetitions(L).

% Not constructive version
disjoint([],_L2):- !.   
disjoint([X|L1],L2):-
	is_not_member(L2,X),!,
	disjoint(L1,L2).

is_not_member([],_X).
is_not_member([(Y1,Y2)|L2],(X1,X2)):-
	(X1 \== Y1;Y2 .<>. X2),     
	is_not_member(L2,(X1,X2)).

% % Constructive version
% disjoint([],_L2).
% disjoint([X|L1],L2):-
% 	neg(member(L2,X)),
% 	disjoint(L1,L2).

	
append([],L2,L2).
append([X|L1],L2,[X|L]):-
	append(L1,L2,L).

days(T,D):-
	days_aux(T,DaysList),
	lenght(DaysList,D).

days_aux([],[]).
days_aux([(Day,_Hour)|T],DaysList):-
	days_aux(T,DaysL),
	insert(Day,DaysL,DaysList).

insert(Day,[],[Day]).
insert(Day,[Day|L],[Day|L]):-
	!.
insert(Day,[Day1|L1],[Day1|L]):-
	insert(Day,L1,L).

lenght([],0).
lenght([_X|L],Len):-
	lenght(L,Len1),
	Len .=. Len1 + 1.

few_days :# fuzzy_predicate([(0,1), (1,0.8), (2, 0.6), (3,0.4), (4,0.2), (5,0)]).

intermediate_time(T,H):-
	timetable_per_day(T,(T1,T2,T3,T4,T5)),
	intermediate_time_per_day(T1,H1),
	intermediate_time_per_day(T2,H2),
	intermediate_time_per_day(T3,H3),
	intermediate_time_per_day(T4,H4),
	intermediate_time_per_day(T5,H5),
	H .=. H1 + H2 + H3 + H4 + H5.

timetable_per_day([],([],[],[],[],[])).
timetable_per_day([(l,H)|T],(T1A,T2,T3,T4,T5)):-
	timetable_per_day(T,(T1,T2,T3,T4,T5)),
	insert_order((l,H),T1,T1A).
timetable_per_day([(m,H)|T],(T1,T2A,T3,T4,T5)):-
	timetable_per_day(T,(T1,T2,T3,T4,T5)),
	insert_order((m,H),T2,T2A).
timetable_per_day([(x,H)|T],(T1,T2,T3A,T4,T5)):-
	timetable_per_day(T,(T1,T2,T3,T4,T5)),
	insert_order((x,H),T3,T3A).
timetable_per_day([(j,H)|T],(T1,T2,T3,T4A,T5)):-
	timetable_per_day(T,(T1,T2,T3,T4,T5)),
	insert_order((j,H),T4,T4A).
timetable_per_day([(v,H)|T],(T1,T2,T3,T4,T5A)):-
	timetable_per_day(T,(T1,T2,T3,T4,T5)),
	insert_order((v,H),T5,T5A).

insert_order((D,H),[],[(D,H)]).
insert_order((D,H),[(D,H1)|T],[(D,H),(D,H1)|T]):-
	H .=<. H1.
insert_order((D,H),[(D,H1)|T],[(D,H1)|T1]):-
	H .>. H1,
	insert_order((D,H),T,T1).


intermediate_time_per_day([],0).
intermediate_time_per_day([(_D,H)|T],Hours):-
	intermediate_time_per_day_aux(T,H,Hours).

intermediate_time_per_day_aux([],_H,0).
intermediate_time_per_day_aux([(_D,H1)|T],H,Hours):-
	Num .=. H1 - H - 1,
	intermediate_time_per_day_aux(T,H1,Hours1),
	Hours .=. Hours1 + Num.
 
without_gaps(X,0) :- X .<.0.
without_gaps :# fuzzy_predicate([(0,1), (1,0.8), (5, 0.3), (7,0.1), (8,0)]).


proof1(M):-
	compatible([(l,8),(l,9),(j,9),(j,10)],[(x,16),(m,17),(j,15),(j,16)],M).
proof2(M):-
	compatible([(l,8),(l,9),(l,10),(j,10)],[(l,13),(l,14),(j,11),(j,12)],M).
proof3(M):-
	compatible([(l,8),(l,9),(m,9),(v,12)],[(m,9),(m,10),(v,13),(v,14)],M).
