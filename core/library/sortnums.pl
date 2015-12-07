:- module(_,[sort_numbers/2],[]).

sort_numbers(List, Sorted) :-
        sort(List, -1, S, []), !,
        Sorted = S.

sort([Head|Tail], Lim, Sorted, Rest) :- !,
        Qh = [Head|_],
        samrun(Tail, Qh, Qh, Run, Rest0),
        sort_(Rest0, 1, Lim, Run, Sorted, Rest).
sort(Rest, _, [], Rest).

sort_([Head|Tail], J, Lim, Run0, Sorted, Rest) :-
        J =\= Lim, !,
        Qh = [Head|_],
        samrun(Tail, Qh, Qh, Run1, Rest0),
        sort_(Rest0, 1, J, Run1, Run2, Rest1),
        merge(Run0, Run2, Run),
        K is J<<1,
        sort_(Rest1, K, Lim, Run, Sorted, Rest).
sort_(Rest, _, _, Sorted, Sorted, Rest).

% samrun(List, Q1, Q2, Run, Rest)

% List is a list of elements, Rest is some tail of that list,
% Run is an ordered list of the difference between List and Rest,
% Q1 is the cons containing the first element of List.
% Q2 is the last cons of Run.

samrun(Tail, _, _, _, _) :- var(Tail), !, fail.
samrun([H|Tail], QH, QT, Run, Rest) :-
        QT = [Q|QT2], 
        H >= Q, !,
        QT2 = [H|_],
        samrun(Tail, QH, QT2, Run, Rest).
samrun([H|Tail], QH, QT, Run, Rest) :-
        QH = [Q|_],
        H < Q, !,
        samrun(Tail, [H|QH], QT, Run, Rest).
samrun(Rest, Run, [_], Run, Rest).


% merge(+List, +List, -List).
merge([], L2, Out) :- !,
        Out = L2.
merge([H1|T1], L2, Out) :-
        L2 = [H2|_],
        H1 =< H2, !,
        Out = [H1|Out1],
        merge(T1, L2, Out1).
merge(L1, [H2|L2], Out) :- !,
        Out = [H2|Out1],
        merge(L1, L2, Out1).
merge(List, _, List).
