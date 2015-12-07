:- module(dummy, _, [profiler, expander]).

%,[catch,expander]).
%:- module(school,_,[catch,profile,expander]).
:- use_module(library(aggregates)).

%:- timing student/2.

student(aaa, aaa).
student(tom, cs342).
student(tom, cs453).

teacher(binkley, cs453).
teacher(binkley, cs342).

course(cs453, eco103, tue).
course(cs342, eco103, fri).

prog(L) :- findall((S, T, R), p(S, T, R), L).

p(S, T, R) :-
	student(S, C1), student(S, C2),
	teacher(T, C1), teacher(T, C2),
	course(C1, R, _D1), course(C2, R, _D2),
	\+(C1 = C2).
