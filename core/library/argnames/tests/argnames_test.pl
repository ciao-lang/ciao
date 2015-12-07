:- module(argnames_test,[],[assertions,regtypes,argnames]).

:- doc(title, "Tests for dynamic argnames").
:- doc(author, "Jose F. Morales").

% A record for points
:- argnames point(x, y, z).

:- export(point/4).
:- test point(X, Y, Z, P) : (X = 1, Y = 2, Z = 3) =>
	(P = point(1,2,3)) # "Point constructor".
point(X, Y, Z, P) :-
	P = point${x => X, y => Y, z => Z}.

:- export(spec/0).
:- test spec + (is_det, not_fails) # "Obtaining a functor spec for a record".
spec :-
	Spec = point${/},
	Spec = point/3.

:- export(no_expand/0).
:- test no_expand + (is_det, not_fails) # "Checking that expansion does not treat
   terms that are not records".
no_expand :-
	X = noarg(no$0),
	X = noarg(X0),
	X0 =.. ['$', no, 0].

:- export(argnames/0).
:- test argnames + (is_det, not_fails) # "Obtaining the argument names".
argnames :-
	Keys = point${argnames},
	Keys = [x,y,z].

:- export(dyn/0).
:- test dyn + (is_det, not_fails) # "Accessing arguments by name dynamically at runtime".
% todo: depends on test argnames
% todo: depends on test no_expand
dyn :-
	point(1,10,100,P),
	Keys = point${argnames},
	values(Keys, P, Values),
	Values == [1, 10, 100].

values([], _P, []).
values([K|Ks], P, [V|Vs]) :-
	P = point${K => V},
	values(Ks, P, Vs).



	
	
