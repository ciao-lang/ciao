:- module(_, [tan/2, asin/2, acos/2, min/3, max/3], [assertions]).

:- doc(author, "Edison Mera").
:- doc(module, "Implements basic scientific functions that are missing
	in is/2 predicate.").

tan(X, Y) :- Y is sin(X)/cos(X).
asin(X, Y) :- Y is 2 * atan(X/(1+sqrt(1 - X*X))).
acos(X, Y) :- Y is 4 * atan(sqrt(1 - X)/(sqrt(1 + X) + sqrt(2))).
min(X, Y, Z) :- (X > Y -> Z = Y ; Z = X).
max(X, Y, Z) :- (X > Y -> Z = X ; Z = Y).
