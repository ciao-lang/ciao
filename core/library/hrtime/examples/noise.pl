:- module(_, [main/0], []).

% Example: measure continously the execution time of foo/0 predicate,
%   and display an (ascii) graph w.r.t. estimated filtered value. We
%   can expect to see the see spikes when the CPU is doing other
%   tasks.

:- use_module(library(hrtime)).
:- use_module(library(between)).

% TODO: does it make sense to use a Kalman filter here?

% One dimensional Kalman filter
%
%  x: filtered value
%  q: covariance of process noise
%  r: covariance of measurement noise
%  p: covariance of estimation error 
%  k: Kalman gain

q(0.00001).
r(0.00001).

% s(X,P).
:- data s/2.

upd(Measurement, X) :-
	retract_fact(s(X0, P0)), !,
	upd_(Measurement, X0, X, P0, P),
	assertz_fact(s(X, P)).
upd(_Measurement, X) :-
	X = 1200.0,
	% X = Measurement,
	assertz_fact(s(X, 0)).

upd_(Measurement, X0, X, P0, P) :-
%%	K is 0.001,
%%	P is P0,
%%	X is X0*(1-K)+Measurement*K.
 	q(Q),
 	r(R),
 	P1 is P0 + Q,
 	K is P1 / (P1 + R),
 	X is X0 + K * (Measurement - X0),
 	P is (1 - K) * P1.

main :-
	( repeat,
	  hrtime(X),
	  foo,
	  hrtime(Y), 
	  T is Y-X,
	  upd(T, EstT),
%	  display((EstT, T)), nl,
	  Err is (T - EstT)/500.0,
	  ( Err < 0 -> Err2 is -Err ; Err2 = Err ),
	  Err3 is integer(Err2),
	  ( Err3 > 70 -> Err4 = 70 ; Err4 = Err3 ),
%	  display(Err4), nl,
	  dispbar(Err4), nl,
	  fail 
	; true 
	).

dispbar(X) :- X =< 0, !.
dispbar(X) :- display('*'), X1 is X - 1, dispbar(X1).

foo :- between(1,1000,_),fail;true.