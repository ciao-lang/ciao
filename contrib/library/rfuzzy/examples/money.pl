:- module(money,_,[rfuzzy, clpr]).


% We define positive and negative integers.

positive_integer(X) :- X .>. 0.

% Do not use this to define valid natural numbers.
%positive_integer(1).
%positive_integer(X) :-
%	positive_integer(Y),
%	number(Y),
%	X is Y + 1,
%	X < 100. % For efficiency reasons.

negative_integer(X) :- X .<. 0.

% Do not use this to define valid natural numbers.
%negative_integer(-1).
%negative_integer(X) :-
%	negative_integer(Y),
%	X is Y - 1, 
%	X > -100. % For efficiency reasons.

valid_integer(X) :- positive_integer(X).
valid_integer(X) :- negative_integer(X).

% We define 2 functions: have_money and owe_money

rfuzzy_type_for(have_money/1, [valid_integer/1]).
rfuzzy_default_value_for(have_money/1, 0.9) if positive_integer/1 .
rfuzzy_default_value_for(have_money/1, 0.1). % Always

rfuzzy_type_for(owe_money/1, [valid_integer/1]).
rfuzzy_default_value_for(owe_money/1, 0.9) if negative_integer/1 .
rfuzzy_default_value_for(owe_money/1, 0.1). % Always
