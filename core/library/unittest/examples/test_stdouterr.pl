:- module(test_stdouterr, [p/2, q/2, pq/2], [assertions]).

% Run and show results:
%   ?- run_tests(test_stdouterr, [], [check, show_results]).
% Just show saved results (including output):
%   ?- run_tests(test_stdouterr, [], [show_results]).

:- use_module(library(write)).
:- use_module(library(streams)).

:- test p(X,Y) : (X=1) => (Y=2).

p(X, Y) :-
    write(out0(X,Y)), nl,
    Y is X + 1,
    write(out(X,Y)), nl.

:- test q(X,Y) : (X=1) => (Y=2).

q(X, Y) :-
    write(user_error, err0(X,Y)), nl(user_error),
    Y is X + 1,
    write(user_error, err(X,Y)), nl(user_error).
    
:- test pq(X,Y) : (X=1) => (Y=2).

pq(X, Y) :-
    write(out0(X,Y)), nl,
    write(user_error, err0(X,Y)), nl(user_error),
    Y is X + 1,
    write(out(X,Y)), nl,
    write(user_error, err(X,Y)), nl(user_error).
    
