:- module(_, [main/0], [condcomp]).

% A test suite for conditional compilation
% Author: Jose F. Morales

:- if(current_prolog_flag(dialect, ciao)).
dialect(ciao).
:- else.
dialect(other).
:- endif.

% Test 1
:- if(true).
p(1, ok).
:- elif(true).
p(1, bad_elif).
:- else.
p(1, bad_else).
:- endif.

% Test 2
:- if(fail).
p(2, bad_if).
:- elif(true).
p(2, ok).
:- else.
p(2, bad_else).
:- endif.

% Test 3
:- if(fail).
p(3, bad_if).
:- elif(fail).
p(3, bad_elif).
:- else.
p(3, ok).
:- endif.

% Test 4
:- if(true).
p(4, ok).
:- elif(fail).
p(4, bad_elif).
:- else.
p(4, bad_else).
:- endif.

% Test 5
:- if(true).
p(5, ok).
:- else.
p(5, bad_else).
:- endif.

% Test 6
:- if(fail).
p(6, bad_if).
:- else.
p(6, ok).
:- endif.

% Test 7
:- if(true).
p(7, ok).
:- elif(fail).
p(7, bad_elif).
:- endif.

% Test 8
:- if(fail).
p(8, bad_if).
:- elif(true).
p(8, ok).
:- endif.

% Test 9
:- if(fail).
    :- if(fail).
    p(9, bad_if).
    :- elif(true).
    p(9, bad_elif).
    :- endif.
:- elif(true).
p(9, ok).
:- endif.

% Test 10
:- if(true).
    :- if(fail).
    p(10, bad_if).
    :- elif(true).
    p(10, ok).
    :- endif.
:- elif(fail).
p(10, bad_elif).
:- endif.

test_p :-
	% This should display no bad_case
	( p(I, Status),
	  \+ Status = ok,
	  display(bad_case(I, Status)), nl,
	  fail
	; true
	),
	% This should display no missing_ok
	( p(I, _), \+ (p(I, Status), Status = ok),
	  display(missing_ok(I)), nl,
	  fail
	; true
	).

main :-
	test_p,
	dialect(D),
	display(dialect(D)),
	nl.
