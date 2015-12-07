%%----------------------------------------------%%
%% A generic class for item storage.            %%
%%----------------------------------------------%%
:- class(generic).

% Public interface declaration:
:- export([set/1,get/1,callme/0]).

% An attribute
:- data datum/1.

% Inheritance declaration: datum/1 will be available to 
% descendant classes (if any).
:- inheritable(datum/1).

% Attribute initialization: attributes are easily initialized
% by writing clauses for them.
datum(none).

% Methods

set(X) :-
        type_check(X),
        set_fact(datum(X)).

get(X) :-
        datum(X).

callme :-
	a_virtual(IMPL),
	display(IMPL),
	display(' implementation of a_virtual/0 '),
	nl.

% Constructor: in this case, every time an instance
% of this class is created, it will display a message.
generic :-
	display(' generic class constructor '),
	nl.

% Destructor: analogous to the previous constructor,
% it will display a message every time an instance
% of this class is eliminated.
destructor :-
	display(' generic class destructor '),
	nl.

% Predicates:
% cannot be called as messages (X:method)

% Virtual declaration: tells the system to use the most
% descendant implementation of a_virtual/1 when calling
% it from inside this code (see callme/0).
% If there is no descendant implementation for it, 
% the one defined bellow will be used.
:- virtual a_virtual/1.

a_virtual(generic).

:- virtual type_check/1.

type_check(X) :-
	nonvar(X).
