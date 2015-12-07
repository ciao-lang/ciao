%%----------------------------------------------%%
%% A class for stacks.                          %%
%%----------------------------------------------%%

%% Class declaration: the current source defines a class.
:- class(stack,[],[]).

% State declaration: storage/1 is an attribute.
:- dynamic storage/1.

% Interface declaration: the following predicates will
% be available at run-time.
:- export(push/1).
:- export(pop/1).
:- export(top/1).
:- export(is_empty/0).

% Methods

push(Item) :-
	nonvar(Item), 
	asserta_fact(storage(Item)).

pop(Item) :-
	var(Item),
	retract_fact(storage(Item)).

top(Top) :-
	storage(Top), !.

is_empty :-
	storage(_), !, fail.
is_empty.
