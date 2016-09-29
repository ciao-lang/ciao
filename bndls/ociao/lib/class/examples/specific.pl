%%----------------------------------------------%%
%% This class provides additional functionality %%
%% to the "generic" class.                      %%
%%----------------------------------------------%%
:- class(specific).

% Establish an inheritance relationship with class "generic".
:- inherit_class(library(class/examples/generic)).

% Override inherited datum/1.
% datum/1 is said to be overriden because there are both an
% inherited definition (from class "generic") and a local one,
% which overrides the one inherited.
:- data datum/1. 
:- inheritable datum/1.

% Extend the public interface inherited from "generic".
% note that set/1 and a_virtual/0 are also overriden. 
% undo/0 is a new functionality added.
:- export([set/1,undo/0]).

% Methods

set(Value) :-
	inherited datum(OldValue),
	!,
	inherited set(Value),
	asserta_fact(datum(OldValue)).
set(Value) :-
	inherited set(Value).

undo :-
        retract_fact(datum(Last)), !,
        asserta_fact(inherited(datum(Last))).
undo :-
	retractall_fact(inherited(datum(_))).

% Constructor
specific :-
	generic,
	retractall_fact(inherited(datum(_))),
	display(' specific class constructor '),
	nl.

% Destructor
destructor :-
	display(' specific class destructor '),
	nl.

% Predicates

% New implementation of a_virtual/1. 
% Since this predicate was declared virtual, the
% implementation below will be called from the inherited 
% method callme/0 instead of the version defined at "generic".
a_virtual(specific).
