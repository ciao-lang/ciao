%%----------------------------------%%
%% A generic class to perform       %%
%% item storage keeping a history   %%
%% of the item values.              %%
%%----------------------------------%%
:- class(pseudo_stack).

%% Implements class "generic" via a "stack"

:- inherit_class(library(class/examples/stack)).
:- implements(library(class/examples/generic)).

callme :-
	display('Stack implementation'),
	nl.

set(Item) :-
	push(Item).

get(Item) :-
	top(Item).
