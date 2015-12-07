%%----------------------------------------------------------------------
%%
%% MYCIN RULE (very simple) DATABASE EXAMPLE
%%
%% Angel Fernandez Pineda.
%%
%% Distributed under Ciao Prolog license terms
%%
%% DESCRIPTION:
%%
%% There will be three horses at race: arrow, thunder and rolling_stone.
%% Just execute winner(X) cf CF  in order to know which horse
%% has a better chance to win...
%%
%%----------------------------------------------------------------------

:- module(horse_riding,[],[mycin]).

%%----------------------------------------------------------------------
%% CURRENT KNOWLEDGE
%%----------------------------------------------------------------------

weather(sunny)  cf 0.4.
weather(cloudy) cf 0.2.
weather(rainy)  cf 0.2.

wind(yes) cf 0.8.

floor(wet) cf 0.6.
floor(dry) cf 0.4.

%%----------------------------------------------------------------------
%% RULES
%%----------------------------------------------------------------------

:- export(winner/1).

winner(arrow) cf 0.7 :- 
	weather(sunny),
	floor(wet).

winner(arrow) cf 0.6 :- 
	weather(cloudy),
	wind(yes).

winner(arrow) cf 0.6 :- 
	weather(rainy),
	floor(dry).

%

winner(thunder) cf 0.7 :- 
	weather(sunny),
	wind(yes).

winner(thunder) cf -0.7 :- 
	weather(rainy),
	floor(wet).

winner(thunder) cf -0.7 :-
	weather(cloudy),
	floor(wet).

%

winner(rolling_stone) cf 0.9 :-
	weather(rainy),
	wind(yes).

winner(rolling_stone) cf -0.6 :-
	weather(cloudy),
	floor(dry).

winner(rolling_stone) cf -0.5 :-
	weather(sunny),
	floor(dry).
