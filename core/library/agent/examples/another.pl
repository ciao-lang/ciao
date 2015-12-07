:- agent(another,[inform/2]).
:- protocol('actmods/platformbased').

:- use_module(library(prompt)).

agent.

message(Agent,Mess):-
    display(Agent), display(': '), display(Mess), nl,
    display('Reply?- '),
    prompt_for(Answer),
    Agent::inform(Answer).
