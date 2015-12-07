:- agent(one,[inform/2]).
:- protocol('actmods/platformbased').

:- use_module(library(prompt)).

agent:-
    prompt_for(_), nl,
    go.

go:-
    display('Send?- '),
    prompt_for(Mess),
    another::inform(Mess),
    receive(another,Answer),
    display('another: '), display(Answer), nl,
    go.

:- concurrent answer/2.

message(Agent,Answer):-
    assertz_fact(answer(Agent,Answer)).

receive(Agent,Answer):-
    retract_fact(answer(Agent,Answer)).
