:- module(agent1,[agent1/2],[persdb]).

:- use_module(send, [send/2]).

:- use_module(library(numlists), [sum_list/2]).

persistent_dir(trace_db,traces).
:- persistent(received/2, trace_db).

agent1(Sender, Msg) :-
   assertz_fact(received(Sender, Msg)),
   case(Msg, Sender).
	
case(shutdown, _) :-
   !,
   halt.
case(add(NumList), Sender) :-
   sum_list(NumList, Sum),
   send(Sender, sum(Sum)),
   !.
case(_, _).
