:- module(send, [send/2], [actmods, hiord]).
:- push_prolog_flag(unused_pred_warnings, no).
:- use_module(library(actmods/webbased_locate)).
:- use_active_module(agent1, [agent1/2]).
:- use_active_module(agent2, [agent2/2]).

:- meta_predicate send(pred(2), addmodule).

send(Sender, Message, I) :-
	Sender(I, Message).
:- pop_prolog_flag(unused_pred_warnings).
