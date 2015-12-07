:- module(agent_call, [(::)/2,self/1], [assertions]).

:- use_module(library(actmods/actmodrt), [remote_call/2]).
:- use_module(user, ['$agent$address'/2]).

:- multifile '$actmod$name'/1.
:- data '$actmod$name'/1.

:- pred self(Name) : var => atm
    # "returns the @var{Name} of the agent.".

self(X):- current_fact('$actmod$name'(X)).

:- pred ::(Agent,Message)
    # "sends @var{Message} to @var{Agent}.".

::(Agent,Message):-
	current_fact('$actmod$name'(I)), !,
	Message=..[F|As],
	Call=..[F,I|As],
	'$agent$address'(Agent,Add),
	remote_call(Add, Call).
::(Agent,Message):-
	throw(no_sender_id(::(Agent,Message))).
