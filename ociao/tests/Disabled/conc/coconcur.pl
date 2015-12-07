%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- class(coconcur,[],[]).

:- use_module(mess, [mess/1]). 

:- export([main/1]). 

main(X) :-
	retract_fact(mess(X)),
	asserta_fact(mess(X)).
