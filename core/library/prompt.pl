
:- module(prompt,
	[ prompt_for/1,
	  prompt_for/2,
	  prompt_for_default/2,
	  prompt_for_default/3
	],
	[
	]).

prompt_for(Atom):- prompt_for(user,Atom).

prompt_for(S,Atom):-
	get_code(S,C),
	prompt_for_string(C,S,String),
	name(Atom,String).

prompt_for_default(Atom,Default):-
	prompt_for_default(user,Atom,Default).

prompt_for_default(S,Atom,Default):-
	prompt_for(S,Atom0),
	prompt_for_end(Atom0,Default,Atom).

prompt_for_end('',Def,Def):- !.
prompt_for_end(Atom,_Def,Atom).

prompt_for_string(-1,_S,""):- !.
prompt_for_string(10,_S,""):- !.
prompt_for_string(C,S,[C|More]):- !,
	get_code(S,C1),
	prompt_for_string(C1,S,More).
