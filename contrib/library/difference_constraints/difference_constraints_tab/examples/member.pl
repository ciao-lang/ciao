get_ta_member([HL|_],[HT|_],HL,HT,1).
get_ta_member([_|RL],[_|RT],L,T,N2) :-
	get_ta_member(RL,RT,L,T,N),
	N2 is N + 1.

put_member(1,L,X,[_|RLX],RX,[L|RLX],[X|RX]) :- !.
put_member(N,L,X,[HLX|RL],[HX|RT],[HLX|RLout],[HX|RTout]) :-
	N2 is N - 1,
	put_member(N2,L,X,RL,RT,RLout,RTout).

put_member_no_delay(1,L,X,[_|RLX],[_|RX],[L|RLX],[X|RX]) :- !.
put_member_no_delay(N,L,X,[HLX|RL],[HX|RT],[HLX|RLout],[HX|RTout]) :-
	N2 is N - 1,
	put_member_no_delay(N2,L,X,RL,RT,RLout,RTout).

take_out_member(0,R,R) :- !.
take_out_member(1,[_|R],R) :- !.
take_out_member(N,[H|R1],[H|R2]) :-
	N1 is N - 1,
	take_out_member(N1,R1,R2).

get_diff(0,0,_) :- !.
get_diff(N,N,Id) :- N > 0, N =\= Id.
get_diff(N,K,Id) :- 
	N > 0,
	N1 is N - 1,
	get_diff(N1,K,Id).

get_clocks([],[],_,[]) :- !.
get_clocks([S|RL],[T|RT],L,[T|RL2]) :- 
	member(S,L), !,
	get_clocks(RL,RT,L,RL2).
get_clocks([_|RL],[_|RT],L,RL2) :- !,
	get_clocks(RL,RT,L,RL2).

inv_reset(_,[]).
inv_reset(X,[Y|R]) :-
	X #=< Y,
	inv_reset(X,R).