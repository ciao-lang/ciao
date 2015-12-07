
:- module(write_board,[ write_board/2 ],[]).

% ----------------------------------------------------

main([N]):- try(N).

try(N):-
	power10(N,P),
	P1 is P+1,
	S is P1+1,
	write_headers(P1,S,N).

% ----------------------------------------------------

write_board(B,N):-
	power10(N,P),
	P1 is P+1,
	S is P1+1,
	write_headers(P1,S,N),
	write_board0(B,N,P1).

write_board0([(L,C)|B],N,S):-
	write_num(L,S),
	write_line(1,N,C),
	nl,
	write_board0(B,N,S).
write_board0([],_N,_S).

write_line(M,N,_C):-
	M>N, !.
write_line(M,N,C):-
	M1 is M+1,
	write_escaque(M,C),
	write_line(M1,N,C).

write_escaque(C,C):- !,
	display(' Q').
write_escaque(_,_):-
	display(' *').

write_num(N,S):-
	power10(N,NS),
	SS is S-NS,
	write_spaces(SS),
	display(N).

% ----------------------------------------------------

write_headers(0,_,_):- !.
write_headers(I,S,N):-
	write_spaces(S),
	I1 is I-1,
	separator(I1,0,Sep),
	N1 is floor(N/(10**I)),
	write_one_header(N1,Sep),
	L is floor((N mod (10**I))/(10**I1))+1,
	write_column_nums(1,L,Sep),
	nl,
	write_headers(I1,S,N).

separator(0,Now,Sep):- !,
	Sep is floor(2*Now).
separator(I,Now,Sep):-
	I1 is I-1,
	Now1 is Now+9*(10**I1),
	separator(I1,Now1,Sep).

write_one_header(0,_Sep):- !.
write_one_header(N,Sep):-
	N1 is N-1,
	write_column_nums(1,10,Sep),
	write_spaces(Sep),
	display(' '),
	display(0),
	write_one_header(N1,Sep).

write_column_nums(N,N,_Sep):- !.
write_column_nums(M,N,Sep):-
	write_spaces(Sep),
	display(' '),
	display(M),
	M1 is M+1,
	write_column_nums(M1,N,Sep).

write_spaces(0):- !.
write_spaces(N):-
	N1 is N-1,
	display(' '),
	write_spaces(N1).

power10(N,S):-
	log(N,10,L),
	S is floor(L).

log(X,Base,Log):- Log is log(X)/log(Base).
