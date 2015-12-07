:- module(write_tokens, [write_tokens/1], []).

:- use_module(library(strings)).
:- use_module(library(write)).
:- use_module(library(format)).

write_tokens(Xs) :-
	write_tokens_2(Xs, 0, no).

write_tokens_2([], _, _) :- !.
write_tokens_2([format(inner)|Xs], I, N) :- !,
	I1 is I + 2,
	write_tokens_2(Xs, I1, N).
write_tokens_2([format(outer)|Xs], I, N) :- !,
	I1 is I - 2,
	write_tokens_2(Xs, I1, N).
write_tokens_2([format(inner2)|Xs], I, N) :- !,
	I1 is I + 5,
	write_tokens_2(Xs, I1, N).
write_tokens_2([format(outer2)|Xs], I, N) :- !,
	I1 is I - 5,
	write_tokens_2(Xs, I1, N).
write_tokens_2([format(new_line)|Xs], I, N) :- !,
	( N = no ->
	    true
	; write(' \\')
	),
	( Xs = [format(label)|_] ->
	    I1 is I - 2
	; I1 = I
	),
	nl, write_tabs(I1),
	write_tokens_2(Xs, I, N).
write_tokens_2([format(one_line(N))|Xs], I, _) :- !,
	write_tokens_2(Xs, I, N).
write_tokens_2([format(_)|Xs], I, N) :- !,
	write_tokens_2(Xs, I, N).
write_tokens_2([X|Xs], I, N) :-
	( (X = [_|_]; X = []) ->
	    write('"'),
	    write_string(X),
	    write('"')
	; X = identifier(F, P) ->
	    format(F, P)
	; write(X)
	),
	( ommit_space(Xs) ->
	    true
	; write(' ')
	),
	write_tokens_2(Xs, I, N).

ommit_space([format(new_line)|_]) :- !.
ommit_space([format(glue)|_]) :- !.
ommit_space([format(_)|Xs]) :- 
	ommit_space(Xs).

write_tabs(I) :- I =< 0, !.
write_tabs(I) :- I > 0,
	write(' '), I1 is I - 1, write_tabs(I1).
