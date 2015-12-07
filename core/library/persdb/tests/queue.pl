:- module(queue, [main/0],[persdb]).

:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(aggregates)).

persistent_dir(queue_dir,'./pers_queue').

:- persistent(queue/1, queue_dir).

queue(first).
queue(second).

main :-
        prompt(_Old, ''),
	read(A),
	( handle_action(A) ->
	    true
	; write('Unknown command.'),
	  nl
	),
	main.

handle_action(end_of_file) :-
     halt.
handle_action(halt) :-
     halt.
handle_action(in(Term)) :-
     assertz_fact(queue(Term)),
     main.
handle_action(slip(Term)) :-
     asserta_fact(queue(Term)),
     main.
handle_action(out) :-
     (  retract_fact(queue(Term))
     -> writeq(Term),write('.'),nl
     ;  write('FIFO empty.') ),
     nl,
     main.
handle_action(list) :-
     findall(Term,queue(Term),Terms),
     write('Contents: '), write(Terms), nl,
     main.
