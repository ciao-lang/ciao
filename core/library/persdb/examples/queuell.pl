:- module(queuell, [main/0],['persdb/persdb_ll',iso]).

:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(aggregates)).

persistent_dir(queue_dir,'./pers').

:- persistent(queue/1, queue_dir).

queue(first).
queue(second).

main:-
     write('Action ( in(Term). | slip(Term) | out. | list. | halt. ): '),
     read(A),
     (  handle_action(A)
     -> true
     ;  write('Unknown command.'), nl ),
     main.

handle_action(end_of_file) :-
     halt.
handle_action(halt) :-
     halt.
handle_action(in(Term)) :-
     passertz_fact(queue(Term)),
     main.
handle_action(slip(Term)) :-
     passerta_fact(queue(Term)),
     main.
handle_action(out) :-
     (  pretract_fact(queue(Term))
     -> write('Out '), write(Term)
     ;  write('FIFO empty.') ),
     nl,
     main.
handle_action(list) :-
     findall(Term,queue(Term),Terms),
     write('Contents: '), write(Terms), nl,
     main.
