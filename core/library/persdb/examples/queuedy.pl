:- module(queuedy, [main/1],[persdb,iso]).

:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(aggregates)).

:- persistent(queue/1, queue_dir).

queue(first).
queue(second).

main([]) :- error('You have to provide a directory').
main([Dir]) :-
        asserta_fact(persistent_dir(queue_dir,Dir)),
        initialize_db,
        main.

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
     assertz_fact(queue(Term)),
     main.
handle_action(slip(Term)) :-
     asserta_fact(queue(Term)),
     main.
handle_action(out) :-
     (  retract_fact(queue(Term))
     -> write('Out '), write(Term)
     ;  write('FIFO empty.') ),
     nl,
     main.
handle_action(list) :-
     findall(Term,queue(Term),Terms),
     write('Contents: '), write(Terms), nl,
     main.
