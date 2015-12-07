:- module(persistentdb, [main/0], [assertions, unittestdecls, fsyntax]).

:- use_module(library(format)).
:- use_module(library(system)).
:- use_module(library(terms)).

:- use_module(ciaobld(ciaoc_aux), [invoke_ciaoc/1]).
:- use_module(library(process)).

:- data where_command/2 .

command_name(queue).
command_name(example_dynamic).
command_name(example_static).

initialize :-
	( absolute_file_name(library(persdb), PersDB),
	  atom_concat(Dir, 'persdb.pl', PersDB),
	  command_name(Command),
	  atom_concat([Dir, 'tests/', Command], WCommand),
	  assertz_fact(where_command(Command,WCommand)),
	  fail
	; true
	).

:- test main.

main :- 
	initialize,
        format("Compiling queue~n", []),
        where_command(queue, Queue),
	% TODO: '-x' is needed because the test program lives in lib/ or library/!
	%   (this seems a bug)
        invoke_ciaoc(['-x', Queue]),
        format("Starting queue process~n", []),
        process_call(Queue, [], [stdin(stream(Stdin)), background(QueueP)]),
        format(Stdin, "in(~a).~n", [a]),
        format(Stdin, "in(~a).~n", [b]),
        format(Stdin, "halt.~n", []),
	flush_output(Stdin),
	close(Stdin),
	process_join(QueueP),
	nl,
	format("Waiting for next persdb test~n~n", []),
	pause(1),
	ex_s_main,	
	nl,
	format("Waiting for next persdb test~n~n", []),
	pause(1),
	ex_d_main,
        format("Not doing more persdb/queue tests! Please write some more!~n",[]).

ex_s_main :-
        format("Compiling an static example of persdb~n", []),
        where_command(example_static, Static),
        invoke_ciaoc(['-x', Static]),
        format("Starting static example process~n", []),
	sub_main(Static, 5), nl.

ex_d_main :-
        format("Compiling a dynamic example of persdb~n", []),
        where_command(example_dynamic, Dynamic),
        invoke_ciaoc(['-x', Dynamic]),
        format("Starting dynamic example process~n", []),
	sub_main(Dynamic, 5), nl.


sub_main(Command, N) :-
	N > 0, !,
	process_call(Command, [], [stdin(stream(Stdin)), background(P)]),
	pause(1),
	number_codes(N,CN), atom_codes(AN,CN),
        format(Stdin, "~a.~n", [AN]),
	flush_output(Stdin),
	close(Stdin),
	process_join(P),
	N1 is N - 1,
	sub_main(Command, N1).
sub_main(_,_).





 %%         format("Restarting queue process~n", []),
 %%         exec(Queue, Stdin1, Stdout1),
 %%         format(Stdin1, "out.~n",[]),
 %%         get_line(Stdout1, OutAt1),
%%        format(Stdin1, "halt.~n", []).
%%        format("read first: ~s~n", [OutAt1]).
 %%         format(Stdin1, "out.~n",[]),
 %%         read(Stdout1, OutAt2),
 %%         format(Stdin1, "halt.~n",[]),
 %%         (
 %%             [OutAt1, OutAt2] = [At1, At2] ->
 %%              format("Ok with persistent predicates~n", [])
 %%         ;
 %%             format("Problems with persistent predicates: wrote ~w, read ~w~n",
 %%             [[At1, At2], [OutAt1, OutAt2]])
 %%         ).
