#!/usr/bin/env ciao-shell
% -*- mode: ciao; -*-

:- use_package([assertions]).

:- use_module(library(read), [read_term/3]).
:- use_module(library(write), [numbervars/3]).
:- use_module(library(format)).

:- doc(title,"Finding differences between two Prolog files").

:- doc(author,"Francisco Bueno").

:- doc(module,"This simple program works like the good old diff but for
   files that contain Prolog code. It prints out the clauses that it finds
   are different in the files. Its use avoids textual differences such as
   different variable names and different formatting of the code in the
   files.

   @section{Usage (pldiff)}

   @begin{verbatim}
   @includefact{usage_text/1}
   @end{verbatim}

   but you can also use the program as a library and invoke the predicate:

   @begin{verbatim}
   pldiff( <filename> , <filename> )
   @end{verbatim}
   ").

:- doc(bug,"Currently uses variant/2 to compare clauses. This is useful,
   but there should be an option to select the way clauses are compared,
   e.g., some form of equivalence defined by the user.").


main([FileA,FileB]):- !,
	pldiff(FileA,FileB).
main(['-h']):-
	usage.
main(Args):-
	format(user_error,"ERROR: Invalid arguments ~w~n",[Args]),
	usage.

usage :-
	usage_text(TextS),
	format(user_error,"Usage: ~n~s",[TextS]).

usage_text("
	pldiff <file1> <file2>
	   : find differences 

	pldiff -h
	   : print this information
").

pldiff(FileA,FileB):-
	open(FileA,read,StreamA),
	open(FileB,read,StreamB),
	new_queue(QueueA),
	new_queue(QueueB),
	diff(StreamA,StreamB,QueueA,QueueB,0,0,1,2),
	close(StreamA),
	close(StreamB).

diff(StreamA,StreamB,QueueA,QueueB,LCA,LCB,IdA,IdB):-
        read_term(StreamA,LineA,[variable_names(DictA)]),
	diff_line(LineA,DictA,StreamA,StreamB,QueueA,QueueB,LCA,LCB,IdA,IdB).

diff_line(end_of_file,_Dict,_Stream,StreamB,_Queue,QueueB,_LC,LCB,_Id,IdB):- !,
	dump(QueueB,IdB),
	output(StreamB,LCB,IdB).
diff_line(LineA,_DictA,StreamA,StreamB,QueueA,QueueB,LCA,LCB,IdA,IdB):-
	split(QueueB,LineA,LeftQueueB,RightQueueB), !,
	LCA1 is LCA+1,
	dump_queues(IdA,IdB,QueueA,LeftQueueB),
	new_queue(NewQueueA),
	diff_next_line(RightQueueB,NewQueueA,StreamB,StreamA,LCB,LCA1,IdB,IdA).
diff_line(LineA,DictA,StreamA,StreamB,QueueA,QueueB,LCA,LCB,IdA,IdB):-
	LCA1 is LCA+1,
	enqueue(QueueA,line(LineA,DictA,LCA1),NewQueueA),
	diff(StreamB,StreamA,QueueB,NewQueueA,LCB,LCA1,IdB,IdA).

diff_next_line(QueueA,QueueB,StreamA,StreamB,LCA,LCB,IdA,IdB):-
	empty_queue(QueueA), !,
	diff(StreamA,StreamB,QueueA,QueueB,LCA,LCB,IdA,IdB).
diff_next_line(QueueA,QueueB,StreamA,StreamB,LCA,LCB,IdA,IdB):-
	diff(StreamB,StreamA,QueueB,QueueA,LCB,LCA,IdB,IdA).

split(Queue,_Line,_LeftQueue,_RightQueue):-
	empty_queue(Queue), !,
	fail.
split(Queue,Line0,LeftQueue,RightQueue):-
	dequeue(Queue,line(Line,_Dict,_LC),NewQueue),
	equal_lines(Line0,Line), !,
	new_queue(LeftQueue),
	RightQueue = NewQueue.
split(Queue,Line,LeftQueue,RightQueue):-
	dequeue(Queue,Item,NewQueue),
	enqueue(NewLeftQueue,Item,LeftQueue),
	split(NewQueue,Line,NewLeftQueue,RightQueue).

new_queue(queue(X,X)).

empty_queue(queue(X,Y)):- X==Y.

enqueue(queue(H,[X|T]),X,queue(H,T)).

dequeue(queue([X|H],T),X,queue(H,T)).

dump_queues(1,2,QueueA,QueueB):-
	dump_queues_in_order(QueueA,QueueB).
dump_queues(2,1,QueueB,QueueA):-
	dump_queues_in_order(QueueA,QueueB).

dump_queues_in_order(QueueA,QueueB):-
	empty_queue(QueueA),
	empty_queue(QueueB), !.
dump_queues_in_order(QueueA,QueueB):-
	format("*** diff found:~n",[]),
	dump(QueueA,1),
	dump(QueueB,2).

dump(Queue,_Id):-
	empty_queue(Queue), !.
dump(Queue,Id):-
	dequeue(Queue,line(Line,Dict,LC),NewQueue),
	dump_line(Line,Dict,LC,Id),
	dump(NewQueue,Id).

output(Stream,LC,Id):-
        read_term(Stream,Line,[variable_names(Dict)]),
	( Line = end_of_file
	-> true
	 ; LC1 is LC+1,
	   dump_line(Line,Dict,LC1,Id),
	   output(Stream,LC1,Id)
	).

dump_line(Line,Dict,LC,Id):-
	unify_vars(Dict),
	format("~w: ~w: ~q~n",[Id,LC,Line]).

unify_vars([]).
unify_vars([N=V|Dict]):-
	V='$VAR'(N),
	unify_vars(Dict).

equal_lines(LineA,LineB):-
	variant(LineA,LineB).

variant(Term1,Term2):-
	\+ \+
        (  numbervars(Term1,0,N),
	   numbervars(Term2,0,N),
	   Term1 = Term2
        ).
