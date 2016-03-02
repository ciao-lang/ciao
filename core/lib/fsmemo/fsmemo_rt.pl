:- module(fsmemo_rt, [], [assertions, dcg, regtypes, fsyntax, hiord]).

:- doc(title, "Runtime for fsmemo").
:- doc(author, "Jose F. Morales").

:- use_module(library(port_reify), [once_port_reify/2, port_call/1]).
:- use_module(library(system),
	[file_exists/1, modif_time/2, modif_time0/2]).

:- include(library(fsmemo/fsmemo_defs)).

% ---------------------------------------------------------------------------

:- data task_status/2.

set_status(Key, Status) :-
	retractall_fact(task_status(Key, _)),
	assertz_fact(task_status(Key, Status)).

clean_status :-
	retractall_fact(task_status(_, _)).

% ---------------------------------------------------------------------------

:- export(fsmemo_call/1).
:- pred fsmemo_call(Tasks) # "Schedules and runs the tasks preserving
   the dependency order and reusing already computed results.".

fsmemo_call(Tasks) :-
	clean_status,
	once_port_reify(schedule_tasks(Tasks, Bs, []), Port),
	clean_status,
%	( member(B, Bs), display(B), nl, fail ; true ),
	port_call(Port),
	run_tasks(Bs).

run_tasks([]).
run_tasks([Task|Tasks]) :-
	run_task(Task), run_tasks(Tasks).

run_task(Task) :-
	( Task = true -> true
	; 'fsmemo.run'(Task) -> true
	; throw(error(failed(Task), fsmemo_call/1))
	).

% Schedule tasks
schedule_tasks([], R, R) :- !.
schedule_tasks([Task|Tasks], R1, R) :- !,
	schedule_task(Task, R1, R2),
	schedule_tasks(Tasks, R2, R).

% Schedule task execution sequentially based on dependencies
schedule_task(Task, R1, R) :-
	get_task_key(Task, Key),
	( task_status(Key, Status) -> true
	; Status = none
	),
	( Status = processed -> % TODO: rename to outdated?
	    R1 = R
	; Status = processing ->
	    throw(error(cycle(Task), fsmemo_call/1))
	; Status = file ->
	    R1 = R
	; Status = done ->
	    R1 = R
	; Task = 'SOURCE'(_) ->
	    ( key_to_file(Key, File), file_exists(File) ->
	        set_status(Key, file),
		R1 = R
	    ; throw(error(missing_source(Key), fsmemo_call/1))
	    )
	; /*Status = none,*/
	  set_status(Key, processing),
	  'fsmemo.deps'(Task, Deps),
	  schedule_tasks(Deps, R1, R2),
	  get_task_keys(Deps, DepKeys),
	  ( subsumed_target(Key, DepKeys) ->
	      NewStatus = done, % No need to recompute
	      R2 = R
	  ; NewStatus = processed,
	    R2 = [Task|R]
	  ),
	  set_status(Key, NewStatus)
	).

% The task with key Key, which strictly depends on tasks with
% keys DepKeys, does not need to be recomputed in the current
% state.
subsumed_target(Key, DepKeys) :-
	% Key exists and is newer than all DepKeys
	newer_data(DepKeys, Key),
	% No dependant target has been marked for recomputation
	\+ (member(DepKey, DepKeys),
	    task_status(DepKey, processed)).

get_task_keys([], []).
get_task_keys([Task|Xs], [Key|Ys]) :-
	get_task_key(Task, Key),
	get_task_keys(Xs, Ys).

get_task_key(Task, Key) :-
	Task = 'SOURCE'(AbsFile),
	!, % (special entry)
	Key = AbsFile.
get_task_key(Task, Key) :-
	'fsmemo.key'(Task, Key).

newer_data(DepKeys, Key) :-
	key_to_file(Key, File),
	newer_data_(DepKeys, File).

newer_data_([], _) :- !.
newer_data_([DepKey|DepKeys], File) :-
	newer_data__(DepKey, File),
	newer_data_(DepKeys, File).

newer_data__(DepKey, File) :-
	key_to_file(DepKey, DepFile),
	up_to_date(File, DepFile).

key_to_file(X,X). % we assume that Keys are absolute file names

% (Assumes that source file exists)
up_to_date(Target, Source) :-
	modif_time(Source, SourceTime),
	modif_time0(Target, TargetTime),
	SourceTime =< TargetTime.
