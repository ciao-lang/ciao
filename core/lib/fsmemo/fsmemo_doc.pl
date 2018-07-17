:- use_package(assertions).
:- doc(nodoc,assertions).

:- doc(title,"Execution of tasks with file-based memoization").

:- doc(author, "Jose F. Morales").

:- doc(module, "
This package offers a minimalistic interface to execute tasks with
file-based memoization. Tasks produce output in files and take as
input files or the output of other tasks. Tasks are memoized (not
recomputed unless their input data has changed). Detection of changes
in data is approximated by timestamps.

Definition of a task:
@begin{verbatim}
   'fsmemo.deps'(Task, Deps): Obtain the dependent tasks of Task
   'fsmemo.key'(Task, Key): Obtain the Key associated to the Task
   'fsmemo.run'(Task): Run the Task
@end{verbatim}

We assume that:
@begin{itemize}
@item The @var{Key} is an absolute file name with the result of the
  task computation.
@item Task running code does not invoke any task directly.
@end{itemize}

The @pred{fsmemo_call/1} predicate schedules and runs the tasks
preserving the dependency order.
").

:- doc(bug, "Use nested syntax? E.g., task(A,B).deps(Deps) :- ...").
:- doc(bug, "Document predicates, exceptions").
:- doc(bug, "Do not use find_file/2").
:- doc(bug, "Improve documentation").
:- doc(bug, "Add locks and parallel execution, etc. (reuse existing
   parallelism and memoization/tabling features in Ciao)").

