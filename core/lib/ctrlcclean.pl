:- module(ctrlcclean, [ctrlc_clean/1, delete_on_ctrlc/2, ctrlcclean/0],
	[assertions]).

:- use_module(library(system), [delete_file/1, working_directory/2]).

:- meta_predicate(ctrlc_clean(goal)).

ctrlc_clean(Goal) :- intercept(Goal, control_c, ctrlcclean).

:- data delOnCtrlC/2.

% todo: (jfran) when is delOnCtrlC retracted?
delete_on_ctrlc(File, Ref) :-
        working_directory(Dir, Dir),
        asserta_fact(delOnCtrlC(Dir, File), Ref).

ctrlcclean :-
        retract_fact(delOnCtrlC(Dir, File)),
        working_directory(_, Dir),
        delete_file(File),
        fail.
ctrlcclean :- halt.
