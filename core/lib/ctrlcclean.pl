:- module(ctrlcclean, [], [noprelude, datafacts]).

% TODO: incomplete, make it more robust (handle signals INT, TERM)

:- use_module(engine(basiccontrol)).
:- use_module(engine(term_basic)).
:- use_module(engine(exceptions)).
:- if(defined(optim_comp)).
:- use_module(engine(rt_exp), [rt_modexp/4]).
:- endif.

:- use_module(library(system), [delete_file/1, working_directory/2]).

:- export(ctrlc_clean/1).
:- meta_predicate(ctrlc_clean(goal)).
ctrlc_clean(Goal) :- intercept(Goal, control_c, ctrlcclean).

:- data del_on_ctrlc/2.

% TODO: document usage, it must be ended with erase/1
:- export(delete_on_ctrlc/2).
delete_on_ctrlc(File, Ref) :-
    working_directory(Dir, Dir),
    asserta_fact(del_on_ctrlc(Dir, File), Ref).

:- export(ctrlcclean/0).
ctrlcclean :-
    retract_fact(del_on_ctrlc(Dir, File)),
    working_directory(_, Dir),
    delete_file(File),
    fail.
ctrlcclean :- halt.

