% (included file) % TODO: module?

:- use_module(engine(runtime_control), [prompt/2]). % TODO: move prompt/2 to some IO related module?

:- push_prolog_flag(multi_arity_warnings, off).

:- data top_prompt/1.

:- pop_prolog_flag(multi_arity_warnings).

:- data top_prompt_base/1.

top_prompt_base('?- ').

:- export(top_prompt/2). % (not a top-level command)
% Actually, sets top_prompt_base, but since seen externally, used simpler name
top_prompt(Old, New) :-
    top_prompt_base(Old), % TODO: twice? why?
    retract_fact(top_prompt_base(Old)),
    asserta_fact(top_prompt_base(New)).

% TODO: use once_port_reify?
:- meta_predicate with_top_prompt(goal).
with_top_prompt(Goal) :-
    current_fact(top_prompt(TP)),
    prompt(Prompt, TP),
    ( true ; prompt(_, Prompt), fail ),
    call(Goal),
    prompt(_, Prompt),
    !.

:- data querylevel/1.

reset_query_level :-
    retractall_fact(querylevel(_)),
    asserta_fact(querylevel(0)),
    set_top_prompt(0).

inc_query_level :-
    retract_fact(querylevel(N)),
    N1 is N+1,
    asserta_fact(querylevel(N1)),
    set_top_prompt(N1).

dec_query_level :-
    retract_fact(querylevel(N)),
    N1 is N-1,
    asserta_fact(querylevel(N1)),
    set_top_prompt(N1).

set_top_prompt(0) :- !,
    retractall_fact(top_prompt(_)),
    top_prompt_base(P),
    asserta_fact(top_prompt(P)).

set_top_prompt(N) :-
    number_codes(N, NS),
    atom_codes(NA, NS),
    top_prompt_base(P),
    atom_concat(NA,  ' ', NS1),
    atom_concat(NS1, P,   TP),
    retractall_fact(top_prompt(_)),
    asserta_fact(top_prompt(TP)).

