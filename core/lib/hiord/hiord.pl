:- package(hiord).
:- set_prolog_flag(read_hiord, on).
:- use_module(engine(hiord_rt)).

:- if(defined(optim_comp)).
:- use_module(engine(rt_exp), [rt_pgcall/2, rt_modexp/4, rt_exp/6]). % TODO: put a $ in the name % Opt? % TODO: make it really optional
:- endif.
