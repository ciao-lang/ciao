:- module(_, _, [profiler]).

:- use_module(library(profiler/graph_to_tex)).

:- cost_center p1/0, p2/0, q/1.

p1 :-
	q(_A),
	fail.
p1.
p1 :-
	q(f).

p2 :- q(e).

q(a).
q(b).

main2 :-
	p1,
	p2.

:- use_module(library(profiler/profiler_utils)).
:- use_module(library(profiler/profiler_base)).
t0 :-
	profile_reset,
	profile(main2).

t1 :-
        profile_dump.

t2 :-
	profile_info(A),
        A = p(Freq, _, _),
        get_profile_cc_graph(time(Freq), 
            [call_exits_c, call_fails_c,
             redo_exits_c, redo_fails_c], A, Graph),
        graph_to_tex(time(Freq), 
            [call_exits_c, call_fails_c,
             redo_exits_c, redo_fails_c], Graph).
