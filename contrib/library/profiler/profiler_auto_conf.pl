:- module(profiler_auto_conf, [
		cc_auto_conf/4,
		cc_auto_conf/5,
		cc_auto_conf/6,
		cc_auto_conf/7,
		cc_auto_conf/8,
		clean_module/1,
		get_goal_filename/2,
		reset/1,
		record_time/1,
		tree_to_tex/2],
	    [assertions, nativeprops, foreign_interface]).

:- doc(author, "Teresa Trigo").
:- doc(module, "Profiler Auto Configuration of Cost Centers").

:- doc(summary, "This module performs an automatic cost center
           configuration for a given goal. As a result it gives the
           trace of the highest resource (e.g., time, counts, ticks,
           etc.) consuming path starting from the goal.").

% :- use_package(profiler).

% :- all_cost_center(nohooks).
% :- cost_center cc_auto_conf_iter/17.

:- use_module(library(profiler/profiler_utils)).
:- use_module(library(profiler/profiler_base)).
:- use_module(library(profiler/profiler_extra)).
:- use_module(library(profiler/profiler_type)).

:- use_module(library(profiler/graph_to_tex)).

:- use_module(library(aggregates)).
:- use_module(library(format)).
:- use_module(library(hiordlib)).
:- use_module(library(compiler), [use_module/1]).
:- use_module(library(streams)).
:- use_module(library(system)).
:- use_module(library(lists)).
:- use_module(library(llists), [flatten/2]).
:- use_module(library(file_utils)).
:- use_module(library(write)).
:- use_module(library(terms),                    [atom_concat/2]).
:- use_module(library(compiler/c_itf_internal), [defines_module/2]).
:- use_module(library(compiler/global_module_options)).
:- use_module(library(pretty_print)).

:- pred cc_auto_conf/4 :: callable * nnegint * list * term
# "Same as @var{cc_auto_conf/5} applied to time.".

:- meta_predicate cc_auto_conf(goal, ?, ?, ?).
cc_auto_conf(Goal, Cond, Goals, Tree) :-
	profile_info(ProfInfo),
	get_profile_info(frequency, ProfInfo, Freq),
	cc_auto_conf(time(Freq), Goal, Cond, Goals, Tree).

:- pred cc_auto_conf(Field, Goal, Cond, Goals, Tree) :: info_item_name *
	callable * nnegint * list * term
# "Auto configuration of cost centers in order to find the sub-graph @var{Goals}
    of the call graph that is responsible of the performance leak. @var{Goal} 
    is the call to be profiled and @var{Field} is the resource name w.r.t. 
    the optimization is made (e.g., time, counts, ticks, etc.).".

:- meta_predicate cc_auto_conf(?, goal, ?, ?, ?).

cc_auto_conf(Field, Goal, Cond, Goals, Tree) :-
	cc_auto_conf_(Field, [Module], Module, Goal, Cond, Goals, Tree, _, _).

:- meta_predicate cc_auto_conf(?, goal, ?, ?, ?, ?, ?).

cc_auto_conf(Field, Goal, Cond, Goals, Tree, ProfileInfo, NIter) :-
	cc_auto_conf_(Field, [Module], Module, Goal, Cond, Goals, Tree,
	    ProfileInfo, NIter).

:- pred cc_auto_conf/4 :: info_item_name * list * callable * term
# "Same as @var{cc_auto_conf/3} able to deal with modular programs.".

:- meta_predicate cc_auto_conf(?, ?, goal, ?, ?, ?).

cc_auto_conf(Field, Modules, Goal, Cond, Goals, Tree) :-
	cc_auto_conf_(Field, Modules, _, Goal, Cond, Goals, Tree, _, _).

:- meta_predicate cc_auto_conf(?, ?, goal, ?, ?, ?, ?, ?).

cc_auto_conf(Field, Modules, Goal, Cond, Goals, Tree, ProfileInfo, NIter) :-
	cc_auto_conf_(Field, Modules, _, Goal, Cond, Goals, Tree, ProfileInfo,
	    NIter).

:- meta_predicate get_goal_filename(goal, ?).
get_goal_filename(Goal, FN) :-
	Goal = '$:'(ExtPred),
	functor(ExtPred, FuncName, _),
	compose_name(Module, _, FuncName),
	defines_module(FN, Module) -> true.

:- doc(bug, "record_time/1 must be deleted when experiments are
	finished --EMM/MTT.").

:- meta_predicate cc_auto_conf_(?, ?, ?, goal, ?, ?, ?, ?, ?).
cc_auto_conf_(Field, Modules, Module, Goal, Cond, Goals, Tree, ProfileInfo,
	    NIter) :-
	Goal = '$:'(ExtPred),
	functor(ExtPred, FuncName, Arity),
	compose_name(Module, _, FuncName),
	defines_module(FN, Module),
	list(Modules, clean_module),
	glbmod_add_package(Modules, profiler),
	list(Modules, write_all_cc_decl),
	record_time(use_module(FN)),
	profile_reset,
	record_time((\+ profile(Goal) -> true ; true)),
	profile_info(ProfileInfo),
	get_profile_info(cc_info, ProfileInfo, CCInfo),
	get_ecc_info(ecc_list, CCInfo, InfoForEdges),
	obtain_edges(InfoForEdges, [], EdgesCG),
	obtain_nodes(InfoForEdges, ticks, NodesCG),
	PredName=FuncName/Arity ->
	cc_auto_conf_iter(Field, Goal, Modules, PredName, EdgesCG, FN, Cond,
	    [PredName], Goals, 1, 1, NodesCG, Nodes, [], Edges, [], 1, NIter),
	Tree = g(Nodes, Edges),
	list(Modules, reset).

:- multifile portray/1.

portray(n(PredName, R/T)) :-
	predname(PredName),
	num(R),
	num(T),
	display('n('),
	write(PredName),
	display(', '),
	X is R*100.0/T,
	format("~2f\%)", [X]).

write_all_cc_decl(Module) :-
	defines_module(FN, Module) ->
	atom_concat(FN, '_cc_auto.pl', FNCC),
	string_to_file(":- all_cost_center.", FNCC).

:- export(recorded_time/1).
:- data recorded_time/1.

:- meta_predicate record_time(goal).

record_time(Goal) :-
	measure(ticks, Goal, T1),
	asserta_fact(recorded_time(T1)).

:- meta_predicate cc_auto_conf_iter(?, goal, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,
	    ?, ?).
cc_auto_conf_iter(Field, Goal, Modules, PredName0, EdgesCG, FN, Cond, Goals0,
	    Goals, Value0, Total0, NodesCG, Nodes0, Nodes, Edges0, Edges,
	    NIter0, NIter) :-
	called_preds(EdgesCG, PredName0, CalledPreds),
	append(CalledPreds, Goals0, AllGoals),
	write_cc_assertions(Modules, [PredName0|AllGoals]),
	list(Modules, clean_module),
	record_time(use_module(FN)),
	profile_reset,
	record_time((\+ profile(Goal) -> true ; true)),
	NIter1 is NIter0 + 1,
	profile_info(ProfInfo),
	get_profile_cc_called_ccs(PredName0, [Field], ProfInfo, GoalCalledCCs),
	map(Goals0, goal_to_cc, CCGoals0),
	difference(GoalCalledCCs, CCGoals0, GoalCalledCCsU),
	get_profile_cc_summary_total_cc_item(Field, ProfInfo, Total),
	( percentage(Cond) ->
	    per_ccs(GoalCalledCCsU, Cond, Total, NodesCG, [], MaxCCs)
	;
	    max_ccs(GoalCalledCCsU, Cond, MaxCCs)
	),
	map(MaxCCs, cc_to_goal, Goals1, Goals0),
	Nodes0 = [n(PredName0, Value0 / Total0)|Nodes1],
	cc_auto_conf_iter_cc(MaxCCs, PredName0, Field, Goal, Modules, EdgesCG,
	    FN, Total, Cond, Goals1, Goals, NodesCG, Nodes1, Nodes, Edges0,
	    Edges, NIter1, NIter).

percentage(per(_)).
percentage(per(_, _)).

cc_to_goal(CC, [Goal|Goals], Goals) :-
	goal_to_cc(Goal, CC).

goal_to_cc(MaxCCFunc / Arity, (MaxCCFunc / Arity, _)).

:- meta_predicate cc_auto_conf_iter_cc(?, ?, ?, goal, ?, ?, ?, ?, ?, ?, ?, ?,
	    ?, ?, ?, ?).
cc_auto_conf_iter_cc([], _, _, _, _, _, _, _, _, Goals, Goals, _, Nodes, Nodes,
	    Edges, Edges, NIter, NIter).
cc_auto_conf_iter_cc([(PredName, [Value])|MaxCCs], PredName0, Field, Goal,
	    Modules, EdgesCG, FN, Total, N, Goals0, Goals, NodesCG, Nodes0,
	    Nodes, [e(PredName0, PredName, [])|Edges0], Edges, NIter0, NIter)
:- cc_auto_conf_iter(Field, Goal, Modules, PredName, EdgesCG, FN, N,
	    Goals0, Goals1, Value, Total, NodesCG, Nodes0, Nodes1, Edges0,
	    Edges1, NIter0, NIter1),
	cc_auto_conf_iter_cc(MaxCCs, PredName0, Field, Goal, Modules, EdgesCG,
	    FN, Total, N, Goals1, Goals, NodesCG, Nodes1, Nodes, Edges1, Edges,
	    NIter1, NIter).

% Given a predicate returns the ones whose consumption is bigger than N(in %)
per_ccs(Goals0, per(N), TotalT, _, _, MaxCCs) :-
	!,
	per_ccs_(Goals0, N, TotalT, MaxCCs).
per_ccs(Goals0, per(N, M), TotalT, Nodes, [], MaxCCs) :-
	length(Nodes, LN1),
	LN is LN1 - 2, %rcc and goal
	per_ccs_2(Goals0, 0, N, M, TotalT, LN, [], MaxCCs).

per_ccs_([],    _, _,     []) :- !.
per_ccs_(Goals, N, Total, MaxCCs) :-
	minimum(Goals, max_cc, (G, T)),
	Per is (T/Total) * 100,
	( Per >= N ->
	    select((G, T), Goals, Goals1),
	    MaxCCs = [(G, T)|MaxCCs1],
	    per_ccs_(Goals1, N, Total, MaxCCs1)
	;
	    per_ccs_([], N, Total, MaxCCs)
	).

% Given a predicate returns the (<M%) ones whose joint consumption is bigger 
% than N(in %)
per_ccs_2([], _, _, M, _, LN, MaxCCs0, MaxCCs) :-
	!,
	length(MaxCCs0, CCs),
	PerN is (CCs/LN) * 100,
	(PerN =< M -> MaxCCs = MaxCCs0 ; MaxCCs = []).
per_ccs_2(Goals, PartialP, N, M, TotalT, LN, MaxCCs0, MaxCCs) :-
	minimum(Goals, max_cc, (G, T)),
	PerP is (T/TotalT) * 100,
	length(MaxCCs0, CCs),
	PerN is ((CCs + 1) /LN) * 100,
	( (PerN =< M) ->
	    select((G, T), Goals, Goals1),
	    PartialP1 is PartialP + PerP,
	    per_ccs_2(Goals1, PartialP1, N, M, TotalT, LN, [(G, T)|MaxCCs0],
		MaxCCs)
	;
	    per_ccs_2([], PartialP, N, M, TotalT, LN, MaxCCs0, MaxCCs)
	).
per_ccs_2(_Goals, PartialP, N, M, TotalT, LN, MaxCCs0, MaxCCs) :-
	per_ccs_2([], PartialP, N, M, TotalT, LN, MaxCCs0, MaxCCs).


% Given a predicate returns its called N CCs more time consuming
max_ccs(Goals0, max(N), MaxCCs) :-
	!,
	max_ccs_(Goals0, N, MaxCCs).
max_ccs(Goals0, N, MaxCCs) :-
	max_ccs_(Goals0, N, MaxCCs).

max_ccs_([],     _, []) :- !.
max_ccs_(_,      0, []) :- !.
max_ccs_(Goals0, N, [MaxCC|MaxCCs]) :-
	N > 0,
	minimum(Goals0, max_cc, MaxCC),
	select(MaxCC, Goals0, Goals),
	!,
	N1 is N - 1,
	max_ccs_(Goals, N1, MaxCCs).

max_cc((_, [X]), (_, [Y])) :- X > Y.

:- pred reset/1 :: atm # "Given a name of a file erases its content".
reset(Module) :-
	defines_module(FN, Module),
	atom_concat(FN, '_cc_auto.pl', FNCC),
	string_to_file("", FNCC).

:- export(write_cc_assertions/2).
:- pred write_cc_assertions/2 # "Given a set of predicates writes a
	cost_center assertion for all of them.".
write_cc_assertions(Modules, NewCCs) :-
	member(Module, Modules),
	defines_module(FN, Module),
	findall(Pred/Arity,
	    (
		member(ModPred/Arity, NewCCs),
		compose_name(Module, Pred, ModPred)
	    ), Preds),
	atom_concat(FN, '_cc_auto.pl', FNCC),
	open_output(FNCC, SO),
	pretty_print((:- cost_center(Preds)), []),
	close_output(SO),
	fail
    ;
	true.

:- export(compose_name/3).
compose_name(_,      '$$remainder_cc', '$$remainder_cc') :- !.
compose_name(Module, Pred,             ExtPred) :-
	atom_concat([Module, ':', Pred], ExtPred),
	!.

% Given a predicate returns its called predicates (not hooks) 
% in a call graph in which all the predicates of the module are CCs
called_preds([],                                 _,        []).
called_preds([e(PredName, PredName, _)|EdgesCG], PredName, NewCCs) :- % avoid loops
	!,
	called_preds(EdgesCG, PredName, NewCCs).
called_preds([e(PredName, NewCC, _)|EdgesCG], PredName, [NewCC|NewCCs]) :-
	!,
	called_preds(EdgesCG, PredName, NewCCs).
called_preds([_|EdgesCG], PredName, NewCCs) :-
	called_preds(EdgesCG, PredName, NewCCs).

%File operations%
%---------------%

:- use_module(engine(internals),
	[po_filename/2, itf_filename/2]).

:- pred clean_module/1 : atm
# "Remove the files auto generated by the compiler (itf, po) in order
    to force recompilation.".

clean_module(Module) :-
	defines_module(FN, Module),
	clean_basename(FN).

clean_basename(FN) :-
	po_filename(FN, FNPO),
	itf_filename(FN, FNItf),
	(file_exists(FNPO) ->  delete_file(FNPO) ;  true),
	(file_exists(FNItf) -> delete_file(FNItf) ; true).

% Interesting test: Calling The profiler to optimize the
% profiler_auto_conf.pl module... It Works!!! --EMM
%
% profile(cc_auto_conf(color_map_2000, 1, G, T)).


tree_to_tex(NodeField, g(Nodes, Edges)) :-
	flatten(Edges, FlatEdges),
	map(FlatEdges, adapt_graph_edge, EdgesG),
	map(Nodes, adapt_graph_node, NodesG),
	graph_to_tex((NodeField,per), [], g(NodesG, EdgesG)).
 

adapt_graph_edge(e(S,D,[]),e(S,D,' ')).

adapt_graph_node(n(N,X/Y), n(N,Per)) :-
        Per is X / Y * 100.