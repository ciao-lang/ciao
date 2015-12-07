:- module(_, [
		get_cc_item/5,
		get_cc_summary/3,
		get_ecc_info/3,
		get_ecc_item/3,
		get_flat_info/3,
		get_flat_overhead/3,
		get_info_item/3,
		get_info_pred/3,
		get_info_pred_from_list/4,
		get_info_total/3,
		get_profile_cc_data/5,
		get_profile_cc_data_item/5,
		get_profile_cc_graph/4,
		get_profile_cc_graph_time/2,
		get_profile_cc_called_preds/4,
		get_profile_cc_called_ccs/4,
		get_profile_cc_summary_data/4,
		get_profile_cc_summary_data_time/3,
		get_profile_cc_summary_total/3,
		get_profile_cc_summary_total_cc_item/3,
		get_profile_cc_summary_total_total/3,
		get_profile_cc_summary_total_total_item/3,
		get_profile_ecc_summary_data/5,
		get_profile_flat_data/4,
		get_profile_flat_total/3,
		get_profile_flat_total_item/3,
		get_profile_info/3,
		get_profile_total_time/2,
		obtain_edges/3,
		obtain_nodes/3
	    ], [assertions, regtypes, dcg]).

% :- max_length_line(80).

:- include(library(profiler/profiler_base_inline)).

:- use_module(library(hiordlib), [map/3, map/4]).
:- use_module(library(profiler/profiler_type)).
:- use_module(library(lists)).

:- doc(author, "Edison Mera").
:- doc(author, "Teresa Trigo").

:- doc(module, "This module contain implementation of predicates
   that help to access the information returned by the profiler.").

:- doc(bug, "Predicate get_profile_cc_summary_ccs can be optimized by
           implementing this without usage of obtain_nodes and map to avoid 
           computation of Counts--MTT").


% ----------------------------------------------------------------------------
% Useful predicates
% ----------------------------------------------------------------------------

:- pred get_profile_info/3 :: profile_info_name * profile_info_type * term.
get_profile_info(frequency, p(V, _, _), V).
get_profile_info(flat_info, p(_, V, _), V).
get_profile_info(cc_info,   p(_, _, V), V).

:- pred get_flat_info/3 :: flat_info_name * flat_info_type * term.
get_flat_info(info_list,        fr(V, _, _, _, _), V).
get_flat_info(info_total,       fr(_, V, _, _, _), V).
get_flat_info(overhead_list,    fr(_, _, V, _, _), V).
get_flat_info(overhead_total,   fr(_, _, _, V, _), V).
get_flat_info(overhead_summary, fr(_, _, _, _, V), V).

:- pred get_info_pred_from_list/4 :: predname * list(info_item_type) *
	pred_type * info_item_type.
get_info_pred_from_list(FunctorDesc, InfoItemList, PredType, InfoItem) :-
	member(p(FunctorDesc, PredType, InfoItem), InfoItemList).

:- pred get_info_item/3 :: info_item_name * info_item_type * num.
get_info_item(skips,        i(V, _, _, _, _, _, _, _, _, _, _), V).
get_info_item(call_exits_c, i(_, V, _, _, _, _, _, _, _, _, _), V).
get_info_item(call_exits_t, i(_, _, V, _, _, _, _, _, _, _, _), V).
get_info_item(call_fails_c, i(_, _, _, V, _, _, _, _, _, _, _), V).
get_info_item(call_fails_t, i(_, _, _, _, V, _, _, _, _, _, _), V).
get_info_item(redo_exits_c, i(_, _, _, _, _, V, _, _, _, _, _), V).
get_info_item(redo_exits_t, i(_, _, _, _, _, _, V, _, _, _, _), V).
get_info_item(redo_fails_c, i(_, _, _, _, _, _, _, V, _, _, _), V).
get_info_item(redo_fails_t, i(_, _, _, _, _, _, _, _, V, _, _), V).
get_info_item(counts,       i(_, _, _, _, _, _, _, _, _, V, _), V).
get_info_item(ticks,        i(_, _, _, _, _, _, _, _, _, _, V), V).
get_info_item(time(Freq),   i(_, _, _, _, _, _, _, _, _, _, T), V) :-
	V is (T * 1000) / Freq.

:- pred get_info_total/3 :: info_total_name * info_total_type * term.
get_info_total(num_preds, t(V, _), V).
get_info_total(info_item, t(_, V), V).

:- pred get_ecc_info/3 :: ecc_info_name * ecc_info_type * term.
get_ecc_info(ecc_list,    cr(V, _), V).
get_ecc_info(ecc_summary, cr(_, V), V).

:- pred get_cc_summary/3 :: cc_summary_name * cc_summary_type * term.
get_cc_summary(cuts,      tc(V, _, _, _, _), V).
get_cc_summary(scuts,     tc(_, V, _, _, _), V).
get_cc_summary(info_item, tc(_, _, V, _, _), V).
get_cc_summary(info_list, tc(_, _, _, V, _), V).
get_cc_summary(total,     tc(_, _, _, _, V), V).

:- pred get_ecc_item/3 :: ecc_item_name * ecc_item_type * term.
get_ecc_item(functor_d,       c(V, _, _, _, _, _, _), V).
get_ecc_item(functor_s,       c(_, V, _, _, _, _, _), V).
get_ecc_item(hooks_enabled,   c(_, _, V, _, _, _, _), V).
get_ecc_item(cuts,            c(_, _, _, V, _, _, _), V).
get_ecc_item(scuts,           c(_, _, _, _, V, _, _), V).
get_ecc_item(info_item,       c(_, _, _, _, _, V, _), V).
get_ecc_item(total_edge_info, c(_, _, _, _, _, _, V), V).

:- pred get_flat_overhead/3 :: flat_overhead_name * flat_overhead_type *
	nnegint.
get_flat_overhead(hooks_code,   oh(V, _, _, _, _, _), V).
get_flat_overhead(instr_code,   oh(_, V, _, _, _, _), V).
get_flat_overhead(t_overhead,   oh(_, _, V, _, _, _), V).
get_flat_overhead(prog_nohooks, oh(_, _, _, V, _, _), V).
get_flat_overhead(prog_hoohs,   oh(_, _, _, _, V, _), V).
get_flat_overhead(prog_total,   oh(_, _, _, _, _, V), V).

:- pred get_cc_total/3 :: cc_total_name * cc_total_type * term.
get_cc_total(called_pred,        d(V, _, _, _), V).
get_cc_total(total_called_pred,  d(_, V, _, _), V).
get_cc_total(called_hooks,       d(_, _, V, _), V).
get_cc_total(total_called_hooks, d(_, _, _, V), V).

:- pred get_info_pred/3 :: info_pred_name * info_pred_type * term.
get_info_pred(pred_name, p(V, _, _), V).
get_info_pred(pred_type, p(_, V, _), V).
get_info_pred(info_item, p(_, _, V), V).

:- pred get_profile_flat_data/4 :: predname * info_item_name *
	profile_info_type * info_item_value.
get_profile_flat_data(FunctorDesc, Field, ProfileInfo, Value) :-
	get_profile_info(flat_info, ProfileInfo, FlatInfo),
	get_flat_info(info_list, FlatInfo, FlatInfoList),
	get_info_pred_from_list(FunctorDesc, FlatInfoList, _, FlatInfoItem),
	get_info_item(Field, FlatInfoItem, Value).

:- pred get_profile_flat_total/3 :: info_total_name * profile_info_type *
	info_total_type.
get_profile_flat_total(Field, ProfileInfo, Value) :-
	get_profile_info(flat_info, ProfileInfo, FlatInfo),
	get_flat_info(info_total, FlatInfo, InfoTotal),
	get_info_total(Field, InfoTotal, Value).

:- pred get_profile_flat_total_item/3 :: info_item_name * profile_info_type *
	info_item_value.
get_profile_flat_total_item(Field, ProfileInfo, Value) :-
	get_profile_flat_total(info_item, ProfileInfo, InfoItem),
	get_info_item(Field, InfoItem, Value).

:- pred get_profile_cc_data/5 :: ecc_item_name * predname * predname *
	profile_info_type * term.
get_profile_cc_data(Field, FunctorDescD, FunctorDescS, ProfileInfo, Value) :-
	get_profile_info(cc_info, ProfileInfo, CCInfo),
	get_ecc_info(ecc_list, CCInfo, CCList),
	get_cc_item_from_list(FunctorDescD, FunctorDescS, CCList, CCItem),
	get_ecc_item(Field, CCItem, Value).

% D means Destiny and S means Source (of the edge of the graph)
get_cc_item_from_list(CCFunctorDescD, CCFunctorDescS, CCItemList, CCItem) :-
	CCItem = c(CCFunctorDescD, CCFunctorDescS, _, _, _),
	member(CCItem, CCItemList).

:- pred get_profile_cc_called_preds/4 :: predname * predname *
	profile_info_type * term.
get_profile_cc_called_preds(FunctorDescD, FunctorDescS, ProfileInfo, Value) :-
	get_profile_cc_data(total_edge_info, FunctorDescD, FunctorDescS,
	    ProfileInfo, TotalInfo),
	get_cc_total(called_pred,  TotalInfo, TotalCalledPreds),
	get_cc_total(called_hooks, TotalInfo, TotalCalledHooks),
	map(TotalCalledPreds, get_functor_desc_of_item, ValuePreds),
	map(TotalCalledHooks, get_functor_desc_of_item, ValueHooks),
	append(ValuePreds, ValueHooks, Value).

get_functor_desc_of_item(Item, FunctorDesc) :-
	get_info_pred(pred_name, Item, FunctorDesc).

:- pred get_profile_cc_data_item/5 :: info_item_name * predname * predname *
	profile_info_type * info_item_value.
get_profile_cc_data_item(Field, FunctorDescD, FunctorDescS, ProfileInfo,
	    Value) :-
	get_profile_cc_data(info_item, FunctorDescD, FunctorDescS, ProfileInfo,
	    InfoItem),
	get_info_item(Field, InfoItem, Value).

:- pred get_profile_cc_summary_total/3 :: cc_summary_name * cc_summary_type *
	term.
get_profile_cc_summary_total(Field, ProfileInfo, Value) :-
	get_profile_info(cc_info, ProfileInfo, CCInfo),
	get_ecc_info(ecc_summary, CCInfo, CCSummary),
	get_cc_summary(Field, CCSummary, Value).

:- pred get_info_edge_from_list/7 :: predname * predname * list(info_item_type)
	* hooks_enabled_type * nnegint * nnegint * info_item_type.
get_info_edge_from_list(FunctorDescD, FunctorDescS, InfoItemList, HooksEnabled,
	    Cuts, SCuts, InfoItem) :-
	member(e(FunctorDescD, FunctorDescS, HooksEnabled, Cuts, SCuts,
		InfoItem), InfoItemList).

:- pred get_profile_ecc_summary_data/5 :: info_item_name * predname * predname
	* profile_info_type * info_item_value.
get_profile_ecc_summary_data(Field, FunctorDescD, FunctorDescS, ProfileInfo,
	    Value) :-
	get_profile_cc_summary_total(info_list, ProfileInfo, CCInfoList),
	get_info_edge_from_list(FunctorDescD, FunctorDescS, CCInfoList, _,
	    _, _, CCInfoItem),
	get_info_item(Field, CCInfoItem, Value).

:- pred get_profile_cc_summary_total_cc_item/3 :: info_item_name *
	profile_info_type * info_item_value.
get_profile_cc_summary_total_cc_item(Field, ProfileInfo, Value) :-
	get_profile_cc_summary_total(info_item, ProfileInfo, InfoItem),
	get_info_item(Field, InfoItem, Value).

:- pred get_profile_cc_summary_total_total/3 :: info_total_name *
	profile_info_type * term.
get_profile_cc_summary_total_total(Field, ProfileInfo, Value) :-
	get_profile_cc_summary_total(total, ProfileInfo, Total),
	get_info_total(Field, Total, Value).

:- pred get_profile_cc_summary_total_total/3 :: info_item_name *
	profile_info_type * info_item_value.
get_profile_cc_summary_total_total_item(Field, ProfileInfo, Value) :-
	get_profile_cc_summary_total_total(info_item, ProfileInfo, InfoItem),
	get_info_item(Field, InfoItem, Value).

:- pred get_profile_total_time/2 :: profile_info_type * num.
get_profile_total_time(ProfileInfo, TotalTime) :-
	get_profile_info(frequency, ProfileInfo, ClockFreq),
	get_profile_cc_summary_total_total_item(time(ClockFreq), ProfileInfo,
	    TotalTime).

:- pred get_profile_cc_graph/4 :: info_item_name * list(info_item_name) *
	profile_info_type * term.
get_profile_cc_graph(NodeField, EdgeFields, ProfileInfo, Graph) :-
	get_profile_info(cc_info, ProfileInfo, CCInfo),
	get_ecc_info(ecc_list, CCInfo, InfoForEdges),
	obtain_edges(InfoForEdges, EdgeFields, Edges),
%       The size of a node can be measured in any info_item_name value
	obtain_nodes(InfoForEdges, NodeField, Nodes),
	Graph = g(Nodes, Edges).

:- pred get_profile_cc_called_ccs/4 :: predname * list(info_item_name) *
	profile_info_type * list.
get_profile_cc_called_ccs(FunctorDescS, EdgeFields, ProfileInfo, CalledCCs) :-
	get_profile_info(cc_info, ProfileInfo, CCInfo),
	get_ecc_info(ecc_list, CCInfo, InfoForEdges),
	obtain_edges(InfoForEdges, EdgeFields, Edges),
	map(Edges, filter_edge_by_source(FunctorDescS), CalledCCs, []).

filter_edge_by_source(e(S, D, T), S) --> !, [(D, T)].
filter_edge_by_source(_,          _) --> [].

:- pred get_profile_cc_graph_time/2 :: profile_info_type * term.
get_profile_cc_graph_time(ProfileInfo, Graph) :-
	get_profile_info(frequency, ProfileInfo, ClockFreq),
	get_profile_cc_graph(time(ClockFreq), [time(ClockFreq)], ProfileInfo,
	    Graph).

:- pred get_profile_cc_summary_data_time/3 :: predname * profile_info_type *
	num.
get_profile_cc_summary_data_time(FunctorDescD, ProfileInfo, Value) :-
	get_profile_info(frequency, ProfileInfo, ClockFreq),
	get_profile_cc_summary_data(time(ClockFreq), FunctorDescD, ProfileInfo,
	    Value).

:- pred get_profile_cc_summary_data/4 :: info_item_name * predname *
	profile_info_type * num.
get_profile_cc_summary_data(Field, FunctorDescD, ProfileInfo, Value) :-
	get_profile_info(cc_info, ProfileInfo, CCInfo),
	get_ecc_info(ecc_list, CCInfo, CCList),
	get_cc_item(CCList, Field, FunctorDescD, 0, Value).

:- pred get_cc_item/5 :: list(ecc_item_type) * info_item_name *
	predname * num * num
# "Given a cc descriptor and the profiler info it returns the
	execution time acumulated in such cost center".

get_cc_item([],                  _,     _,            N,  N).
get_cc_item([CCInfo|CCInfoList], Field, FunctorDescD, N0, N) :-
	get_ecc_item(functor_d, CCInfo, FunctorDescD),
	!,
	get_ecc_item(info_item, CCInfo, InfoItem),
	get_info_item(Field, InfoItem, Amount),
	N1 is Amount + N0,
	get_cc_item(CCInfoList, Field, FunctorDescD, N1, N).
get_cc_item([_CCInfo|CCInfoList], Field, FunctorDescD, N0, N) :-
	get_cc_item(CCInfoList, Field, FunctorDescD, N0, N).

% ----------------------------------------------------------------------------
% Auxiliar predicates for get_profile_cc_graph:
% ----------------------------------------------------------------------------

obtain_edge(InfoEdge, Fields, e(S, D, Values)) :-
	get_ecc_item(functor_d, InfoEdge, D),
	get_ecc_item(functor_s, InfoEdge, S),
	get_ecc_item(info_item, InfoEdge, InfoItem),
	map(Fields, get_info_item(InfoItem), Values).

obtain_edges(InfoEdges, Fields, Edges) :-
	map(InfoEdges, obtain_edge(Fields), Edges).

obtain_nodes([],                           _,     []).
obtain_nodes([InfoForFirstE|InfoForRestE], Field, [Node|Nodes]) :-
	get_ecc_item(functor_d, InfoForFirstE, D),
	remove_occurrencies([InfoForFirstE|InfoForRestE], D, InfoForRestE_NoD,
	    Field, 0, Value),
	Node = n(D, Value),
	obtain_nodes(InfoForRestE_NoD, Field, Nodes).

remove_occurrencies([],        _, [],        _,     Value,  Value).
remove_occurrencies([E|RestE], D, RestE_NoD, Field, Value0, Value) :-
	get_ecc_item(functor_d, E, D),
	!,
	get_ecc_item(info_item, E, I),
	get_info_item(Field, I, Value1),
	Value2 is Value0 + Value1,
	remove_occurrencies(RestE, D, RestE_NoD, Field, Value2, Value).
remove_occurrencies([E|RestE], D, [E|RestE_NoD], Field, Value0, Value) :-
	remove_occurrencies(RestE, D, RestE_NoD, Field, Value0, Value).
