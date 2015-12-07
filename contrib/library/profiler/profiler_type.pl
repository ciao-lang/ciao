:- module(_, _, [assertions, regtypes]).

:- regtype profile_info_name/1.
profile_info_name(frequency).
profile_info_name(flat_info).
profile_info_name(cc_info).

:- regtype profile_info_type/1 # "Term containing the information
   collected by the profiler".
profile_info_type(p(ClockFrequency, FlatInfo, CCInfo)) :-
	nnegint(ClockFrequency), %  CPU clock frequency
	flat_info_type(FlatInfo), % Cost centers information
	ecc_info_type(CCInfo). %    Edges (between 2 cost centers) info

:- regtype flat_info_name/1.
flat_info_name(info_list).
flat_info_name(info_total).
flat_info_name(overhead_list).
flat_info_name(overhead_total).
flat_info_name(overhead_summary).

:- regtype flat_info_type/1.
flat_info_type(fr(FlatInfoList, FlatInfoTotal,
		FlatOverheadList, FlatOverheadTotal, FlatOverheadSummary)) :-
	list(FlatInfoList, info_pred_type), %     Predicates info
	info_total_type(FlatInfoTotal), %         Total predicates info
	list(FlatOverheadList, info_pred_type), % Overheads info
	info_total_type(FlatOverheadTotal), %     Total overheads info
	flat_overhead_type(FlatOverheadSummary). % Overhead summary

:- regtype flat_overhead_name/1.
flat_overhead_name(hooks_code).
flat_overhead_name(instr_code).
flat_overhead_name(t_overhead).
flat_overhead_name(prog_nohooks).
flat_overhead_name(prog_hoohs).
flat_overhead_name(prog_total).

:- regtype flat_overhead_type/1.
flat_overhead_type(oh(HooksCode, InstrCode, TOverhead, ProgNoHooks, ProgHooks,
		ProgTotal)) :-
	nnegint(HooksCode), %   Executing wam profiler hooks
	nnegint(InstrCode), %   Executing instrumentation code
	nnegint(TOverhead), %   Total overhead
	nnegint(ProgNoHooks), % Executing code with WAM hooks turned off
	nnegint(ProgHooks), %   Executing code with WAM hooks turned on
	nnegint(ProgTotal). %   Total Time

:- regtype pred_type/1 # "Type of predicate.".
pred_type(compac).
pred_type(compid).
pred_type(emul).
pred_type(emulid).
pred_type(fast).
pred_type(fastid).
pred_type(undef).
pred_type(c).
pred_type(interp).
pred_type(buabor).
pred_type(butapp).
pred_type(bucall).
pred_type(buscll).
pred_type(bundcl).
pred_type(butrue).
pred_type(bufail).
pred_type(bucins).
pred_type(burest).
pred_type(bucomp).
pred_type(bugele).
pred_type(buinst).
pred_type(builtd).
pred_type(other).
pred_type(remain).

:- regtype info_pred_name/1.

info_pred_name(pred_name).
info_pred_name(pred_type).
info_pred_name(info_item).

:- regtype info_pred_type/1 # "Contain profiling info of a predicate.".
info_pred_type(p(FunctorDesc, PredType, InfoItemType)) :-
	predname(FunctorDesc), % Predicate name 
	pred_type(PredType), %            Type of predicate
%                                         (could be an overhead element)
	info_item_type(InfoItemType).

:- regtype info_item_type/1.
info_item_type(i(Skips, CallExitsC, CallExitsT, CallFailsC, CallFailsT,
		RedoExitsC, RedoExitsT, RedoFailsC, RedoFailsT, Counts,
		Ticks)) :-
	nnegint(Skips), %      Number of nodes removed with the cut
	nnegint(CallExitsC), % Counts of call-exit executions
	nnegint(CallExitsT), % Ticks of call-exit executions
	nnegint(CallFailsC), % Counts of call-fail executions
	nnegint(CallFailsT), % Ticks of call-fail executions
	nnegint(RedoExitsC), % Counts of redo-exit executions
	nnegint(RedoExitsT), % Ticks of redo-exit executions
	nnegint(RedoFailsC), % Counts of redo-fail executions
	nnegint(RedoFailsT), % Ticks of redo-fail executions
	nnegint(Counts), %     Counts of executions
	nnegint(Ticks). %      Ticks of executions

:- regtype info_item_value/1.
info_item_value(Value) :-
	num(Value).

:- regtype info_item_name/1.
info_item_name(call_exits_c).
info_item_name(call_exits_t).
info_item_name(call_fails_c).
info_item_name(call_fails_t).
info_item_name(redo_exits_c).
info_item_name(redo_exits_t).
info_item_name(redo_fails_c).
info_item_name(redo_fails_t).
info_item_name(counts).
info_item_name(ticks).
info_item_name(skips).
info_item_name(nskips).
info_item_name(cuts).
info_item_name(scuts).
info_item_name(time(Freq)) :- nnegint(Freq).

:- regtype info_total_name/1.
info_total_name(num_preds).
info_total_name(info_item).

:- regtype info_total_type/1.
info_total_type(t(NumPreds, InfoItemType)) :-
	nnegint(NumPreds), % Number of predicates of the set
	info_item_type(InfoItemType).

:- regtype profile_info_edge_type/1 # "Contain profiling info of a
   predicate when it is called from other.".
profile_info_edge_type(e(FunctorDescD, FunctorDescS, HooksEnabled,
		Cuts, SCuts, InfoItemType)) :-
	predname(FunctorDescD), % Callee predicate name 
%                                          (could be an overhead element)
	predname(FunctorDescS), % Caller predicate name 
%                                          (could be an overhead element)
	hooks_enabled_type(HooksEnabled),
	nnegint(Cuts),
	nnegint(SCuts),
	info_item_type(InfoItemType).

:- regtype ecc_info_name/1.
ecc_info_name(ecc_list).
ecc_info_name(ecc_summary).

:- regtype ecc_info_type/1.
ecc_info_type(cr(CCList, CCSummary)) :-
	list(CCList, ecc_item_type),
	cc_summary_type(CCSummary).

:- regtype cc_total_name/1.
cc_total_name(called_pred).
cc_total_name(total_called_pred).
cc_total_name(called_hooks).
cc_total_name(total_called_hooks).

:- regtype cc_total_type/1.
cc_total_type(d(CostCenters, TotalCC, Hooks, TotalH)) :-
	list(CostCenters, info_pred_type), % Called predicates info
	info_total_type(TotalCC), %          Total called predicates 
%                                            info
	list(Hooks, info_pred_type), %       Called hooks info
	info_total_type(TotalH). %           Total called hooks info

:- regtype hooks_enabled_type/1.
hooks_enabled_type(0).
hooks_enabled_type(1).

:- regtype ecc_item_name/1.
ecc_item_name(functor_d).
ecc_item_name(functor_s).
ecc_item_name(hooks_enabled).
ecc_item_name(info_item).
ecc_item_name(total_edge_info).

:- regtype ecc_item_type/1.
ecc_item_type(c(FunctorDescD, FunctorDescS, HooksEnabled, Cuts, SCuts,
		InfoItem, TotalEdgeInfo)) :-
	predname(FunctorDescD), %                Callee predicate name
	predname(FunctorDescS), %                Caller predicate name
	hooks_enabled_type(HooksEnabled), %      Hooks enabled
%       The rest of the elements refer to the calls caller -> callee
	nnegint(Cuts),
	nnegint(SCuts),
	info_item_type(InfoItem), %      Info Item
	cc_total_type(TotalEdgeInfo). /* Predicates called during 
                                                 the execution of the callee */

:- regtype cc_summary_name/1.
cc_summary_name(info_item).
cc_summary_name(info_list).
cc_summary_name(total).

:- regtype cc_summary_type/1.
cc_summary_type(tc(Cuts, SCuts, InfoItem, InfoList, Total)) :-
	nnegint(Cuts), %       Number of effective cuts done in a node scope
	nnegint(SCuts), %      Cuts without effect done in a node scope
	info_item_type(InfoItem),
	list(InfoList, profile_info_edge_type), % Full edges (between two cost
%                                                 centers) info -for the graph-
	info_total_type(Total). %                 Total edges (between two cost 
%                                                 centers) info -as a summary-
