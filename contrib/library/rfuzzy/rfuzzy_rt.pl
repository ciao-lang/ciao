:- module(rfuzzy_rt, [
	% Rfuzzy Computations.
	rfuzzy_compute_defined_operators/1,
	rfuzzy_compute_aux/5,	
	% Aggregators.
	rfuzzy_defined_aggregators/1, 
	inject/3, merge/4, prod/3, iprod/3, mean/3, 
	min/3, luka/3, dprod/3, max/3, dluka/3, complement/3,
	% Quantifiers.
	rfuzzy_defined_quantifiers/1,
	% Auxiliar predicates.
	print_msg/3, print_msg_nl/1, activate_rfuzzy_debug/0,
	rfuzzy_conversion_in/2, rfuzzy_conversion_out/2,
	supreme/2, reorder_by_truth_value/3, 
	one_by_one_first_head/2, one_by_one_first_tail/2,
	rfuzzy_process_attribute_dump/4,
	append_local/3, memberchk_local/2, remove_list_dupplicates/3,
	sets_union/3, lists_substraction/3
		     ],[hiord]).

:- use_module(library(write),[write/1]).
:- use_package(clpr).
:- use_module(library(terms),[copy_args/3]).

% ---------------------------------------------------------------------------------------------------
% ---------------------------------------------------------------------------------------------------
% ---------------------------------------------------------------------------------------------------

% REMOVED: preinject/3,postinject/4, id/2, id/3, id (in defined_aggregators), 

rfuzzy_defined_aggregators([min, max, prod, iprod, dprod, luka, dluka, complement, mean]).

min(X,Y,Z):- X .=<. Y, X .=. Z .
min(X,Y,Z):- X .>. Y, Y .=. Z .

max(X,Y,Z):- X .>=. Y, X .=. Z .
max(X,Y,Z):- Y .>. X, Y .=. Z .

prod(X,Y,M):- M .=. X * Y.
iprod(X,Y,M):- M .=. 1 - (X * Y).
dprod(X,Y,M):- M .=. X + Y - (X * Y).

luka(X,Y,M):- 
	Temp .=. X + Y  - 1, 
	max(0, Temp, M).

dluka(X,Y,M):- 
	Temp .=. X + Y,
	min(1, Temp, M).

complement(X, C, Z) :-
	Temp1 .=. C - X,
	min(1, Temp1, Temp2),
	max(0, Temp2, Z).

mean(X, Y, Z) :- Z .=. (X + Y) / 2.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

supreme([], _Element) :- !, 
	print_msg('debug', 'supreme', 'list is empty. FAIL.'),
	!, fail.
supreme(List_In, Answer) :-
	functor(Answer, _Name, Arity), 
	Arguments_Arity is Arity -1,
	Truth_Value_Arity is Arity +1,
	
	print_msg('debug', 'supreme :: List_In', List_In),
	supreme_aux_1(List_In, List_Aux_1), !,
	print_msg('debug', 'supreme :: List_Aux_1', List_Aux_1),
	reorder_by_truth_value(List_Aux_1, [], List_Aux_2),
	print_msg('debug', 'supreme :: List_Aux_2', List_Aux_2),
	one_by_one_first_head(List_Aux_2, Element),
	print_msg('debug', 'supreme :: element_taken', Element),

	copy_args(Arguments_Arity, Element, Answer),
	arg(Truth_Value_Arity, Element, Truth_Value),
	arg(Arity, Answer, Truth_Value).
	

% Head is only kept if it is the supreme.
supreme_aux_1([], []) :- !.
supreme_aux_1([Head | Tail_In], [Head | List_Out]) :- 
	print_msg_nl('ultradebug'),
	print_msg('ultradebug', 'supreme_aux_1 :: Head', Head),
	supreme_aux_2(Head, Tail_In, Tail_Out), !,
	supreme_aux_1(Tail_Out, List_Out).
supreme_aux_1([_Head | Tail_In], List_Out) :-
	supreme_aux_1(Tail_In, List_Out).

supreme_aux_2(_Head, [], []) :- !.
supreme_aux_2(Head, [Next | Tail_In], Tail_Out) :-
	split_out_fuzzy_functor_args(Head, Prio_1, TV_1, Args_1),
	split_out_fuzzy_functor_args(Next, Prio_2, TV_2, Args_2),

	Args_1 = Args_2, !, % They are for the same fuzzy values.
	print_msg('ultradebug', 'supreme_aux_2', 'equal args'),
	(
	    (	Prio_1 .>. Prio_2  )
	;
	    (   Prio_1 .=. Prio_2, TV_1 .>=. TV_2  )
	), !,
	print_msg('ultradebug', 'supreme_aux_2', 'higher Prio or TV.'),
	supreme_aux_2(Head, Tail_In, Tail_Out).
supreme_aux_2(Head, [Next | Tail_In], [Next | Tail_Out]) :-
	supreme_aux_2(Head, Tail_In, Tail_Out).

reorder_by_truth_value([], List_In, List_In) :- !.
reorder_by_truth_value([Head_1 | Tail], List_In, List_Out) :-
	reorder_by_truth_value_aux(Head_1, List_In, List_Aux), !,
	reorder_by_truth_value(Tail, List_Aux, List_Out).
	
reorder_by_truth_value_aux(Head_1, [], [Head_1]) :- !.
reorder_by_truth_value_aux(Head_1, [ Head_2 | Tail_In ], [ Head_2 | Tail_Out ]) :-
	has_less_truth_value(Head_1, Head_2), !,
	reorder_by_truth_value_aux(Head_1, Tail_In, Tail_Out).
reorder_by_truth_value_aux(Head_1, [ Head_2 | Tail_In ], [ Head_1, Head_2 | Tail_In ]) :- !.

has_less_truth_value(Head_1, Head_2) :-
	print_msg('debug', 'has_less_truth_value(Head_1, Head_2)', (Head_1, Head_2)),
	functor(Head_1, _Name_1, Arity_1), 
	functor(Head_2, _Name_2, Arity_2), 
	arg(Arity_1, Head_1, TV_1),
	arg(Arity_2, Head_2, TV_2),
	has_less_truth_value_aux(TV_1, TV_2).

has_less_truth_value_aux(TV_1, TV_2) :-
%	var(TV_1), var(TV_2),
	print_msg('debug', 'has_less_truth_value_aux(TV_1, TV_2)', (TV_1, TV_2)),
	TV_1 .<. TV_2, !,
	print_msg('debug', 'has_less_truth_value_aux(TV_1, TV_2)', 'yes'), !.

has_less_truth_value_aux(TV_1, TV_2) :-
%	var(TV_1), var(TV_2),
	TV_1 .>=. TV_2, !,
	print_msg('debug', 'has_less_truth_value_aux(TV_1, TV_2)', 'no'),
	!, fail.

has_less_truth_value_aux(TV_1, TV_2) :-
	print_msg('error', 'has_less_truth_value_aux(TV_1, TV_2)', (TV_1, TV_2)),
	(   (   nonvar(TV_1), print_msg('error', 'Dump_TV_1', 'nonvar'))
	;
	    (   dump_constraints(TV_1, TV_1, Dump_TV_1),
		print_msg('error', 'Dump_TV_1', (Dump_TV_1)))
	),
	(   (   nonvar(TV_2), print_msg('error', 'Dump_TV_2', 'nonvar'))
	;
	    (   dump_constraints(TV_2, TV_2, Dump_TV_2),
		print_msg('error', 'Dump_TV_2', (Dump_TV_2)))
	), !, fail.

split_out_fuzzy_functor_args(Head, Prio, TV, Other_Args) :-
%	print_msg('debug', 'split_out_fuzzy_functor_args(Head)', split_out_fuzzy_functor_args(Head)),
	copy_term(Head, Head_Copy),
	functor(Head_Copy, Name, _Arity), 
	Head_Copy=..[Name | Functor_Args],
	split_out_fuzzy_functor_args_aux(Functor_Args, Prio, TV, Other_Args), 
	print_msg('ultradebug', 'split_out_fuzzy_functor_args(Head, Prio, TV, Args)', split_out_fuzzy_functor_args(Head, Prio, TV, Other_Args)).

split_out_fuzzy_functor_args_aux([Prio, TV], Prio, TV, []) :- !.
split_out_fuzzy_functor_args_aux([Arg | Args_List_In], Prio, TV, [Arg | Args_List_Out]) :- 
	split_out_fuzzy_functor_args_aux(Args_List_In, Prio, TV, Args_List_Out).

one_by_one_first_head([Element|_List], Element).
one_by_one_first_head([_FirstElement|List], Element) :-
	one_by_one_first_head(List, Element).

one_by_one_first_tail([Element], Element) :- !.
one_by_one_first_tail([_FirstElement|List], Element) :-
	one_by_one_first_tail(List, Element).
one_by_one_first_tail([Element|_List], Element) :- !.

% ---------------------------------------------------------------------------------------------------
% ---------------------------------------------------------------------------------------------------
% ---------------------------------------------------------------------------------------------------
 
%:- meta_predicate preinject(?,pred(2),?).
%
%id(L,L).
%
%preinject([],_,[]):-!.
%preinject(L,P,T):- P(L,T).

:- meta_predicate inject(?,pred(3),?).

inject([],_,_).
inject([T],_,T).
inject([X,Y|Rest],P,T):-
	P(X,Y,T0),
	inject([T0|Rest],P,T).

%:- meta_predicate postinject(?,?,pred(3),?).
%
%id(_,V,V).
%postinject([],A,_,A):-!.
%postinject(L,V,P,T):- P(L,V,T).


:- meta_predicate merge(?,?,pred(3),?).

merge([],L,_,L).

merge(L,[],_,L).

merge(L1,L2,P,L):-
	list(L1),list(L2),!,
	mergeaux(L1,L2,P,L).

mergeaux([],[],_,[]).

mergeaux([X|L1],[Y|L2],P,[Z|L]):-
	P(X,Y,Z),
	mergeaux(L1,L2,P,L).

:- new_declaration(is_fuzzy/3,on).
:- is_fuzzy('=>',4,truth).

:- meta_predicate =>(pred(3),goal,goal,?).

%=>(Formula,X,Y,M):- 
%	functor(X,_,Ax),
%	arg(Ax,X,Mx),
%	functor(Y,_,Ay),
%	arg(Ay,Y,My),
%	call(X),
%	call(Y),
%	call(Formula,Mx,My,M).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

rfuzzy_conversion_in(X, Y) :-
	nonvar(X),
	X .=. Y.
rfuzzy_conversion_in(X, _Y) :-
	\+(nonvar(X)).

rfuzzy_conversion_out(rat(X, Y), (X/Y)) :- !.
rfuzzy_conversion_out(X, X) :-
	number(X).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

rfuzzy_process_attribute_dump([Dump], Var, Condition, Value) :-
	functor(Dump, Condition, Arity),
	Arity = 2,
	arg(1, Dump, Var), 
	arg(2, Dump, Value), 
	!. 

rfuzzy_process_attribute_dump(Dump, _Var, _Condition, _Value) :-
	print_msg('error', 'rfuzzy_process_attribute_dump :: Dump', Dump),
	!, fail.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

rfuzzy_compute_defined_operators([('=~=', 'rfuzzy_enum_type'), ('=', 'rfuzzy_any_type'), ('=/=', 'rfuzzy_any_type'), ('>', 'rfuzzy_number_type'), ('<', 'rfuzzy_number_type'), ('>=', 'rfuzzy_number_type'), ('=<', 'rfuzzy_number_type')]).
rfuzzy_compute_aux(Operator, _Elt1, _Elt2, _Computed_Similarities, _Truth_Value) :-
	var(Operator), !, fail.
rfuzzy_compute_aux(Operator, Elt1, Elt2, _Computed_Similarities, Truth_Value) :-
	Operator = '=', !,
	(
	    (	Elt1 .=. Elt2, !,	Truth_Value .=. 1    )
	;
	    (	Truth_Value .=. 0    )
	).

rfuzzy_compute_aux(Operator, Elt1, Elt2, _Computed_Similarities, Truth_Value) :-
	Operator = '=/=', !,
	(
	    (	Elt1 .=. Elt2, !,	Truth_Value .=. 0    )
	;
	    (	Truth_Value .=. 1    )
	).
rfuzzy_compute_aux(Operator, Elt1, Elt2, _Computed_Similarities, Truth_Value) :-
	Operator = '>', !,
	(
	    (	Elt1 .>. Elt2, !,	Truth_Value .=. 1    )
	;
	    (	Truth_Value .=. 0    )
	).
rfuzzy_compute_aux(Operator, Elt1, Elt2, _Computed_Similarities, Truth_Value) :-
	Operator = '<', !,
	(
	    (	Elt1 .<. Elt2, !,	Truth_Value .=. 1    )
	;
	    (	Truth_Value .=. 0    )
	).
rfuzzy_compute_aux(Operator, Elt1, Elt2, _Computed_Similarities, Truth_Value) :-
	Operator = '>=', !,
	(
	    (	Elt1 .>=. Elt2, !,	Truth_Value .=. 1    )
	;
	    (	Truth_Value .=. 0    )
	).
rfuzzy_compute_aux(Operator, Elt1, Elt2, _Computed_Similarities, Truth_Value) :-
	Operator = '=<', !,
	(
	    (	Elt1 .=<. Elt2, !,	Truth_Value .=. 1    )
	;
	    (	Truth_Value .=. 0    )
	).
rfuzzy_compute_aux(Operator, Elt1, Elt2, Computed_Similarities, Truth_Value) :-
	Operator = '=~=', !,
	Format = rfuzzy_computed_similarity_between(Elt1, Elt2, TV, Cred_Op, Cred),
	(
	    (
		memberchk_local(Format, Computed_Similarities), !,
		functor(Cred_Functor, Cred_Op, 3),
		arg(1, Cred_Functor, TV), 
		arg(2, Cred_Functor, Cred), 
		arg(3, Cred_Functor, Truth_Value),
		call(Cred_Functor)
	    )
	;
	    (
		Truth_Value .=. 0
	    )
	).

rfuzzy_compute_aux(Operator, _Elt1, _Elt2, _Computed_Similarities, 0) :- !,
	print_msg('error', 'rfuzzy_compute_aux :: Unknown operator', Operator), !, fail.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% rfuzzy_defined_quantifiers([]).
rfuzzy_defined_quantifiers([(very, 2, TV_In, TV_Out, (TV_Out .=. (TV_In * TV_In)))]).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

sets_union([], Set2, Set2) :- !.
sets_union([Member1 | Set1], Set2, Set3) :-
	memberchk_local(Member1, Set2), !,
	sets_union(Set1, Set2, Set3).
sets_union([Member1 | Set1], Set2, [Member1 | Set3]) :-
	sets_union(Set1, Set2, Set3).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

lists_substraction([], _List_2, []) :- !.
lists_substraction([Head | Tail ], List_2, Result_List) :-
	memberchk_local(Head, List_2), !, 
	lists_substraction(Tail, List_2, Result_List).
lists_substraction([Head | Tail ], List_2, [Head | Result_List]) :-
	lists_substraction(Tail, List_2, Result_List).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

append_local([], N2, N2).
append_local([Elto|N1], N2, [Elto|Res]) :-
	append_local(N1, N2, Res).

memberchk_local(Element, [Element | _Tail]) :- !.
memberchk_local(Element, [_Head | Tail]) :- !,
	memberchk_local(Element, Tail).

% remove_list_dupplicates(List_In, List_Aux, List_Out)
remove_list_dupplicates([], List_Aux, List_Aux) :- !.
remove_list_dupplicates([Element|List_In], List_Aux, List_Out) :-
	memberchk_local(Element, List_Aux), !,
	remove_list_dupplicates(List_In, List_Aux, List_Out).
remove_list_dupplicates([Element|List_In], List_Aux, List_Out) :-
	remove_list_dupplicates(List_In, [Element|List_Aux], List_Out).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

:- data print_msg_level/1.

activate_all_rfuzzy_print_msg_level :-
	assertz_fact(print_msg_level('info')), % An intermediate level
	assertz_fact(print_msg_level('warning')), % The level printing less	
	assertz_fact(print_msg_level('error')), % The level printing less
	assertz_fact(print_msg_level('configured')). % The level printing less

% This is to enable debug. Deactivated by default.
activate_rfuzzy_debug :-	
	assertz_fact(print_msg_level('debug')). % The lowest level

% Main predicate in charge of printing.
print_msg(Level, Msg1, Msg2) :- 
	\+(print_msg_level('configured')), !,
	activate_all_rfuzzy_print_msg_level,
	print_msg(Level, Msg1, Msg2).
print_msg(Level, Msg1, Msg2) :- 
	print_msg_level('configured'),
	print_msg_level(Level), !,
	translate_level_to_pre_msg1(Level, Pre_Msg1),
	print_msg_aux(Pre_Msg1, Msg1, [], Msg2),
	print_msg_nl(Level).
print_msg(_Level, _Msg1, _Msg2) :- 
	print_msg_level('configured'), !. 

translate_level_to_pre_msg1('debug', 'DEBUG: ') :- !.
translate_level_to_pre_msg1('info', 'INFO: ') :- !.
translate_level_to_pre_msg1('warning', 'WARNING: ') :- !.
translate_level_to_pre_msg1('error', 'ERROR: ') :- !.
translate_level_to_pre_msg1('', '') :- !.

% This gets rid of lists. Be careful with variables !!!
print_msg_aux(Pre_Msg1, Msg1, Msg1_Info, Var) :- 
	var(Var), !,
	print_msg_real(Pre_Msg1, Msg1, Msg1_Info, Var).
print_msg_aux(Pre_Msg1, Msg1, Msg1_Info, []) :- !,
	print_msg_real(Pre_Msg1, Msg1, [ ' (list)' | Msg1_Info ], ' (empty)').
print_msg_aux(Pre_Msg1, Msg1, Msg1_Info, [ Msg2_Head | Msg2_Tail ]) :- !,
	print_msg_aux(Pre_Msg1, Msg1, [ ' (list)' | Msg1_Info ], Msg2_Head),
	print_msg_nl('error'), % Print it always.
	print_msg_aux(Pre_Msg1, Msg1, Msg1_Info, Msg2_Tail).
print_msg_aux(Pre_Msg1, Msg1, Msg1_Info, Msg2) :- !,
	print_msg_real(Pre_Msg1, Msg1, Msg1_Info, Msg2).

% Predicate that really prints.
print_msg_real(Pre_Msg1, Msg1,  Msg1_Info, Msg2) :-
	write(Pre_Msg1), 
	write(Msg1), 
	print_msg1_info(Msg1_Info),
	write(':  '),  write(Msg2),
	write('    ').

% Print msg1 Info (in reverse order to show the structure).
print_msg1_info([]) :- !.
print_msg1_info([Head | Tail]) :- !,
	print_msg1_info(Tail),
	write(' '),
	write(Head).

print_msg_nl(Level) :- print_msg_level(Level), !, nl.
print_msg_nl(_Level) :- !.

