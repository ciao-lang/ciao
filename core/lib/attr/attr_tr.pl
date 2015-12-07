:- module(attr_tr, [sentence/3], []).

:- use_module(library(messages), [warning_message/2]).

:- data found_hook/2.

reset(M):-
	retractall_fact(found_hook(M, _)).

add_hook(M, T):-
	current_fact(found_hook(M, T)), !.
add_hook(M, T):-
	assertz_fact(found_hook(M, T)), !.

sentence(0, _, Module):-
	reset(Module), 
	fail.

sentence(Clause, _, Module):-
	(Clause = (Head:-_) -> true ; Clause = Head),
	functor(Head, F, N), 
	(
%	    F=attr_unify_hook, N=2 ->
%	    add_hook(Module, attr_unify_hook)
%	;
	    F=attr_portray_hook, N=2 ->
	    add_hook(Module, attr_portray)
	;
	    F=attribute_goals, N=3 ->
	    add_hook(Module, attribute_goals)
        ;
	    F=dump_constraints, N=4 ->
	    add_hook(Module, dump_constraints)
	), 
	fail.
	    
sentence(end_of_file, Tail0, Module):-
	(
	    Tail0 = [
		% Deactivated until we find better solution.
		% (:- doc(hide, 'attr_rt:unify_hook'/3)), 	
		      (:- multifile 'attr_rt:unify_hook'/3), 
	              ('attr_rt:unify_hook'(Module, V, Other):- 
		          attr_unify_hook(V, Other)) |
                      Tail1]
	),
	(
	    retract_fact(found_hook(Module, attr_portray)) ->
	    Tail1 = [ 
		% Deactivated until we find better solution.
		% (:- doc(hide, 'attr_rt:portray_hook'/3)), 
	              (:- multifile 'attr_rt:portray_hook'/3), 
	              ('attr_rt:portray_hook'(Module, Value, Var):- 
		          attr_portray_hook(Value, Var)) |
                      Tail2]
	;
%	    warning_message("attr_portray_attrs/2 hook not defined", []),
	    Tail1 = Tail2
	), 
	(
	    retract_fact(found_hook(Module, attribute_goals)) ->
	    Tail2 = [ 
		% Deactivated until we find better solution.
		% (:- doc(hide, 'attr_rt:attribute_goals'/4)),
		      (:- multifile 'attr_rt:attribute_goals'/4), 
	              ('attr_rt:attribute_goals'(Module, V, L1, L2):- 
		          attribute_goals(V, L1, L2)) |
                      Tail3]
	;
%	    warning_message("attribute_goals/3 hook not defined", []),
	    Tail2 = Tail3
	), 
	(
	    retract_fact(found_hook(Module, dump_constraints)) ->
	    Tail3 = [ 
		% Deactivated until we find better solution.
		% (:- doc(hide, 'attr_rt:dump_constraints'/5)),
		      (:- multifile 'attr_rt:dump_constraints'/5), 
	              ('attr_rt:dump_constraints'(Module, V, C, L1, L2):- 
		          dump_constraints(V, C, L1, L2)) |
                      Tail4]
	;
%	    warning_message("dump_constraints/3 hook not defined", []),
	    Tail3 = Tail4
	), 
	Tail4 = [end_of_file].
	
	
 
	
 
		     
