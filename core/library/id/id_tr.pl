:- module(id_tr, [id_sentence/3, id_clause/3],[assertions, hiord]).

:- use_module(library(lists), [append/3]).
:- use_module(library(terms), [atom_concat/2]).

:- data iterative/4.
:- data driver_already_generated/2.

id_sentence(0, 0, Module):-!,
	reset(Module).
id_sentence(end_of_file, end_of_file, Module):-!,
	(
	    current_fact(iterative(Module, _, _, _)) -> 
	    true
	;
	    warning(['No iterative predicate declared in module ', Module])
	).
id_sentence((:- Decl), [], Module):-
	( 
	    Decl = iterative(Pred,Init,Formul)
	->
	    Limit = unlimited
	;
	    Decl = iterative(Pred,Init,Formul, Limit)
	),
	declare_iterative(Module, Pred, info(Init, Formul, Limit)).
id_sentence((Head :- Body), Transformation, Module):-!,
	transform_clause_head(Module, Head, Body, Transformation).
id_sentence(Head, Transformation, Module):- 
	transform_clause_head(Module, Head, true, Transformation).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
id_clause(end_of_file, end_of_file, Module):-
	reset(Module).
id_clause(clause(Head_depth, Body), clause(Head_depth, Body_depth), Module):-
	current_fact(iterative(Module, _, (Head_depth, Depth), _)),
	transform_body(Body, Module, Body_depth, Depth). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reset(Module):-
	retractall_fact(iterative(Module, _, _, _)), 
	retractall_fact(driver_already_generated(Module, _)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
declare_iterative(Module, F/N, Info):-
	functor(Call, F, N), 
	(
	    current_fact(iterative(Module, Call, _, _)) ->
	    warning(['Predicate ', Module:F/N, ' already delcared as iterative. Declaration ignored.'])
	;
	    transform_iterative(Call, Call_depth, Depth),
	    assertz_fact(iterative(Module, Call, (Call_depth,Depth), Info))
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
transform_clause_head(Module, Head, Body, Transformation):-
	current_fact(iterative(Module, Head, (Head_depth, _), _Info)), 
	generate_driver_if_necessary(Module, Head, [(Head_depth :- Body)], Transformation).

	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_driver_if_necessary(Module, Head, Tail, Tail):-
 	    current_fact(driver_already_generated(Module, Head)), !.
generate_driver_if_necessary(Module, Head, Tail, 
	[ (LHS0 :- RHS0), (LHS1 :- RHS1), (LHS2 :- RHS2) | Tail ]) :-
	
	functor(Head, F, N), functor(Call, F, N),
	current_fact(iterative(Module, Call, (Call_depth, Depth), info(Init,Formul, Limit))),

        Call =.. [F|Args],
	LHS0 = Call,
        atom_concat(['$', F, '__iterative'],F_iterative),
        append(Args, [Init], ArgsInit),               
        Call_iterative0 =.. [F_iterative|ArgsInit],
	(
	    (Limit = unlimited ; Init =< Limit) ->
	    RHS0 = Call_iterative0
	;
	    RHS0 = (Init =< Limit, Call_iterative0)
	),

	append(Args, [Depth],ArgsDepth),   
	Call_iterative1 =.. [F_iterative|ArgsDepth],
        LHS1 = Call_iterative1,
        RHS1 = Call_depth,
 
        LHS2 = Call_iterative1,
        append(Args, [NextDepth], ArgsNextDepth),
        Call_iterative2 =.. [F_iterative|ArgsNextDepth],
        (
	    Limit = unlimited  ->
	    RHS2 = (Formul(Depth, NextDepth), Call_iterative2)
	;
	    RHS2 = (Depth < Limit, Formul(Depth, NextDepth), Call_iterative2)
	),


	assertz_fact(driver_already_generated(Module, Call)).


transform_body(B, Module, B_depth, Depth) :-
	transform_literals(B, Module, B_depth_, NewDepth, Flag),
	(
	    var(Flag) ->
	    (
		B_depth_ = true  ->
		B_depth = (Depth > 0)
	    ;
		B_depth = (Depth > 0 ; B_depth_)
	    )
	;
	    B_depth = (Depth > 0, NewDepth is Depth - 1, B_depth_)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
transform_literals((L,B), Module, (L_depth,B_depth), Depth, Flag) :-
        transform_literal(L, Module, L_depth, Depth, Flag),
        transform_literals(B, Module, B_depth, Depth, Flag).
transform_literals((L;B), Module, (L_depth;B_depth), Depth, Flag) :-
        transform_literal(L, Module, L_depth, Depth, Flag),
        transform_literals(B, Module, B_depth, Depth, Flag).
transform_literals(L, Module, L_depth, Depth, Flag) :-
        transform_literal(L, Module, L_depth, Depth, Flag).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
transform_literal(L, Module, L_depth, Depth, Flag) :-
	current_fact(iterative(Module, L, (L_depth, Depth), _Info)), !,
	Flag = t.
transform_literal(L, _Module, L, _, _).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
transform_iterative(Call, Call_depth, Depth):-
	Call =.. [F|Args],
	atom_concat(['$', F,'__depth'],F_depth),
	append(Args,[Depth],ArgsDepth),
	Call_depth =.. [F_depth|ArgsDepth].
	
