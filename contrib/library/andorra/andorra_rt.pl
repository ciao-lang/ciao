:- module(andorra_rt,[wakeup/2,
	              suspend_andorra/5,
		      obtain_vars/2,
		      simplify/2,
		      verify_det/3],_).

:- use_module(library(terms_vars), [varset/2]).
:- use_module(library(sort), [sort/2]).
:- use_module(library(sets), 
        [
            insert/3, 
            ord_union/3, 
            ord_delete/3, 
            ord_intersection_diff/4
        ]).


:- data '$read_mode'/1.

:- meta_predicate suspend_andorra(?,?,goal,?,?).

:- use_package(library(attr/attr_mono_compat)).

:- include(andorraops).

obtain_vars((C1,_C2),Vars):-
	obtain_vars(C1,Vars).
obtain_vars((C1;C2),Vars):-
	obtain_vars(C1,Vars1),
	obtain_vars(C2,Vars2),
	ord_union(Vars1,Vars2,Vars).
%	varset([Vars1,Vars2],Vars).
obtain_vars(Cond,Vars):-
	varset(Cond,Vars).


suspend_andorra(S,LS,HSusp,Cond,Type):-
	attach_attribute(S,frozen_andorra(LS,HSusp,Cond,Type,S)),
	prepare_det(LS,S).

verify_det(HOrig,LGyCs,NewLGyCs):-
	set_fact('$read_mode'(yes)),	
 	update_state(HOrig,LGyCs,NewLGyCs),
 	set_fact('$read_mode'(no)),!.

wake_determinate_(_):-
	'$read_mode'(yes),!.
wake_determinate_(Lg):-
	wake_determinate(Lg).
	
verify_attribute(affect_det(Lg,Var),Val):-
	detach_attribute(Var),
	Var = Val,
	wake_determinate_(Lg),!.

verify_attribute(frozen_andorra(LS,HSusp,_HOrig,_LGyCs,S),up):-	
	detach_attribute(S),
	remove_affect(LS,S),
	S = up,
	!, 
	call(HSusp).

update_state(_,[],[]):- !.
update_state(HOrig,[guard_clause(H,R)|LGyCs],[guard_clause(H,R)|NewLGyCs]):-
	\+ \+ HOrig = H,
	valid(R),
	!,
	update_state(HOrig,LGyCs,NewLGyCs).

update_state(HOrig,[guard_clause(_H,_R)|LGyCs],NewLGyCs):-
	 update_state(HOrig,LGyCs,NewLGyCs),!.

valid([]):- 
	!.

valid([Const|Rest]):-
	valid_(Const),
	valid(Rest).

valid_(X > Y):- !,
	( ground(X/Y) -> X > Y ; true).

valid_(X < Y):- !,
	( ground(X/Y) -> X < Y ; true).

valid_(X =< Y):- !,
	( ground(X/Y) -> X =< Y ; true).

valid_(X >= Y):- !,
	( ground(X/Y) -> X >= Y ; true).

valid_(X =:= Y):- !,
	( ground(X/Y) -> X =:= Y ; true).

valid_(X =\= Y):- !,
	( ground(X/Y) -> X =\= Y ; true).

valid_(int(X)):- !,
	( nonvar(X) -> int(X) ; true ).

valid_(integer(X)):- !,
	( nonvar(X) -> integer(X) ; true ).

valid_(nnegint(X)):- !,
	( nonvar(X) -> nnegint(X) ; true ).

valid_(flt(X)):- !,       
	( nonvar(X) -> flt(X) ; true ).

valid_(float(X)):- !,                       
	( nonvar(X) -> float(X) ; true ).

valid_(num(X)):- !,
	( nonvar(X) -> num(X) ; true ).

valid_(number(X)):- !,
	( nonvar(X) -> number(X) ; true ).

valid_(atm(X)):- !,
	( nonvar(X) -> atm(X) ; true ).

valid_(atom(X)):- !,
	( nonvar(X) -> atom(X) ; true ).

valid_(struct(X)):- !,
	( nonvar(X) -> struct(X) ; true ).

valid_(constant(X)):- !,
	( nonvar(X) -> constant(X) ; true ).

valid_(atomic(X)):- !,
	( nonvar(X) -> atomic(X) ; true ).

valid_(cayllable(X)):- !,
	( nonvar(X) -> callable(X) ; true ).


% valid_ ...

test_unify(X,Y) :- 
	set_fact('$read_mode'(yes)),
	\+ \+ X = Y,
	set_fact('$read_mode'(no)).
test_unify(_X,_Y) :-
	set_fact('$read_mode'(no)),
	!,fail.

wakeup(L1,L2) :- L1==L2, !.
wakeup([L1|L2],L3) :- L1=up, 
	wakeup(L2,L3).



prepare_det([],_):-
	!.
prepare_det([V|Rest],S):-
	( 
	    get_attribute(V,affect_det(Sset,V)) ->
	    insert(Sset,S,NewSset),
	    update_attribute(V,affect_det(NewSset,V))
        ;
	    attach_attribute(V,affect_det([S],V))
	),
	prepare_det(Rest,S),!.

combine_attributes(affect_det(Lg1,V1),affect_det(Lg2,V2)):-
	detach_attribute(V1),
	detach_attribute(V2),
	V1=V2,
        ord_union(Lg1,Lg2,Lg),
	attach_attribute(V1,affect_det(Lg,V1)),
        reduce_to_set(Lg).

reduce_to_set([]).
reduce_to_set([S|Rest]):-
        get_attribute(S,frozen_andorra(LS,HSusp,HOrig,LGyCs,S)),
        sort(LS, SortedLS),
        update_attribute(S,frozen_andorra(SortedLS,HSusp,HOrig,LGyCs,S)),
        reduce_to_set(Rest).




wake_determinate([]):-
	!.
wake_determinate([S|Rest]):-
	( 
	    get_attribute(S, frozen_andorra(LS,HSusp,HOrig,LGyCs,S)) 	->  
	    (
		LGyCs = builtin ->
		simplify(HOrig,NewCond),
		update_condition_builtin(S,LS,HSusp,NewCond)
	    ;
		verify_det(HOrig,LGyCs,NewLGyCs),
		update_condition(LS,HSusp,HOrig,NewLGyCs,S)
	    )
	; 
	    true
	),  
	wake_determinate(Rest),!.


update_condition_builtin(S,LS,HSusp,NewCond):-
	( 
	    NewCond = true ->
	    detach_attribute(S),
	    remove_affect(LS,S),	                
	    !,call(HSusp) 
	; 
	    reduce_attributes_builtin(S,LS,HSusp,NewCond,builtin)
	).


update_condition(LS,HSusp,HOrig,NewLGyCs,S) :-
	( 
	    NewLGyCs = [] -> 
	    detach_attribute(S),   
	    remove_affect(LS,S),	                
	    !,fail 
	;
	    ( 
		NewLGyCs = [_] -> 
		detach_attribute(S),
		remove_affect(LS,S),	                
		!,
		call(HSusp)
	    ;
		reduce_attributes(S,LS,HSusp,HOrig,NewLGyCs)
	    )
	).



reduce_attributes_builtin(S,LS,HSusp,HOrig,LGyCs):-
	obtain_vars(HOrig,NewLS),!,
	update_attribute(S, frozen_andorra(NewLS,HSusp,HOrig,LGyCs,S)),
	sort(LS,OldLS),
	ord_intersection_diff(OldLS,NewLS,I,OldVars),
	ord_intersection_diff(NewLS,OldLS,I,Vars),
	remove_affect(OldVars,S),
	add_affect(Vars,S).


reduce_attributes(S,LS,HSusp,HOrig,LGyCs):-
	varset(HOrig,NewLS),
	update_attribute(S, frozen_andorra(NewLS,HSusp,HOrig,LGyCs,S)),
	sort(LS,OldLS),
	ord_intersection_diff(OldLS,NewLS,I,OldVars),
	ord_intersection_diff(NewLS,OldLS,I,Vars),
	remove_affect(OldVars,S),
	add_affect(Vars,S).



add_affect([],_S).
add_affect([V|Rest],S):-
	( 
	    get_attribute(V,affect_det(Sset,V)) ->
	    insert(Sset,S,NewSset),
	    update_attribute(V,affect_det(NewSset,V))
        ;
	    attach_attribute(V,affect_det([S],V))
	),
	add_affect(Rest,S).





remove_affect([],_S).
remove_affect([V|Vs],S):-
	(
            var(V) ->
            get_attribute(V,affect_det(Lg,V)),
            ord_delete(Lg,S, NewLg),
            (
                NewLg = [] ->
                detach_attribute(V)
            ;
                update_attribute(V,affect_det(NewLg,V))
            )
        ;
            true
        ),
        remove_affect(Vs,S).


getterm(A,A):-
	var(A),!,fail.
getterm(term(A,Path),C):- 
	!, 
	getterm_(A,Path,C).
getterm(A,A).


getterm_(Term,[],Term):- 
	nonvar(Term),!.
getterm_(Term,[N|Path],SubTerm):- 
	nonvar(Term),
	functor(Term,_,A),
	(
	    A>=N ->
	    arg(N,Term,Arg),
	    getterm_(Arg,Path,SubTerm)
	;
	    SubTerm = '$missing'
	).
  
	
instantiated(Term,[],'$nonvar'):- nonvar(Term).
instantiated(Term,[N|Path],Up):-
	nonvar(Term),
	functor(Term,_,A),
	A>=N,
	arg(N,Term,Arg),
	instantiated(Arg,Path,Up).
instantiated(Term,Path,instantiated(Term,Path)).

simplify(true, true):-!.
simplify(ground(Term), true):- ground(Term), !.
simplify(ground(Term), NewTerm):-
        varset(Term, AllVars),
        conjunction(AllVars, NewTerm),!.

simplify(nonvar(Term), true):- nonvar(Term), !.
simplify(nonvar(Term), nonvar(Term)):-!.


simplify(A ?= B,Simplified) :-
	(
	    getterm(A,TermA) ->
	    (
		getterm(B,TermB) ->
		(
		    test_unify(TermA,TermB) ->
		    Simplified = true
		;
		    Simplified = false
		)
	    ;
		Simplified = (A ?= B)
	    )
	;
	    Simplified = (A ?= B)
	),!.



simplify(A ?\= B,Simplified) :-
	(
	    getterm(A,TermA) -> 
	    (
		getterm(B,TermB) ->
		(
		    \+ test_unify(TermA,TermB) ->
		    Simplified = true
		;
		    Simplified = (TermA ?\= TermB)
%		    Simplified = false
		)
	    ;
		Simplified = (A ?\= B)
	    )
	;
	    Simplified = (A ?\= B)
	),!.

simplify(instantiated(A,Path),Result):- 
	 instantiated(A,Path,Term),
	 (
	     Term = '$nonvar' ->
	     Result = true
	 ;
	     Result = Term
	 ),!.

simplify(A \== B,true) :-
	A \== B,!.
simplify(_A \== _B,false):- !.
	
simplify(A == B,true) :-
	A == B,!.
simplify(_A == _B,false):- !.

simplify(false,false):- !.

simplify((C1, C2), Result):-
        simplify(C1, R1), !,
	(
	    R1 = false ->
	    Result = false
	;
	    (
		R1 = true ->
		simplify(C2, Result)
	    ;
		Result = (C1,C2)
	    )
 	),!.


simplify((C1; C2), Result):-
        simplify(C1, R1), !,
        (
            R1 = true ->
            Result = true
        ;
            simplify(C2, R2), !,
            or(R1, R2, Result)
        ),!.


simplify(C,true):- call(C),!.
simplify(_C,false).

conjunction([G], ground(G)):- !.
conjunction([V|Vs], (ground(V), Rest)):- 
        conjunction(Vs, Rest).


%or(true, true, true):- !.
or(C1, false, C1):- !.
or(false, C2, C2):- !.
or(_C1, true, true):- !.
%or(true, _C2, true):- !.
or(C1, C2, (C1; C2)):- !.
