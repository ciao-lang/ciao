%%%%%%%
%%%%%%%  PENDING
%%%%%%%
%%%%%%%  ADD more predicates type N is N + 2 - (comparisons!!!).
%%%%%%%  Refine IS and ARITY measures -> t_exec
%%%%%%%  Intordice t_exec mixing from memorization
%%%%%%%
%%%%%%%


:- module(and_sim_plan_tr,
        [
            do_term_expansion/3
        ]).

:- use_module(library(write), [write/1]).

:- use_module(library(lists),
	[
	    reverse/2,
	    append/3
	]).

:- dynamic trans_expanding/0, 'trans$cont'/1.

:- dynamic module_name/1.

do_term_expansion(0,_,_Module).
do_term_expansion(end_of_file,_,_).
do_term_expansion(':-'(Com),':-'(Com),_).

do_term_expansion(Clause,Clauses,_) :-
 	( 
 	    Clause = (Head :- Body) -> 
 	    true
 	; 
 	    Head = Clause, 
 	    Body = true 
 	),
	translate_clause(Head,Body,Clauses).

translate_clause(sim(X,Y,Z),Body,(sim(X,Y,Z) :- Body)) :- !,
  	write((sim(X,Y,Z) :- Body)), nl.
translate_clause(skip_first(X,Y),Body,(skip_first(X,Y) :- Body)) :- !,
  	write((skip_first(X,Y) :- Body)), nl.
translate_clause(w_event(X,Y),Body,(w_event(X,Y) :- Body)) :- !,
  	write((w_event(X,Y) :- Body)), nl.
translate_clause(update_max_wam(X,Y),Body,(update_max_wam(X,Y) :- Body)) :- !,
  	write((update_max_wam(X,Y) :- Body)), nl.
translate_clause(not_end(X,Y,Z),Body,(not_end(X,Y,Z) :- Body)) :- !,
  	write((not_end(X,Y,Z) :- Body)), nl.
translate_clause(par(X,Y,Z),Body,(par(X,Y,Z) :- Body)) :- !,
  	write((par(X,Y,Z) :- Body)), nl.
translate_clause(par_aux(X,Y),Body,(par_aux(X,Y) :- Body)) :- !,
  	write((par_aux(X,Y) :- Body)), nl.
translate_clause(get_listOK(X,Y,Z,T),Body,(get_listOK(X,Y,Z,T) :- Body)) :- !,
  	write((get_listOK(X,Y,Z,T) :- Body)), nl.
translate_clause(Head,Body,Clauses) :- 
	functor(Head,_Pred,Arity),
	T_EXEC is Arity + 1,
	translate_body(Body,TBody,Id), 
	Clauses = (Head :- current_id(Id), w_event(t_exec(T_EXEC),Id), TBody),
  	write(Clauses), nl.

translate_body(','(H_Body,R_Body),','(H_TBody,R_TBody),Id) :- !,
	translate_body_elem(H_Body,H_TBody,Id),
	translate_body(R_Body,R_TBody,Id).
translate_body(Body,TBody,Id):-
	translate_body_elem(Body,TBody,Id).

translate_body_elem('is'(A,B),(w_event(t_exec(3),Id),'is'(A,B)),Id) :- !.
translate_body_elem('<'(A,B),(w_event(t_exec(3),Id),'<'(A,B)),Id) :- !.
translate_body_elem('>='(A,B),(w_event(t_exec(3),Id),'>='(A,B)),Id) :- !.
translate_body_elem('=='(A,B),(w_event(t_exec(3),Id),'=='(A,B)),Id) :- !.
translate_body_elem('='(A,B),(w_event(t_exec(2),Id),'='(A,B)),Id) :- !.
translate_body_elem(sleep(T),(w_event(t_exec(T),Id)),Id) :- !.

translate_body_elem('&'(A,B),T_Body,Id) :- !,
	get_par_list(0,A,B,Goal_L,Id_L),
	T_Body = (par(Goal_L,Id_L,Id_L_OK), undo(w_event(restart(Id_L_OK),Id))).
translate_body_elem(Body, Body,_).

get_par_list(Last_Id,A,'&'(B,C),[A|Goal_L],[Last_Id1|Id_L]) :- !,
	Last_Id1 is Last_Id + 1,
	get_par_list(Last_Id1,B,C,Goal_L,Id_L).

get_par_list(Last_Id,A,B,[A,B],[Last_Id1,Last_Id2]) :- !,
	Last_Id1 is Last_Id + 1,
	Last_Id2 is Last_Id + 2.
