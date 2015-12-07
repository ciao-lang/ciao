%%----------------------------------------------------------------------
%%
%% MYCIN PROGRAMMING LIBRARY
%%
%% compile-time support
%%
%% AUTHOR: Angel Fernandez Pineda
%% DATE:   FEBRAURY 2000
%%
%%----------------------------------------------------------------------

:- module(mycin_tr,[mycin_sentence_tr/3,mycin_clause_tr/3]).

:- use_module(library(compiler/c_itf)).
:- use_module(library(lists)).
:- use_module(library(mycin/mycin_error_reporting)).
:- use_module(library(expansion_tools)).

%%----------------------------------------------------------------------

:- data mycin_goal/3.            %% Module defines F/A mycin rule(s).
:- data mycin_export/3.          %% Module exports F/A as a mycin goal.
:- data mycin_export_all/1.      %% Module exports all.
:- data extern/3.                %% Module defines F/A to be an externally 
                                 %%   mycin goal.

:- data debug_1st_pass/0.
:- data debug_2nd_pass/0.

%%----------------------------------------------------------------------

% debug_1st_pass.
% debug_2nd_pass.

%%----------------------------------------------------------------------

mycin_sentence_tr(A,B,M) :-
	sentence_tr(A,B,M),
	( debug_1st_pass ->
	  inform_user(['1ST: ',A,' ==> ',B])
	;
	  true
	).

mycin_clause_tr(clause(A,B),clause(C,D),M) :-
	clause_tr(clause(A,B),clause(C,D),M),
	( debug_2nd_pass ->
	  inform_user(['2ND: ( ',A,' :- ',B,' ) ==> ( ',C,' :- ',D,' ) '])
	;
	  true
	).

%%----------------------------------------------------------------------
%%
%% 1ST PASS EXPANSION
%%
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% START OF FIRST PASS EXPANSION
%%----------------------------------------------------------------------

sentence_tr(0,_,Module) :-
	start_of_messages(Module,'Generating mycin interface '),
	retractall_fact(mycin_goal(Module,_,_)),
	retractall_fact(mycin_export(Module,_,_)),
	retractall_fact(extern(Module,_,_)),
	!,
	fail.

%%----------------------------------------------------------------------
%% END OF FIRST PASS EXPANSION
%%----------------------------------------------------------------------

sentence_tr(end_of_file,Clauses,Module) :-
	first_pass_checkings(Module),
	!,
	findall((:- mycin_export(F/A)),mycin_export(Module,F,A),Decls),
	append(Decls,
	       [(:- export(mycin/2)),(:- redefining(mycin/2)),
		(mycin(_,_) :- fail),end_of_file],
	       Clauses),
	end_of_messages(Module).

%%----------------------------------------------------------------------
%% EXTERN DECLARATION
%%----------------------------------------------------------------------

sentence_tr((:- extern(F/A)),[],Module) :-
	atom(F),
	integer(A),
	A >= 0,
	!,
	( extern(Module,F,A) -> true ; assertz_fact(extern(Module,F,A)) ).

sentence_tr((:- extern(Decl)),[],Module) :-
	!,
	message(Module,error,['invalid extern declaration: ',Decl]).

%%----------------------------------------------------------------------
%% PREDICATE EXPORTATION
%%----------------------------------------------------------------------

sentence_tr((:- export(X)),[],Module) :-
	var(X),
	!,
	set_fact(mycin_export_all(Module)).

sentence_tr((:- export(F/A)),[],Module) :-
	!,
	atom(F),
	integer(A),
	A > 0,
	( mycin_export(Module,F,A) -> true ;
	  assertz_fact(mycin_export(Module,F,A))
	).

%%----------------------------------------------------------------------
%% AVOID SOME MODULE SYSTEM DECLARATIONS
%%----------------------------------------------------------------------

sentence_tr((:- Decl),[],Module) :-
	not_applicable(Decl),
	!,
	message(Module,error,
	  ['directive not applicable: ',Decl]).

%%----------------------------------------------------------------------
%% VALIDATE CERTAINTY FACTOR AT RULE
%%----------------------------------------------------------------------

sentence_tr(cf(Pred,_CF),_,Module) :- 
	functor(Pred,F,A),
	extern(Module,F,A),
	!,
	fail.

sentence_tr(( cf(Pred,_CF) :- _),_,Module) :- 
	functor(Pred,F,A),
	extern(Module,F,A),
	!,
	fail.

sentence_tr(cf(Pred,CF),_,Module) :- 
	functor(Pred,F,A),
	validate_cf(CF,Module,['on rule ',Pred]),
	!,
	functor(Pred,F,A),
	( mycin_goal(Module,F,A) -> true ; 
	  assertz_fact(mycin_goal(Module,F,A)) ),
	fail.

sentence_tr((cf(Pred,CF) :- _Body),_,Module) :-
	functor(Pred,F,A),
	validate_cf(CF,Module,['on rule ',Pred]),
	!,
	( mycin_goal(Module,F,A) -> true ; 
	  assertz_fact(mycin_goal(Module,F,A)) ),
	fail.

sentence_tr(Rule,[],_Module) :- 
	( Rule = cf(_,_) ; Rule = ( cf(_,_) :- _ ) ),
	!.

%%----------------------------------------------------------------------
%% OTHER DECLARATIONS NOT EXPANDED
%%----------------------------------------------------------------------

sentence_tr((:- _),_,_) :-
	!,
	fail.

%%----------------------------------------------------------------------
%% AVOID PROLOG RULES
%%----------------------------------------------------------------------

sentence_tr((Goal :- _),[],Module) :- 
	functor(Goal,F,A),
	message(Module,error,['clause for ',F,'/',A,' is not a mycin rule']),
	!.

sentence_tr(Goal,[],Module) :- 
	functor(Goal,F,A),
	message(Module,error,['clause for ',F,'/',A,' is not a mycin rule']),
	!.

%%----------------------------------------------------------------------
%%
%% AUXILIARY PREDICATES FOR 1st PASS EXPANSION
%%
%%----------------------------------------------------------------------

first_pass_checkings(Module) :-
	mycin_export_all(Module),
	retractall_fact(mycin_export(Module,_,_)),
	mycin_goal(Module,F,A),
	assertz_fact(mycin_export(Module,F,A)),
	fail.

first_pass_checkings(Module) :-
	mycin_export(Module,F,A),
	check_exported(Module,F,A),
	fail.

first_pass_checkings(_).

%%----------------------------------------------------------------------

check_exported(Module,F,A) :-
	\+ mycin_goal(Module,F,A),
	\+ extern(Module,F,A),
	message(Module,error,['exported predicate ',F,'/',A,' is unknown']),
	retract_fact(mycin_export(Module,F,A)).

%%----------------------------------------------------------------------

not_applicable(multifile(_)).
not_applicable(meta_predicate(_)).
not_applicable(reexport(_)).
not_applicable(data(_)).
not_applicable(dynamic(_)).
not_applicable(concurrent(_)).
not_applicable(reexport(_,_)).

%%----------------------------------------------------------------------
%%
%% SECOND PASS EXPANSION
%%
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% START OF EXPANSION
%%----------------------------------------------------------------------

clause_tr(clause(0,0),_,Module) :-
	start_of_messages(Module,'Compiling mycin rules'),
	add_exported_clauses(Module),
%	add_extern_defs(Module),
	!,
	fail.

%%----------------------------------------------------------------------
%% END OF EXPANSION
%%----------------------------------------------------------------------

clause_tr(clause(mycin(G,_),(functor(G,_,_),_)),_,Module) :-
	end_of_messages(Module),
	!,
	fail.

%%----------------------------------------------------------------------
%% CLAUSE BODY  EXPANSION
%%----------------------------------------------------------------------

clause_tr(clause(cf(Pred,CF),true),clause(NewPred,true),_Module) :-
	functor(Pred,F,A),
	extern(Module,F,A),
	!,
	add_arg(CF,Pred,NewPred),
	( (number(CF),CF >= -1, CF =< 1) ->
	  true ;
	  message(Module,warning,
	  ['invalid certainty factor at ',
	   'clause of external mycin rule: ',Pred])
	).

clause_tr(clause(cf(Pred,CF),true),clause(NewPred,true),_Module) :-
	add_arg(CF,Pred,NewPred),
	!.

clause_tr(clause(cf(Pred,CF),Body),clause(NewPred,Body),Module) :-
	functor(Pred,F,A),
	extern(Module,F,A),
	!,
	add_arg(CF,Pred,NewPred).

clause_tr(clause(cf(Pred,CF),Body),clause(NewPred,NewBody),Module) :-
	add_arg(CFResult,Pred,NewPred),
	mycin_body_exp(Body,Module,ExpBody,CFListAux),
	normalize_cf_list(CFListAux,CFList),
	NewBody = (ExpBody,mycin_rt:certainty_propagation(CF,CFList,CFResult)).

%%----------------------------------------------------------------------
%% SINGLE GOAL EXPANSION
%%----------------------------------------------------------------------

mycin_body_exp(Body,Module,NewBody,CFList) :-
	CFList = i(_,none),
	body_expander(goal_exp(CFList),
	              fact_exp,
		      spec_exp,
		      Module,
		      Body,
		      NewBody).

%%----------------------------------------------------------------------

%% Avoid recursive expansion

goal_exp('$<-($'(Goal),Goal,_,_).

%% Goal is module-qualified and involves current module.

goal_exp(AModule:Goal,Exp,Module,CFList) :- 
	nonvar(AModule),
	Module = AModule,
	nonvar(Goal),
	functor(Goal,F,A),
	( mycin_goal(Module,F,A) ; extern(Module,F,A) ),
	!,
	goal_exp(Goal,Exp,Module,CFList).

goal_exp(AModule:Goal,true,Module,_) :- 
	nonvar(AModule),
	Module = AModule,
	nonvar(Goal),
	!,
	message(Module,error,
	['unknown mycin goal at current module: ',Module,':',Goal]).

%% Goal is not module qualified and involves current module.

goal_exp(Goal,mycin_rt:mycin('$<-($'(Module:NewG),CFArg,CF),Module,CFList):- 
	nonvar(Goal),
	functor(Goal,F,A),
	F \== ':',
	mycin_goal(Module,F,A),
	!,
	add_arg(CF,Goal,NewG),
	functor(NewG,_,AA),
	arg(AA,NewG,CFArg),
	add_cf(CFList,CF).

goal_exp(Goal,mycin_rt:extern('$<-($'(Module:NewG),CFArg,CF),Module,CFList):- 
	nonvar(Goal),
	functor(Goal,F,A),
	F \== ':',
	extern(Module,F,A),
	!,
	add_arg(CF,Goal,NewG),
	functor(NewG,_,AA),
	arg(AA,NewG,CFArg),
	add_cf(CFList,CF).

%% Goal is not a mycin pred.

goal_exp(Goal,mycin_rt:mycinly('$<-($'(Goal),CF),_,CFList) :-
	!,
	add_cf(CFList,CF).

%%----------------------------------------------------------------------

spec_exp(Spec,_,Module) :-
	message(Module,error,['Spec arguments not supported: ',Spec]),
	fail.

fact_exp(Spec,_,Module) :-
	message(Module,error,['fact arguments not supported: ',Spec]),
	fail.

%%----------------------------------------------------------------------
%%
%% AUXILIARY PREDICATES FOR 2nd PASS EXPANSION
%%
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% ADD CLAUSES FOR MYCIN PREDS TO BE CALLED FROM OTHER MODULES
%%----------------------------------------------------------------------

add_exported_clauses(Module) :- 
	defines_module(Base,Module),
	retract_fact(clause_of(Base,mycin(_,_),fail,_,_,_,_)),
	mycin_export(Module,F,A),
	mycin_goal(Module,F,A),
	functor(Goal,F,A),
	Goal =.. [F|Args],
	append(Args,[CF],NewArgs),
	NewGoal =.. [F|NewArgs],
	asserta_fact(
	  clause_of(Base,
	            mycin(Goal,CFResult),
%		    (!,bagof(CF,Module:NewGoal,L),or_CF_inference(L,CFResult)),
		    (!,mycin_rt:mycin(Module:NewGoal,CF,CFResult)),
		    _,Module,0,0)
	),
	fail.

add_exported_clauses(Module) :- 
	defines_module(Base,Module),
	mycin_export(Module,F,A),
	extern(Module,F,A),
	functor(Goal,F,A),
	Goal =.. [F|Args],
	append(Args,[CF],NewArgs),
	NewGoal =.. [F|NewArgs],
	asserta_fact(
	  clause_of(Base,
	            mycin(Goal,CFResult),
		    (!,mycin_rt:extern(Module:NewGoal,CF,CFResult)),
		    _,Module,0,0)
	),
	fail.

add_exported_clauses(Module) :-
	defines_module(Base,Module),
	assertz_fact(
	  clause_of(Base,
	            mycin(Goal,_),
		    (functor(Goal,F,A),
                     throw(error(
		     existence_error(mycin_procedure,Module:F/A),F/A))
		    ),
		    _,Module,0,0)
	).

/*
%%----------------------------------------------------------------------
%% Keep tracking of extern declarations
%%----------------------------------------------------------------------

add_extern_defs(Module) :-
	extern(Module,F,A),
	asserta_fact(mycin_goal(Module,F,A)),
	fail.
add_extern_defs(_).
*/

%%----------------------------------------------------------------------
%% CF Variables
%%----------------------------------------------------------------------

add_cf(i(X,_),CF) :-
	nonvar(X),
	!,
	add_cf(X,CF).

add_cf(i(i(_,CF),_),CF) :- !.

normalize_cf_list(i(X,none),L) :-
	normalize_cf_aux(X,[],L).

normalize_cf_aux(Var,Aux,Aux) :-
	var(Var),
	!.
	
normalize_cf_aux(i(X,R),Aux,L) :-
	normalize_cf_aux(X,[R|Aux],L).

%%----------------------------------------------------------------------
%%
%% OTHER AUXILIARY PREDICATES
%%
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% VALIDATION OF CF's
%%----------------------------------------------------------------------

validate_cf(CF,_,_) :-
	number(CF),
	CF >= -1.0,
	CF =< 1.0,
	!.

validate_cf(CF,Module,Where) :-
	append(['invalid certainty factor (',CF,') '],Where,Msg),
	message(Module,error,Msg),
	fail.

%%----------------------------------------------------------------------
%% HEAD TRANSLATION
%%----------------------------------------------------------------------

add_arg(NewArg,Pred,NewPred) :-
	Pred =.. [F|Args],
	append(Args,[NewArg],NewArgs),
	NewPred =.. [F|NewArgs].
