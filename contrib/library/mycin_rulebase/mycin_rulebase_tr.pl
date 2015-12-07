%%----------------------------------------------------------------------
%%
%% MYCIN PROGRAMMING LIBRARY
%%
%% compile-time support
%%
%% AUTHOR: Angel Fernandez Pineda
%% DATE:   2002
%%
%% Distributed under Ciao Prolog license terms
%%
%%----------------------------------------------------------------------
%% OVERVIEW OF EXPANSION:
%%
%% -- At first pass --
%%
%% - Clause heads are hold into mycin_pred/3 with no duplicates.
%% - Clause Head is translated into '$mycin_rule$'(Head,CF).
%% - export directives are hold into exported/3. 
%% - export directives are checked and re-generated at end of expansion.
%% - a new clause is generated for each entry point (exported rule) where
%%   CF is the first argument.
%% - mycin_pred/1 directives are generated to be hold in itf file.
%%
%% -- At second pass --
%% 
%% - Goal translation takes place for mycin goals.
%% - A goal is added to each clause in order to accomplish 
%%   certainty propagation.
%%----------------------------------------------------------------------

:- module(mycin_rulebase_tr,[mycin_sentence_tr/3,mycin_clause_tr/3],[]).

:- use_module(library(compiler/c_itf)).
:- use_module(library(lists)).
:- use_module(library(aggregates)).
:- use_module(mycin_rulebase_error_reporting).

%%----------------------------------------------------------------------

:- data mycin_pred/3.            %% Module defines F/A mycin rule(s).
:- data exported/3.              %% Module exports F/A mycin rule(s).

:- data debug_1st_pass/0.
:- data debug_2nd_pass/0.

%%----------------------------------------------------------------------

%debug_1st_pass.
%debug_2nd_pass.

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
	retractall_fact(mycin_pred(Module,_,_)),
	!,
	fail.

%%----------------------------------------------------------------------
%% END OF FIRST PASS EXPANSION
%%----------------------------------------------------------------------

sentence_tr(end_of_file,Clauses,Module) :-
	!,
	validate_exports(Module),
	findall( (:- export(F/AA)),
                 (exported(Module,F,A),AA is A+1),
		 ExportDecls),
	findall( (ExpGoal :- '$mycin_rule$'(Goal,CF)),
	         ( exported(Module,F,A),
                   AA is A+1,
		   functor(ExpGoal,F,AA),
		   ExpGoal =.. [F,CF|Args],
		   Goal =.. [F|Args]
		 ),
		 EntryPoints),
	findall((:- mycin_pred(F/A)),mycin_pred(Module,F,A),Decls),
	append(ExportDecls,EntryPoints,Aux1),
	append(Aux1,Decls,Aux2),
	append(Aux2,
	[
%	    (:- multifile('$mycin_rule$'/2)),
	    ('$$eof$$'),
	    end_of_file
        ],Clauses),
	end_of_messages(Module),
	true.

%%----------------------------------------------------------------------
%% VALIDATE CERTAINTY FACTOR AT RULE
%%----------------------------------------------------------------------

%% Simple fact rule

sentence_tr(cf(Head,CF),[('$mycin_rule$'(Head,CF))],Module) :-
	number(CF),
	is_valid_cf(cf(Head,CF)),
	!,
	note_mycin_pred(Head,Module).

%% Not a simple fact rule

sentence_tr((cf(Head,CF) :- Body),
            [('$mycin_rule$'(Head,CF) :- Body)], Module) :-
	var(CF),
	!,
	note_mycin_pred(Head,Module),
	message(Module,note,['Mycin metarule defined by: ',Head]).

sentence_tr((cf(Head,CF) :- Body),
            [('$mycin_rule$'(Head,CF) :- Body)], Module) :-
	is_valid_cf(cf(Head,CF)),
	!,
	note_mycin_pred(Head,Module),
	message(Module,note,['Mycin metarule defined by: ',Head]).

%% Invalid mycin rule

sentence_tr(cf(Head,_),[],Module) :-
	!,
	functor(Head,F,A),
	message(Module,error,
          ['Illformed mycin rule for ',F,'/',A]).

%%----------------------------------------------------------------------
%% EXPORTED RULES
%%----------------------------------------------------------------------

sentence_tr((:- export(F/A)),[],Module) :-
	atom(F),
	integer(A),
	!,
	if(exported(Module,F,A), 
           true, 
           asserta_fact(exported(Module,F,A))).

%%----------------------------------------------------------------------
%% AVOID SOME PROLOG DIRECTIVES
%%----------------------------------------------------------------------

sentence_tr((:- Directive),[],_) :-
	allowed_directive(Directive),
	!,
        fail.

sentence_tr((:- Directive),[],Module) :-
	!,
	message(Module,
                warning,
                ['Compiler directive ignored: ',Directive]).

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
%% SECOND PASS EXPANSION
%%
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% START OF EXPANSION
%%----------------------------------------------------------------------

clause_tr(clause(0,0),_,Module) :-
	start_of_messages(Module,'Compiling mycin rules'),
	!,
	fail.

%%----------------------------------------------------------------------
%% END OF EXPANSION
%%----------------------------------------------------------------------

clause_tr(clause('$$eof$$',_),_,Module) :-
	end_of_messages(Module),
	!,
	fail.

%%----------------------------------------------------------------------
%% CLAUSE BODY EXPANSION
%%----------------------------------------------------------------------

% Compile mycin rule

clause_tr(clause('$mycin_rule$'(Head,CF),Body),
          clause('$mycin_rule$'(Head,CFResult),NewBody), Module) :-
	Body \== true,
	nonvar(CF),
	!,
	mycin_body_exp(Module,Body,ExpBody,[],CFList),
	NewBody = (ExpBody,mycin_rt:certainty_propagation(CF,CFList,CFResult)),
	true.

% Compile mycin metarule : body is not expanded

clause_tr(clause('$mycin_rule$'(Head,CF),Body),
          clause('$mycin_rule$'(Head,CF),
                 if((Body,mycin_rt:validate_metarule_cf(CF)),
                    true,
                    CF=0)),
	  _Module) :- 
	Body \== true,
	var(CF).

%%----------------------------------------------------------------------
%%
%% AUXILIARY PREDICATES FOR 1st PASS EXPANSION
%%
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% PROLOG DIRECTIVES THAT ARE ALLOWED IN MYCIN PROGRAMS
%%----------------------------------------------------------------------

allowed_directive(use_module(_)).
allowed_directive(use_module(_,_)).

%%----------------------------------------------------------------------
%% CHECK CORRECT SPECIFICATION OF CERTAINTY FACTOR AT RULES
%%----------------------------------------------------------------------

is_valid_cf(cf(Head,CF)) :-
	var(CF),
	nonvar(Head).

is_valid_cf(cf(Head,CF)) :-
	nonvar(Head),
	number(CF),
	CF >= -1,
	CF =< +1.

%%----------------------------------------------------------------------
%% TAKE ACCOUNT OF MYCIN PRED (IF NEEDED)
%%----------------------------------------------------------------------

note_mycin_pred(Head,Module) :-
	functor(Head,F,A),
	(mycin_pred(Module,F,A) -> 
	 true 
        ; 
         asserta_fact(mycin_pred(Module,F,A))
        ).

%%----------------------------------------------------------------------
%% VALIDATE EXPORTS
%%----------------------------------------------------------------------

validate_exports(Module) :-
	exported(Module,F,A),
	\+ mycin_pred(Module,F,A),
	retract_fact(exported(Module,F,A)),
	message(Module,error,
               ['No rules for exported ',F,'/',A]),
	fail.

validate_exports(_).

%%----------------------------------------------------------------------
%%
%% AUXILIARY PREDICATES FOR 2nd PASS EXPANSION
%%
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% navigate through clause body
%%
%% mycin_body_exp(+CurrentModule,+GoalToBeExpanded,-ExpandedGoal,
%%                +PreviousCFList,- NewCFList)
%%----------------------------------------------------------------------

mycin_body_exp(Module,(Goal1,Goal2),(ExpGoal1,ExpGoal2),CFList,NewCFList) :-
	!,
	mycin_body_exp(Module,Goal1,ExpGoal1,CFList,AuxCFList),
	mycin_body_exp(Module,Goal2,ExpGoal2,AuxCFList,NewCFList).

mycin_body_exp(Module,
	       (Goal1;Goal2),
	       (ExpGoal1,ExpGoal2,
                  mycin_support:or_CF_inference(OrCFList,OrCF)
	       ),
	       PrevCFList,
	       [OrCF|PrevCFList]) :-
	!,
	mycin_body_exp(Module,Goal1,ExpGoal1,[],CFList1),
	mycin_body_exp(Module,Goal2,ExpGoal2,[],CFList2),
	append(CFList1,CFList2,OrCFList).

mycin_body_exp(Module,
	       \+(Goal),
	      (ExpGoal,map(CFList,(''(CF,NegCF) :- NegCF is -1*CF),NegCFList)),
	      PrevCFList,
	      NewCFList) :-
	!,
	length(CFList,Aux),
	length(NegCFList,Aux),
	mycin_body_exp(Module,Goal,ExpGoal,[],CFList),
	append(PrevCFList,NegCFList,NewCFList).

mycin_body_exp(Module,'->'(Goal1,Goal2),ExpBody,PrevCFList,NewCFList) :-
	!,
	mycin_body_exp(Module,(\+(Goal1);Goal2),ExpBody,PrevCFList,NewCFList).

mycin_body_exp(Module,if(_,_,_),true,PrevCFList,PrevCFList) :-
	!,
	message(Module,error,['if/3 usage is not allowed in mycin rules']).

mycin_body_exp(Module,SingleGoal,ExpGoal,PrevCFList,[CF|PrevCFList]) :-
	goal_exp(Module,SingleGoal,ExpGoal,CF),
	!.

mycin_body_exp(_Module,SingleGoal,SingleGoal,CFList,CFList).

%%----------------------------------------------------------------------
%% Single goal expansion
%%----------------------------------------------------------------------

% Call to local rule

goal_exp(Module,Module:Goal,'$mycin_rule$'(Goal,CF),CF) :-
	functor(Goal,F,A),
	mycin_pred(Module,F,A),
	!.

goal_exp(Module,Goal,'$mycin_rule$'(Goal,CF),CF) :-
	functor(Goal,F,A),
	mycin_pred(Module,F,A),
	!.

% Call to entry point in another mycin module

goal_exp(Module,AtModule:Goal,AtModule:EntryPoint,CF) :-
	atom(AtModule),
	is_mycin_goal(Goal,AtModule),
	AtModule \== Module,
	Goal       =.. [F|Args],
	EntryPoint =.. [F,CF|Args],
	!.

goal_exp(Module,Goal,AtModule:EntryPoint,CF) :-
	is_mycin_goal(Goal,AtModule),
	AtModule \== Module,
	Goal       =.. [F|Args],
	EntryPoint =.. [F,CF|Args],
	!.

% Call to Prolog goal

goal_exp(_Module,Goal,mycin_rt:mycinly(Goal,CF),CF).


%%----------------------------------------------------------------------
%% LOCATE EXPORTED MYCIN GOAL 
%%----------------------------------------------------------------------

is_mycin_goal(Goal,AtModule) :-
	functor(Goal,F,A),
	c_itf:decl(Base,mycin_pred(F/A)),
	AA is A+1,
	c_itf:exports(Base,F,AA,static,_),
	c_itf:defines_module(Base,AtModule).
