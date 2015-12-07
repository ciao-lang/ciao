:- module(persdb_sql_tr,[sql_persistent_tr/2, sql_goal_tr/2,
	dbId/2],[assertions]).

%:- reexport(library(persdb_sql(db_client),
%	            [socketname/1,dbname/1,user/1,passwd/1])).
%:- reexport(library(persdb_sql(pl2sql),
%	            [projterm/1,querybody/1])).

:- data dbId/2.

sql_persistent_tr( (:- sql_persistent(PrologDef,SQLDef,DBId)), ClauseList) :-
 	functor(PrologDef,PrologName,Arity),
 	functor(Consequence,PrologName,Arity),
	ClauseList =  [ '$is_sql_persistent'(PrologDef,SQLDef,DBId),
 			(Consequence :- db_call_db_atomic_goal(DBId,Consequence)) ],
	assertz_fact(dbId(DBId,Consequence)).

%jcf	
% If a persistent predicate is also defined as a normal predicate inside the same module, 
% queries to that predicate should provide wrong results (first database rows, then 
% prolog clauses), but it doesn't. THIS MUST BE CHECKED!

sql_goal_tr( Goal, db_call_db_atomic_goal(DBId,Goal)) :-
	current_fact(dbId(DBId,Goal)).
%jcf
