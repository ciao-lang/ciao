:- module(_,
        [
	    init_sql_persdb/0,
%% Was just exported to document declaration (for now done with doinclude).
%	    sql_persistent/3,
%	    dbname/1,
%	    user/1,
%	    passwd/1,
%	    socketname/1,
 	    dbassertz_fact/1,
 	    dbretract_fact/1,
 	    dbcurrent_fact/1,
	    dbretractall_fact/1,
 	    make_sql_persistent/3,
	    dbfindall/4,
%	    projterm/1,
%	    querybody/1,
	    dbcall/2, %% Internal predicate
	    db_call_db_atomic_goal/2,
	    sql_query/3,
	    sql_get_tables/2,
	    sql_table_types/3
        ],[assertions, regtypes, basicmodes, det_hook]).

:- reexport(library(persdb_mysql/db_client_types),
	            [socketname/1,dbname/1,user/1,passwd/1]).

:- reexport(library(persdb_mysql/pl2sql),
	            [projterm/1,querybody/1]).

:- reexport(library(persdb_sql_common/sqltypes),
	            [sqltype/1]).
%% sql_query_one_tuple_more/2 internal predicates

%% ---------------------------------------------------------------------------

%jcf% :- pred sql_persistent_location(Keyword,Location) ::  persLocId * persLocation
%jcf% 
%jcf% # "@var{Keyword} is an identifier for the persistent data location
%jcf%   @var{Location}.".
%jcf% 
%jcf% :- regtype persLocation/1.
%jcf% 
%jcf% persLocation(db(Name, User, Password, Machine:Port)) :-
%jcf% 	atm(Name),
%jcf% 	atm(User),
%jcf% 	atm(Password),
%jcf% 	atm(Machine),
%jcf% 	int(Port).

%% ---------------------------------------------------------------------------
%% Multifile predicates.

:- doc(sql_persistent_location/2,"Relates names of locations
   (the @var{Keyword}s) with descriptions of such locations
   (@var{Location}s).").

:- pred sql_persistent_location(Keyword,DBLocation) =>  persLocId * database_desc

# "In this usage, @var{DBLocation} is a @em{relational database}, in which
   case the predicate is stored as tuples in the database.".

:- multifile sql_persistent_location/2.
:- data sql_persistent_location/2.

%% -----------------------------------------
:- doc(hide,'$is_sql_persistent'/3).

:- multifile '$is_sql_persistent'/3.
:- data sql_persistent/2.

%% -----------------------------------------
:- doc(hide,sql__relation/3).
:- doc(hide,sql__attribute/4).
:- multifile([sql__relation/3,sql__attribute/4]).
:- data([sql__relation/3,sql__attribute/4]).


%% ---------------------------------------------------------------------------
%% Properties and regular types.

:- regtype database_desc(D) # "@var{D} is a structure describing a
   database.".

database_desc(db(DBId,User,Passwd,Socket)) :-
	dbname(DBId),
	user(User),
	passwd(Passwd),
	socketname(Socket).

:- doc(database_desc/1,"@includedef{database_desc/1}").

:- regtype tuple(T) # "@var{T} is a tuple of values from the ODBC database
   interface.".

tuple(T) :-
	list(T,atm).

:- doc(tuple/1,"@includedef{tuple/1}").
:- doc(doinclude, tuple/1).

:- doc(doinclude, dbconnection/1).

:- regtype answertableterm(AT) # "@var{AT} is a response from the ODBC
   database interface.".

answertableterm(ok).
answertableterm(t(Answers)) :-
	list(Answers,tuple).
answertableterm(err(Answer)) :-
	term(Answer).	

:- doc(answertableterm/1,"@includedef{answertableterm/1}").

:- regtype answertupleterm(X) #  "@var{X} is a predicate containing a tuple.".

answertupleterm([]).
answertupleterm(tup(T)) :-
	tuple(T).

:- doc(answertupleterm/1,"@includedef{answertupleterm/1}").

%% Imported from db_client.
:- doc(doinclude,dbname/1).
:- doc(doinclude,user/1).
:- doc(doinclude,passwd/1).
:- doc(doinclude,socketname/1).

:- doc(prologPredTypes/1,"@includedef{prologPredTypes/1}").

:- prop prologPredTypes(PredTypes) # "@var{PredTypes} is a structure
    describing a Prolog predicate name with its types.".

prologPredTypes(PredTypes) :-
	PredTypes =.. [PredName|Types],
	atm(PredName),
 	list(Types,sqltype).

:- doc(tableAttributes/1,"@includedef{tableAttributes/1}").

:- prop tableAttributes(TableAttributes)  # "@var{TableAttributes} is a
    structure describing a table name and some attributes.".

tableAttributes(TableAttributes) :-
 	TableAttributes =.. [TableName|AttributeNames],
 	atm(TableName),
 	list(AttributeNames,atm).

:- prop persLocId(Id)  # "@var{Id} is the name of a persistent storage location.".

persLocId(Id) :-
 	atm(Id).

:- prop fact(X)  # "@var{X} is a fact (a term whose main functor is not @tt{':-'/2}).".

fact(_).

:- prop atomicgoal(G)  # "@var{G} is an atomic goal.".

atomicgoal( G ) :-
	term(G).

:- doc(atomicgoal/1,"@var{G} is a single goal, not containing
   conjunctions, disjunctions, etc.").


%-----------------------------------------------------------------------------
:- use_module(library(dynamic)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(terms_vars), [varset/2]).
:- use_module(library(messages), [error_message/2, debug_message/2]).
:- use_module(library(lists), [length/2, append/3]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(engine(internals), [term_to_meta/2, module_concat/3]).

:- use_module(library(persdb_mysql/mysql_client)).

:- use_module(library(persdb_mysql/pl2sql), 
	[pl2sqlstring/3,sqlstring/1
	%% ,sqltype/1
	]).
:- use_module(library(persdb_sql_common/sqltypes), 
	[accepted_type/2
	%% ,sqltype/1
	]).
:- use_module(library(persdb_sql_common/pl2sqlinsert), 
	[pl2sqlInsert/2]).
:- use_module(library(persdb_mysql/delete_compiler/pl2sqldelete), 
	[pl2sqlDelete/2]).


:- doc(bug,"At least in the shell, reloading a file after changing
   the definition of a persistent predicate does not eliminate the old
   definition...").

:- doc(bug,"Functionality missing: some questions need to be debugged.").

:- doc(bug,"Warning: still using kludgey string2term and still
   using some non-uniquified temp files.").

:- doc(bug,"Needs to be unified with the file-based library.").

% :- multifile issue_debug_messages/1.
% :- data issue_debug_messages/1.
% %%%%issue_debug_messages(persdb_mysql_rt).
%jcf%:- use_module(library(format)).
%jcf%:- set_prolog_flag(write_strings,on).
%jcf%jcf_message(S):-
%jcf%	open('/home/jcorreas/cvs/Systems/Amos/DataBase/test.sql',append,Stream),
%jcf% 	format(Stream,"~s~n",[S]),
%jcf% 	flush_output(Stream),
%jcf% 	close(Stream).


%% ---------------------------------------------------------------------------
%% Intro
%% ---------------------------------------------------------------------------

:- doc(doinclude, projterm/1).

:- doc(doinclude, querybody/1).

:- doc(title,"SQL persistent database interface").

:- doc(subtitle_extra,"@bf{The CIAO System Documentation Series}").
:- doc(subtitle_extra,"Technical Report CLIP 10/98.0").
:- doc(subtitle_extra,"RadioWeb (ESPRIT Project 25562) Report D3.1.M2-A2").

:- doc(subtitle_extra,"ECCOSIC (@em{Comision Conjunta Hispano-Norteamericana}").
:- doc(subtitle_extra,"@em{de Cooperacion Cientifica y Tecnologica} Project 98059)"). 
%% :- doc(subtitle_extra,"@em{Draft printed on:} @today{}").

:- doc(subtitle_extra,"December 26, 1998").

:- doc(author, "Ignacio Caballero").
:- doc(author, "Daniel Cabeza").
:- doc(author, "Jos@'{e} Manuel G@'{o}mez P@'{e}rez").
:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Jose F. Morales").
:- doc(author, "Manuel Carro").
:- doc(author, "The CLIP Group").
:- doc(address, "@tt{clip@@dia.fi.upm.es}").
:- doc(address, "@tt{http://www.clip.dia.fi.upm.es/}").
:- doc(address, "Facultad de Inform@'{a}tica").
:- doc(address, "Universidad Polit@'{e}cnica de Madrid").

%% %% The Free Software Foundation.
%% :- doc(copyright,"
%% Copyright @copyright{} 1996-2002 The CLIP Group
%% 
%% @include{DocCopyright.lpdoc}
%% ").
%% :- include(library('Copyright')).

:- doc(summary,"This library implements the SQL-based version of
   the generic of concept @em{persistent predicates}. As introduced in
   the filebased version documentation (the @lib{persdb} library), a
   persistent predicate is a relation such that any updates made to it
   from a program remain after the execution of that program
   terminates.  In this case, persistence is achieved by saving the
   definition and state of the predicates in a @em{relational
   database}. This way, a very natural and high-level interface is
   provided for accessing SQL database relations from a Prolog
   program. Facilities are also provided for reflecting more complex
   @em{views} of the database relations as Prolog predicates. Such
   views can be constructed as conjunctions, disjunctions,
   projections, etc. of database relations, and may include SQL-like
   aggregation operations.").

:- doc(module,"The purpose of this library is to implement an
   instance of the generic concept of @concept{persistent predicates},
   where external @concept{relational databases} are used for storage
   (see the documentation of the @lib{persdb} library and
   @cite{radioweb-D3.1.M1-A1,radioweb-ta} for details).  To this end,
   this library exports SQL persistent versions of the
   @pred{assertz_fact/1}, @pred{retract_fact/1} and
   @pred{retractall_fact/1} builtin predicates.  Persistent predicates
   also allow @concept{concurrent updates} from several programs,
   since each update is atomic.

   The notion of persistence provides a very natural and transparent
   way to access database relations from a Prolog program. Stub
   definitions are provided for such predicates which access the
   database when the predicate is called (using the @lib{db_client}
   library).  A @concept{Prolog to SQL translator} is used to generate
   the required @concept{SQL} code dynamically (see library
   @lib{pl2sql}).

   This library also provides facilities for reflecting more complex
   @concept{views} of the database relations as Prolog
   predicates. Such views can be constructed as conjunctions,
   disjunctions, projections, etc. of database relations. Also,
   @concept{SQL}-like @concept{aggregation operations} are supported.

@section{Implementation of the Database Interface}

The architecture of the low-level implementation of the database
interface was defined with two goals in mind:

@begin{itemize}
@item to simplify the communication between the Prolog system and the
  relational database engines as much as possible, and

@item to give as much flexibility as possible to the overall system.
  This includes simultaneous access to several databases, allowing
  both the databases and clients to reside on the same physical
  machine or different machines, and allowing the clients to reside in
  Win95/NT or Unix machines.
@end{itemize}


In order to allow the flexibility mentioned above, a client-sever
architecture was chosen.  At the server side, a MySQL server connects
to the databases using the MySQL.  At the client side, a MySQL client
interface connects to this server.  The server daemon (mysqld) should
be running in the server machine; check your MySQL documentation on
how to do that.

After the connection is established a client can send commands to the
mediator server which will pass them to the corresponding database
server, and then the data will traverse in the opposite direction.
These messages include logging on and off from the database, sending
SQL queries, and receiving the responses.

The low level implementation of the current library is accomplished by
providing abstraction levels over the MySQL interface library. These
layers of abstraction implement the persistent predicate view, build
the appropriate commands for the database using a translator of Prolog
goals to SQL commands, issue such commands using the mediator
send/receive procedures, parse the responses, and present such
responses to the Prolog engine via backtracking.

@section{Example(s)}

@begin{verbatim}
@includeverbatim{examples/people.pl}
@end{verbatim}
   ").

%% ---------------------------------------------------------------------------

:- new_declaration(sql_persistent/3,yes).

:- doc(doinclude,sql_persistent/3).
:- decl sql_persistent(PrologPredTypes,TableAttributes,Keyword)
   => prologPredTypes * tableAttributes * persLocId

# "Declares the predicate corresponding to the main functor of
   @var{PrologPredTypes} as SQL persistent. @var{Keyword} is the
   @concept{name of a location} where the @concept{persistent storage}
   for the predicate is kept, which in this case must be an external
   relational database. The description of this database is given
   through the @pred{sql_persistent_location} predicate, which must
   contain a fact in which the first argument unifies with
   @var{Keyword}. @var{TableAttributes} provides the @concept{table
   name} and @concept{attributes} in the database corresponding
   respectively to the predicate name and arguments of the (virtual)
   Prolog predicate.

   Although a predicate may be persistent, other usual clauses can be
   defined in the source code. When querying a persistent predicate
   with non-persistent clauses, persistent and non-persisten clauses
   will be evaluated in turn; the order of evaluation is the usual
   Prolog order, considering that persistent clauses are defined in
   the program point where the @decl{sql_persistent/3} declaration is.

   @bf{Example:}

@begin{verbatim}
:- sql_persistent(product( integer,    integer, string, string ),
              product( quantity,   id,      name,   size   ),
              radiowebdb).

sql_persistent_location(radiowebdb,
   db('SQL Anywhere 5.0 Sample', user, pass,
      'r2d5.dia.fi.upm.es':2020)).
@end{verbatim}
".


:- meta_predicate make_sql_persistent(addmodule,?,?).
:- impl_defined(make_sql_persistent/3).

:- pred make_sql_persistent(PrologPredTypes,TableAttributes,Keyword)
   => prologPredTypes * tableAttributes * persLocId

# "Dynamic version of the @decl{sql_persistent/3} declaration.".

make_sql_persistent(PrologDef, Mod, SQLDef, DBId) :-
	PrologDef =.. [PrologName | Types],
	SQLDef    =.. [TableName  | ArgNames],
	functor(PrologDef, PrologName, Arity),
	assertz_fact(sql__relation(PrologName,Arity,TableName)),
	assert_args(Types,ArgNames,1,TableName),
	assertz_fact(sql_persistent(PrologName/Arity,DBId)),
	module_concat(Mod, PrologName, ModPrologName),
	functor(Consequence, PrologName, Arity),
	functor(Head, ModPrologName, Arity),
	equal_args(Head, Consequence, 1, Arity),
%	dynamic(PrologName/Arity),
	this_module(This),
	module_concat(This, db_call_db_atomic_goal(DBId, Consequence), Body),
	term_to_meta((Head :- Body), Clause),
	assertz(Clause).

equal_args(_Pred1, _Pred2, Index, Arity):- Index > Arity, !.
equal_args(Pred1, Pred2, Index, Arity) :-
	arg(Index, Pred1, Arg),
	arg(Index, Pred2, Arg),
	Index1 is Index + 1,
	equal_args(Pred1, Pred2, Index1, Arity).

assert_args([], [], _, _) :-
	!.
assert_args([Type|Ts], [ArgName|As], N, TableName) :-
	sqltype(Type),
	!,
	assertz_fact(sql__attribute(N, TableName, ArgName, Type)),
	N1 is N+1,
	assert_args(Ts, As, N1, TableName).
assert_args([T|_], [_|_], _, TableName) :-
	!,
	error_message("illegal SQL type ~w in predicate for ~w",[T, TableName]).
assert_args(_,_,_,TableName) :-
	error_message("arity mismatch in declaration for ~w",[TableName]).


%% ---------------------------------------------------------------------------

:- meta_predicate dbassertz_fact(addmodule,?).
:- impl_defined(dbassertz_fact/1).

:- pred dbassertz_fact(+Fact) :: fact

  # "Persistent extension of @pred{assertz_fact/1}: the current instance of
     @var{Fact} is interpreted as a fact (i.e., a relation tuple) and is
     added to the end of the definition of the corresponding predicate. If
     any integrity constraint violation is done (database stored
     predicates), an error will be displayed.  The predicate concerned must
     be statically (@decl{sql_persistent/3}) or dinamically
     (@pred{make_sql_persistent/3}) declared. Any uninstantiated variables in
     the @var{Fact} will be replaced by new, private variables.  @bf{Note}:
     @em{assertion of facts with uninstantiated variables not implemented at
     this time.}".

dbassertz_fact(Fact) :-
	functor(Fact,F,A),
	debug_message("fact to assert is ~w",[Fact]),
	init_sql_persdb,
        debug_message("checking if ~w/~w is persistent",[F,A]),
 	sql_persistent(F/A,DBId),
 	debug_message("persistent predicate found, location: ~w",[DBId]),
	debug_message("ready to call to the insertion compiler, with the fact ~w",[Fact]),
	pl2sqlInsert(Fact,SQLString),
	debug_message("SQL insertion sentence is ~s",[SQLString]),
%jcf% 	jcf_message(SQLString),
	sql_query(DBId,SQLString,ResultTerm),
	( ResultTerm='ok' ->
	      debug_message("Persistent fact inserted in the database",[])
	    ;
	      error_message("in insertion. Answer received is ~w",[ResultTerm])
	).

%% ---------------------------------------------------------------------------

:- meta_predicate dbretract_fact(addmodule,?).
:- impl_defined(dbretract_fact/1).

:- pred dbretract_fact(+Fact) :: fact

 # "Persistent extension of @pred{retract_fact/1}: deletes on
    backtracking all the facts which unify with @var{Fact}.  The
    predicate concerned must be statically (@decl{sql_persistent/3}) or
    dinamically (@pred{make_sql_persistent/3}) declared.".

dbretract_fact(Fact):-
        functor(Fact,F,A),
	debug_message("fact to retract is ~w",[Fact]),
	init_sql_persdb,
 	sql_persistent(F/A,DBId),
 	debug_message("persistent predicate found, location: ~w",[DBId]),
        debug_message("checking if ~w is a current fact",[Fact]),
        dbcurrent_fact(Fact), %% fails if Fact is not a current fact
	debug_message("~w found as a current fact",[Fact]),
	debug_message("calling mysql_delete, with the fact ~w",[Fact]),
%%  	           pl2sqlDelete(FactToRetract,SQLString),** we don't use
%%                    the SQL deletion compiler because SQL Anywhere doesn't
%%                    admit aliases in delete statements (SQL Anywhere 5.0
%%                    bug)
	mysql_delete(Fact,DBId),
	debug_message("ODBC deletion done. Persistent fact deleted from the database",[]).


%% ---------------------------------------------------------------------------

:- meta_predicate dbretractall_fact(addmodule,?).
:- impl_defined(dbretractall_fact/1).

:- pred dbretractall_fact(+Fact) :: fact

 # "Persistent extension of @pred{retractall_fact/1}: when called deletes
   all the facts which unify with @var{Fact}. The predicate concerned must
   be statically (@decl{sql_persistent/3}) or dinamically
   (@pred{make_sql_persistent/3}) declared.".

dbretractall_fact(Fact):-
%        asserta_fact(issue_debug_messages(persdb_mysql)),
	message("fact to retract is ~w",[Fact]),
	init_sql_persdb,
	functor(Fact, F, A),
 	sql_persistent(F/A,DBId),
%% %%%%%% TO SEE: maybe there are two predicates with the same name
%%                and located in different places (different databases, files,
%%                persistent and non-persistent predicates,...)
 	debug_message("persistent predicate found, location: ~w",[DBId]),
	debug_message("calling mysql_delete, with the fact ~w",[Fact]),
	mysql_delete(Fact,DBId),
	debug_message("ODBC deletion done. Persistent fact deleted from the database",[]), fail.
dbretractall_fact(_).

%% TO SEE: keep mysql_delete as an internal predicate???
:- doc(hide,mysql_delete/2).

:- pred mysql_delete(+Fact,+DBId) :: atomicgoal * dbconnection

# "Internal predicate,used to delete the fact @var{Fact} from an external
  database. Not meant to be called directly by users.  @var{Fact} must be a
  call to a persistent predicate which resides in database @var{DbId}.  The
  current solution uses @lib{pl2sql}. @pred{pl2sqlstring} provides a SQL
  select sentence, which is used to create a view and delete all its
  elements. Hence, user permission to create a view is needed.".

mysql_delete(Fact,DBId):-
%%%%%% TO MAKE: think about deleting complex predicates
	copy_term(Fact,FactDB),
	pl2sqlDelete(FactDB,SQLString),
	debug_message("Translated select sentence is ~s",[SQLString]),
%jcf% 	jcf_message(SQLString),
%% Permission to create a view and to delete PL_TMP_TO_RETRACT is needed
	%% first we delete PL_TMP_TO_RETRACT, if it exists
	sql_query(DBId,SQLString,ResultTerm),
	!,
	( ResultTerm='ok' ->
	    debug_message("deleted",[])
	  ;
	    error_message("delete failed. Answer received is ~w",[ResultTerm])
	).

mysql_delete(_Fact,_DBId):- %% maybe not needed
	error_message(" Deletion couldn't be done ",[]),
	fail.

%% ---------------------------------------------------------------------------

:- meta_predicate dbcurrent_fact(addmodule,?).
:- impl_defined(dbcurrent_fact/1).

:- pred dbcurrent_fact(+Fact) :: fact

# "Persistent extension of @pred{current_fact/1}: the fact @var{Fact}
   exists in the current database.  The predicate concerned must be
   declared @decl{sql_persistent/3}.  Provides on backtracking all the
   facts (tuples) which unify with @var{Fact}.".

dbcurrent_fact(Fact) :-
	debug_message("fact to call is ~w",[Fact]),
	init_sql_persdb,
        functor(Fact, F, A),
%%%%%% TO MAKE: see if always 'user'. Better 'multifile'?
 	debug_message("checking if ~w/~w is persistent",[F,A]),	
	sql_persistent(F/A,DBId),
 	debug_message("persistent predicate found, location: ~w",[DBId]),
	debug_message("calling current_fact(~w) in database",[Fact]),
	db_call_db_atomic_goal(DBId,Fact).

%% ---------------------------------------------------------------------------
%% Calling complex goals
%% ---------------------------------------------------------------------------

:- meta_predicate dbfindall(?,?,goal,?).

:- pred dbfindall(+DBId,+Pattern,+ComplexGoal,-Results)
   :: dbconnection * projterm * querybody * list

   # "Similar to @pred{findall/3}, but @var{Goal} is executed in
     database @var{DBId}. Certain restrictions and extensions apply to
     both @var{Pattern} and @var{ComplexGoal} stemming from the Prolog
     to SQL translation involved (see the corresponding type
     definitions for details).".

dbfindall(_, Pattern, ComplexGoal, Results) :-
	findall(Pattern, ComplexGoal, Results).

% SINCE MySQL DOES NOT SUPPORT NESTED SELECTS THIS CODE WILL NOT
% WORK. USE findall/3 INSTEAD.
%
%dbfindall(DBId,Pattern,ComplexGoal,Results) :-
%	debug_message("projecting    ~w    onto    ~w    in ~w",
%	       [ComplexGoal,Pattern,DBId]),
%	init_sql_persdb,
%	sql_persistent(_,DBId),
%	!,
%	db_query(DBId,Pattern,ComplexGoal,ResultsList),
%	functor(Pattern,PF,_),
%	map_pattern_functor(ResultsList,PF,Results).
%
%dbfindall(DBId,_Pattern,ComplexGoal,_Results) :-
%	error_message("in database identifier ~w in dbfindall/4 for ~w",
%	       [DBId,ComplexGoal]),
%	fail.
%
%map_pattern_functor([],_PF,[]).
%map_pattern_functor([IResult|IResults],PF,[OResult|OResults]) :-
%	OResult =.. [PF|IResult],
%	map_pattern_functor(IResults,PF,OResults).


%% ---------------------------------------------------------------------------

%% :- meta_predicate dbcall(?,goal).

:- pred dbcall(+DBId,+ComplexGoal) :: dbconnection * querybody

# "Internal predicate, used by the transformed versions of the
   persistent predicates. Not meant to be called directly by users. It
   is exported by the library so that it can be used by the
   transformed versions of the persistent predicates in the modules in
   which they reside. Sends @var{ComplexGoal} to database @var{DBId} for
   evaluation. @var{ComplexGoal} must be a call to a persistent predicate
   which resides in database @var{DBId}.".

dbcall(DBId,ComplexGoal) :-
	debug_message("calling ~w in ~w",[ComplexGoal,DBId]),
	init_sql_persdb,
	sql_persistent(_,DBId),
	!,
	varset(ComplexGoal,Vars),
	Pattern =.. [foo|Vars],
	debug_message("Calling db_query_one_tuple !~n",[]),
	db_query_one_tuple(DBId,Pattern,ComplexGoal,ResultsList),
	ResultsList = tup(Vars).

dbcall(DBId,ComplexGoal) :-
	error_message("in database identifier ~w in dbcall/2 for ~w",
	       [DBId,ComplexGoal]),
	fail.

%% ---------------------------------------------------------------------------

:- doc(doinclude,db_query/4).

:- pred db_query(+DBId,+ProjTerm,+Goal,ResultTerm)
	:: dbconnection * projterm * querybody * tuple

# "@var{ResultTerm} contains all the @concept{tuples} which are the
   response from database @var{DBId} to the Prolog query @var{Goal},
   projected onto @var{ProjTerm}. Uses @pred{pl2sqlstring/3} for the
   @concept{Prolog to SQL translation} and @pred{sql_query/3} for
   posing the actual query.".

db_query(DBId,ProjTerm,Goal,ResultTerm) :-
	copy_term(pair(ProjTerm,Goal),pair(DBProjTerm,DBGoal)),
	pl2sqlstring(DBProjTerm,DBGoal,SQLStringQuery),
	debug_message("sending SQL query ""~s"" ",[SQLStringQuery]),
%jcf% 	jcf_message(SQLStringQuery),
%%	sql_query(DBId, SQLStringQuery, table(_,ResultsList) ),
	sql_query(DBId, SQLStringQuery, t(ResultTerm) ),
	debug_message("result is ~w",[ResultTerm]).

%% ---------------------------------------------------------------------------
:- doc(doinclude,sql_query/3).

:- pred sql_query(+DBId,+SQLString,AnswerTableTerm)
	:: dbconnection * sqlstring * answertableterm

# "@var{ResultTerm} is the response from database @var{DBId} to the
   @concept{SQL query} in @var{SQLString} to database
   @var{DBId}. @var{AnswerTableTerm} can express a set of tuples, an error
   answer or a 'ok' response (see @decl{answertableterm/1} for details).
   At the moment, @pred{sql_query/3} log in and out for each query. This
   should be changed to log in only the first time and log out on exit
   and/or via a timer in the standard way.".
%%%%%% TO MAKE: change this

sql_query(DBId,SQLString,ResultTerm):-
	sql_persistent_location(DBId,db(Id,User,Passwd,Socket)),
	mysql_connect(Socket,Id,User,Passwd,DbConnection),
	( mysql_query(DbConnection,SQLString,ResultTerm) ->
           true
        ; atom_codes(SQLStringAtom,SQLString),
	  atom_concat('in answer to ',SQLStringAtom,Error),
	  ResultTerm = err(Error)
	),
	mysql_disconnect(DbConnection).

%% ---------------------------------------------------------------------------

:- doc(doinclude,db_query_one_tuple/4).

:- pred db_query_one_tuple(+DBId,+ProjTerm,+Goal,ResultTerm)
	:: dbconnection * projterm * querybody * answertupleterm

# "@var{ResultTerm} is one of the @concept{tuples} which are the response
   from database @var{DBId} to the Prolog query @var{Goal}, projected onto
   @var{ProjTerm}. Uses @pred{pl2sqlstring/3} for the @concept{Prolog to
   SQL translation} and @pred{sql_query_one_tuple/3} for posing the actual
   query. After last tuple has been reached, a null tuple is unified with
   ResultTerm, and the connection to the database finishes.".

db_query_one_tuple(DBId,ProjTerm,Goal,tup(ResultTerm)) :-
	copy_term(pair(ProjTerm,Goal),pair(DBProjTerm,DBGoal)),
	debug_message("calling pl2sqlstring",[]),
	pl2sqlstring(DBProjTerm,DBGoal,SQLStringQuery),!,
	debug_message("sending SQL query ""~s"" ",[SQLStringQuery]),
%jcf% 	jcf_message(SQLStringQuery),
	sql_query_one_tuple(DBId, SQLStringQuery, ResultTerm),
	debug_message("result is ~w",[ResultTerm]).

%% ---------------------------------------------------------------------------
:- doc(doinclude,sql_query_one_tuple/3).

:- pred sql_query_one_tuple(+DBId,+SQLString,ResultTuple)
	:: dbconnection * sqlstring * tuple

# "@var{ResultTuple} contains an element from the set of tuples which
   represents the response in @var{DBId} to the @concept{SQL query}
   @var{SQLString}. If the connection is kept, succesive calls return
   consecutive tuples, until the last tuple is reached. Then a null tuple
   is unified with @var{ResultTuple} and the connection is finished (calls
   to @pred{mysql_disconnect/1}).".

sql_query_one_tuple(DBId,SQLString,ResultTerm) :-
	sql_persistent_location(DBId,db(Id,User,Passwd,Socket)),
	mysql_connect(Socket,Id,User,Passwd,DbConnection),
	mysql_query_one_tuple(DbConnection,SQLString,DbQueryConnection),
	Try = fetch_nd(DbConnection,DbQueryConnection,ResultTerm),
	OnCut = fetch_nd_cut(DbConnection,DbQueryConnection),
	OnFail = fetch_nd_fail(DbConnection,DbQueryConnection),
	det_try(Try, OnCut, OnFail).

fetch_nd(DbConnection,DbQueryConnection,ResultTerm) :-
	mysql_fetch(DbQueryConnection,tup(ResultTerm0)), \+ ResultTerm0 = [],
	( ResultTerm0 = ResultTerm ;
	  fetch_nd(DbConnection,DbQueryConnection,ResultTerm) ).

fetch_nd_cut(DbConnection,DbQueryConnection) :-
	mysql_free_query_connection(DbQueryConnection),
        mysql_disconnect(DbConnection).

fetch_nd_fail(DbConnection,DbQueryConnection) :-
	mysql_free_query_connection(DbQueryConnection),
        mysql_disconnect(DbConnection).

%% ---------------------------------------------------------------------------
%% Internal, for calling atomic goals only
%% ---------------------------------------------------------------------------

:- doc(hide,db_call_db_atomic_goal/2).

%% :- meta_predicate db_call_db_atomic_goal(?,goal).

:- pred db_call_db_atomic_goal(+DBId,+Goal) :: dbconnection * atomicgoal

# "Internal predicate, used by the transformed versions of the
   persistent predicates. Not meant to be called directly by users. It
   is exported by the library so that it can be used by the
   transformed versions of the persistent predicates in the modules in
   which they reside. Sends @var{Goal} to database @var{DBId} for
   evaluation. @var{Goal} must be a call to a persistent predicate
   which resides in database @var{DBId}.".

db_call_db_atomic_goal(DBId,Goal) :-
	debug_message("calling ~w in ~w",[Goal,DBId]),
	init_sql_persdb,
	functor(Goal,F,A),
	debug_message("checking if ~w/~w, stored in ~w, is persistent",[F,A,DBId]),
	(  sql_persistent(F/A, DBId)
	->
	   debug_message("Calling db_query_one_tuple for ~w/~w ~n",[F,A]),
	   db_query_one_tuple(DBId,Goal,Goal,ResultsList),
	   debug_message("Return from db_query_one_tuple with ~w ~n",[ResultsList]),
	   Goal =.. [_|OneResult],
	   ResultsList = tup(OneResult)
	   % member(OneResult,ResultsList)
	;  error_message("~w/~w is not a DB persistent predicate",[F,A]),
	   fail).

:- data db_started/0.

:- pred init_sql_persdb

# "Internal predicate, used to transform predicates statically declared as
   persistent (see @decl{sql_persistent/3}) into real persistent predicates. ".

init_sql_persdb:-
	db_started, !.
init_sql_persdb:-
	initialize_db,
	assertz_fact(db_started).

initialize_db:-
 	'$is_sql_persistent'(PrologDef,SQLDef,DBId),
 	make_sql_persistent(PrologDef,SQLDef,DBId),
	fail.
initialize_db.

%% ---------------------------------------------------------------------------

:- pred sql_get_tables(+Location,-Tables) :: database_desc * list(atm)

# "@var{Tables} contains the tables available in @var{Location}.".

:- pred sql_get_tables(+DbConnection,-Tables) :: dbconnection * list(atm)

# "@var{Tables} contains the tables available in @var{DbConnection}.".

sql_get_tables(db(Id,User,Passwd,Socket),TablesList) :- !,
	mysql_connect(Socket,Id,User,Passwd,DbConnection),
	mysql_get_tables(DbConnection,TablesList),
	mysql_disconnect(DbConnection).

sql_get_tables(DBId, TablesList):-
	sql_persistent_location(DBId, Location),
	sql_get_tables(Location, TablesList).

:- pred sql_table_types(+Location,+Table,-AttrTypes)
	:: database_desc * atm * list

# "@var{AttrTypes} are the attributes and types of @var{Table} in
   @var{Location}.".

:- pred sql_table_types(+DbConnection,+Table,-AttrTypes)
	:: dbconnection * atm * list

# "@var{AttrTypes} are the attributes and types of @var{Table} in
   @var{DbConnection}.".

sql_table_types(db(Id,User,Passwd,Socket),TableName,AttTypesList):-
	mysql_connect(Socket,Id,User,Passwd,DbConnection),
	mysql_table_types(DbConnection,TableName, AttTypesNativeList),
	filter_types(AttTypesNativeList,AttTypesList),
	mysql_disconnect(DbConnection).	

sql_table_types(DBId, TableName, AttTypesList):-
	sql_persistent_location(DBId,Location),
	sql_table_types(Location,TableName,AttTypesList).

%% Obtains the sqltype versions of the Native SQL System types retrieved
filter_types([],[]).
filter_types([[NativeId, NativeType]|NativeRest],[[NativeId,Type]|Rest]):-
	accepted_type(NativeType, Type),
	filter_types(NativeRest, Rest).
