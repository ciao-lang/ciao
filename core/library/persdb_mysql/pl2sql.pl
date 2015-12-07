:- module(pl2sql,
	[
	    pl2sqlstring/3,
	    querybody/1,
	    projterm/1,
	    sqlstring/1,
	    pl2sqlterm/3,
	    sqlterm2string/2
	], [dcg]).

:- use_module(library(persdb_sql_common/sqltypes), 
	  [
%jcf%	      sybase2sqltype/2, sybase2sqltypes_list/2, sybasetype/1,
	      get_type/2, type_union/3, type_compatible/2
%jcf%	      , accepted_type/2
          ]).
:- reexport(library(persdb_sql_common/sqltypes), [sqltype/1]).


:- multifile [sql__relation/3, sql__attribute/4].
:- data [sql__relation/3, sql__attribute/4].


:- use_package(assertions).
:- use_package(regtypes).
:- use_package(basicmodes).
:- use_module(library(iso_misc)).
:- use_module(library(lists), [append/3]).
:- use_module(library(aggregates)).
:- use_module(library(messages), [error_message/2]).

:- doc(bug, "Need to separate db predicate names by module.").

:- doc(title, "Prolog to SQL translator").

:- doc(author,"Christoph Draxler").
:- doc(author,"Manuel Hermenegildo (adaptation)").
:- doc(author,"Ignacio Caballero (adaptation)").

:- doc(module, "This library performs translation of Prolog queries
   into @concept{SQL}. The code is an adaptation for Ciao of the
   @concept{Prolog to SQL compiler} written by Christoph Draxler, CIS
   Centre for Information and Speech Processing,
   Ludwig-Maximilians-University Munich,
   @tt{draxler@@cis.uni-muenchen.de}, Version 1.1. Many thanks to
   Christoph for allowing us to include this adaptation of his code
   with Ciao.

   The translator needs to know the correspondence between Prolog
   predicates and the @concept{SQL tables} in the database. To this
   end this module exports two multifile predicates, @pred{sql__relation/3}
   and @pred{sql__attribute/4}. See the description of these predicates for
   details on how such correspondance is specified.

   The main entry points to the translator are @pred{pl2sqlstring/3}
   and @pred{pl2sqlterm/3}. Details on the types of queries allowed
   can be found in the description of these predicates.

   @bf{Example:} the following program would print out a term
   representing the SQL query  corresponding to the 
   given Prolog query:

@begin{verbatim}
@includeverbatim{examples/small_examples/pl2sql_example.pl}
@end{verbatim}

   @bf{Note}: while the translator can be used directly in programs,
   it is more convenient to use a higher-level abstraction:
   @concept{persistent predicates} (implemented in the @lib{persdb}
   library). The notion of persistent predicates provides a completely
   transparent interface between Prolog and relational databases. When
   using this library, the Prolog to SQL translation is called
   automatically as needed.

").

% ----------------------------------------------------------------------------

:- pred sql__relation(PredName, Arity, TableName) :: atm * int * atm

   # "This predicate, together with @pred{sql__attribute/4}, defines the
      correspondence between Prolog predicates and the @concept{SQL
      tables} in the database. These two relations constitute an
      extensible meta-database which maps @concept{Prolog predicate
      names} to @concept{SQL table names}, and @concept{Prolog
      predicate argument positions} to @concept{SQL attributes}.

      @var{PredName} is the chosen Prolog name for an SQL
      table. @var{Arity} is the number of arguments of the predicate.
      @var{TableName} is the name of the SQL table in the Database
      Management System.".

% ---------------------------------------------------------------------------

:- pred sql__attribute(ANumber, TblName, AName, AType) :: int * atm * atm * sqltype 

   # "This predicate maps the argument positions of a Prolog predicate
      to the SQL attributes of its corresponding table. The types of
      the arguments need to be specified, and this information is used
      for consistency checking during the translation and for output
      formatting.  A minimal type system is provided to this end.  The
      allowable types are given by @pred{sqltype/1}.

      @var{ANumber} is the argument number in the Prolog
      relation. @var{TblName} is the name of the SQL table in the
      Database Management System. @var{AName} is the name of the
      corresponding attribute in the table. @var{AType} is the
      (translator) data type of the attribute.".

% ---------------------------------------------------------------------------

:- pred pl2sqlstring(+ProjectionTerm, +DatabaseGoal, -SQLQueryString) 
   :: projterm * querybody * sqlstring

   # "This is the top level predicate which translates complex Prolog
      goals into the corresponding SQL code. 

      The query code is prepared in such a way that the result is
      projected onto the term @var{ProjectionTerm} (also in a similar
      way to the first argument of @pred{setof/3})). See the predicate
      @pred{translate_projection/3} for restrictions on this term.

      @var{SQLQueryString} contains the code of the @concept{SQL
      query}, ready to be sent to an @concept{SQL server}.".

pl2sqlstring(ProjectionTerm, DatabaseGoal, SQLQueryString) :-
	pl2sqlterm(ProjectionTerm, DatabaseGoal, SQLQueryTerm),
	sqlterm2string(SQLQueryTerm, SQLQueryString),
	!.

:- regtype querybody(DBGoal) # "@var{DBGoal} is a database query goal.".

querybody(DBGoal) :- 
	callable(DBGoal).

:- doc(querybody/1, "@var{DBGoal} is a goal meant to be executed in
   the external database. It can be a complex term containing
   @concept{conjunctions}, @concept{disjunctions}, and
   @concept{negations}, of:

   @begin{itemize}

   @item @concept{Atomic goals}, which must have been defined via
   @pred{sql__relation/3} and @pred{sql__attribute/4} and reside in the (same)
   database. Their arguments must be either ground or free
   variables. If they are ground, they must be bound to constants of
   the type declared for that argument. If an argument is a free
   variable, it may @em{share} with (i.e., be the same variable as)
   other free variables in other goal arguments.

   @item @concept{Database comparison goals}, whose main functor must
   be a @concept{database comparison operator} (see
   @lib{pl2sql}:@pred{comparison/2}) and whose arguments must be
   @em{database arithmetic expressions}.

   @item @concept{Database calls to is/2}. The left side of such a
   call may be either unbound, in which case it is bound to the result
   of evaluating the right side, or bound in which case an equality
   condition is tested. The right side must be a @em{database
   arithmetic expression}.

   @end{itemize}

   The binding of variables follows Prolog rules: 

   @begin{itemize} 

   @item variables are bound by positive base goals and on the left
   side of the @pred{is/2} predicate.

   @item Comparison operations, negated goals, and right sides of the
   @pred{is/2} predicate do not return variable bindings and may even
   require all arguments to be bound for a safe evaluation.

   @end{itemize}

   @concept{Database arithmetic expressions} may contain:

   @begin{itemize}

   @item Numeric constants (i.e., integers, reals, etc.).

   @item Bound variables, i.e., variables which will be bound during
   execution through occurrence within a positive database goal, or 
   by a preceding arithmetic function.

   @item @concept{Database arithmetic functions}, which are a subset
   of those typically accepted within @pred{is/2} (see
   @lib{pl2sql}:@pred{arithmetic_functor/2}).

   @item @concept{Database aggregation functions}, each of which has
   two arguments: a variable indicating the argument over which the
   function is to be computed, and a goal argument which must contain
   in at least one argument position the variable (e.g.
   @tt{avg(Seats, plane(Type, Seats))}). The goal argument may only be a
   conjunction of (positive or negative) base goals. See
   @lib{pl2sql}:@pred{aggregate_functor/2} for the admissible
   aggregate functions.

   @end{itemize}

   In addition, variables @cindex{existential quantification} can be
   existentially quantified using @pred{^/2} (in a similar way to how
   it is done in @pred{setof/3}). 

   Note that it is assumed that the arithmetic operators in Prolog and
   SQL are the same, i.e., @tt{+} is addition in Prolog and in SQL,
   etc. 

").

:- regtype projterm(DBProjTerm) # "@var{DBProjTerm} is a database
   projection term.".

projterm(DBProjTerm) :- 
	term(DBProjTerm).

:- doc(projterm/1, "@var{DBProjTerm} is a term onto which the
   result of a database query code is (in a similar way to the first
   argument of @pred{setof/3})). 

   A @var{ProjectionTerm} must meet the following restrictions:

   @begin{itemize}

   @item The functor of @var{ProjectionTerm} may not be one of the
         built-in predicates, i.e. ',', ';', etc. are not allowed.

   @item Only variables and constants are allowed as arguments, i.e.,
         no structured terms may appear.

   @end{itemize}").

:- regtype sqlstring(S) # "@var{S} is a string containing SQL code.".

sqlstring(S) :- 
	string(S).

:- doc(sqlstring/1, "@includedef{sqlstring/1}").

% ---------------------------------------------------------------------------

:- pred pl2sqlterm(+ProjectionTerm, +DatabaseGoal, -SQLQueryTerm) 
   :: projterm * querybody * list(sqlterm)

   # "Similar to @pred{pl2sqlstring/3} except that @var{SQLQueryTerm} is
      a representation of the SQL query as a Prolog term.".

pl2sqlterm(ProjectionTerm, DatabaseGoal, SQLQueryTerm) :-
	init_gensym(variable),
	init_gensym(rel),
	tokenize_term(DatabaseGoal, TokenDatabaseGoal),
	tokenize_term(ProjectionTerm, TokenProjectionTerm),
	convert_to_disj_norm_form(TokenDatabaseGoal, Disjunction),
	query_generation(Disjunction, TokenProjectionTerm, SQLQueryTerm),
	!.
pl2sqlterm(ProjectionTerm, DatabaseGoal, _SQLQueryTerm) :-
	error_message("SQL translation failed for ~q / ~q", [ProjectionTerm, DatabaseGoal]),
	fail.

% ---------------------------------------------------------------------------

:- pred convert_to_disj_norm_form(+Goal, -Disjunction) 

# "Turns the original goal into disjunctive normal form by computing a
    set of flat conjunctions and collecting them in a list, whose
    elements are assumed to be joined by disjuntions.".

convert_to_disj_norm_form(Goal, Disjunction) :-
   findall(Conjunction, linearize(Goal, Conjunction), Disjunction).

% ---------------------------------------------------------------------------

:- pred linearize(+Goal, -ConjunctionList)

# "Returns a conjunction of base goals for a complex disjunctive or
   conjunctive goal. Yields several solutions upon backtracking for
   disjunctive goals.".

linearize(((A, B), C), (LinA, (LinB, LinC))) :-
	% --- transform left-linear to right-linear conjunction (',' is associative)
	linearize(A, LinA),
	linearize(B, LinB),
	linearize(C, LinC).
linearize((A, B), (LinA, LinB)) :-
	A \= (_, _),
	% --- make sure A is not a conjunction ------------------------------------
	linearize(A, LinA),
	linearize(B, LinB).
linearize((A;_B), LinA) :-
	linearize(A, LinA).
linearize((_A;B), LinB) :-
	linearize(B, LinB).
linearize(\+(A), \+ (LinA)) :-
	linearize(A, LinA).
linearize(Var^A, Var^LinA) :-
	linearize(A, LinA).
linearize(A, A) :-
	A \= (_, _),
	A \= (_;_),
	A \= _^_,
	A \= \+(_).

% ---------------------------------------------------------------------------

:- pred tokenize_term(+Term, -TokenizedTerm)

# "If Term is a

   @begin{itemize}

   @item @em{variable}, then this variable is instantiated with a
   unique identifier of the form '$var$'(VarId), and
   @var{TokenizedTerm} is bound to the same term '$var$'(VarId).

   @item @em{constant}, then @var{TokenizedTerm} is bound to
   '$const$'(Term) (where Term is a Prolog string).

   @item @em{string}, then @var{TokenizedTerm} is bound to
   '$const$'(Term) (where Term is also a Prolog string).

   @item @em{complex term}, then the term is decomposed, its arguments
   are tokenized, and @var{TokenizedTerm} is bound to the result of
   the composition of the original functor and the tokenized
   arguments.

   @end{itemize}".

tokenize_term('$var$'(VarId), '$var$'(VarId)) :-
	var(VarId),
	% --- uninstantiated variable: instantiate it with unique identifier.
	gensym(variable, VarId).
tokenize_term('$var$'(VarId), '$var$'(VarId)) :-
	nonvar(VarId),
	%% with cut,  '$const$'(var22) is not instantiated. ^^
	!. 
tokenize_term(Constant, '$const$'(RealConstant)) :-
	nonvar(Constant),
	functor(Constant, _, 0),
	atm(Constant),
	!,
	atom_codes(Constant, RealConstant).
%% Constant is an atom, and RealConstant is a string
tokenize_term(Constant, '$const$'(Constant)) :-
	nonvar(Constant),
	functor(Constant, _, 0).
%% Other constants : numbers, ...
tokenize_term(Constant, '$const$'(Constant)) :-
	nonvar(Constant),
	string(Constant),
	!.
tokenize_term(Term, TokenizedTerm) :-
	nonvar(Term),
	Term \= '$var$'(_),
	Term \= '$const$'(_),
	Term =.. [Functor|Arguments],
	Arguments \== [],
	tokenize_arguments(Arguments, TokenArguments),
	TokenizedTerm =.. [Functor|TokenArguments].

% ---------------------------------------------------------------------------

:- pred tokenize_arguments(Arguments, TokenizedArguments)

# "Organizes tokenization of arguments by traversing the argument list
   and calling tokenize_term for each element of the list.".

tokenize_arguments([], []).
tokenize_arguments([FirstArg|RestArgs], [TokFirstArg|TokRestArgs]) :-
	tokenize_term(FirstArg, TokFirstArg),
	tokenize_arguments(RestArgs, TokRestArgs).

% ---------------------------------------------------------------------------

:- doc(doinclude, query_generation/3).

:- pred query_generation(+ListOfConjunctions, +ProjectionTerm, -ListOfQueries)

# "For each Conjunction in @var{ListOfConjunctions}, translate the pair
   @tt{(ProjectionTerm, Conjunction)} to an SQL query and connect each
   such query through a @concept{UNION-operator} to result in the
   @var{ListOfQueries}.

   A Conjunction consists of positive or negative subgoals. Each
   subgoal is translated as follows:

   @begin{itemize} 

   @item the @concept{functor of a goal} that is not a comparison
         operation is translated to a @concept{relation name} with a
         @concept{range variable},

   @item @concept{negated goals} are translated to @concept{NOT
         EXISTS-subqueries} with @concept{* projection},

   @item @concept{comparison operations} are translated to comparison
         operations in the @concept{WHERE-clause},

   @item @concept{aggregate function terms} are translated to
         @concept{aggregate function (sub)queries}.

   @end{itemize}

   The arguments of a goal are translated as follows:

   @begin{itemize} 

   @item @var{variables of a goal} are translated to @concept{qualified
         attributes},

   @item @concept{variables occurring in several goals} are translated
         to equality comparisons (equi join) in the WHERE-clause,
         @cindex{equi join in the WHERE-clause}

   @item @concept{constant arguments} are translated to
         @concept{equality comparisons in the WHERE-clause}.

   @end{itemize}

   Arithmetic functions are treated specially
   (@pred{translate_arithmetic_function/5}). See also
   @pred{querybody/1} for details on the syntax accepted and
   restrictions.

".

query_generation([], _, []).
query_generation([Conjunction|Conjunctions], ProjectionTerm, [Query|Queries]) :-
	check_projection_term_variables(ProjectionTerm, InitDict),
	check_translate_conjunction(Conjunction, SQLFrom, SQLWhere, InitDict, Dict),
	check_translate_projection(ProjectionTerm, Dict, SQLSelect),
	Query = query(SQLSelect, SQLFrom, SQLWhere),
	check_query_generation(Conjunctions, ProjectionTerm, Queries).

check_projection_term_variables(ProjectionTerm, InitDict) :-
	projection_term_variables(ProjectionTerm, InitDict),
	!.
check_projection_term_variables(ProjectionTerm, _InitDict) :-
	error_message("in projection term ~w", [ProjectionTerm]),
	fail.

check_translate_conjunction(Conjunction, SQLFrom, SQLWhere, InitDict, Dict) :-
	translate_conjunction(Conjunction, SQLFrom, SQLWhere, InitDict, Dict),
	!.
check_translate_conjunction(Conjunction, _SQLFrom, _SQLWhere, _InitDict, _Dict) :-
	error_message("in conjunction ~w", [Conjunction]),
	fail.

check_translate_projection(ProjectionTerm, Dict, SQLSelect) :-
	translate_projection(ProjectionTerm, Dict, SQLSelect),
	!.
check_translate_projection(ProjectionTerm, _Dict, _SQLSelect) :-
	error_message("in projection term ~w", [ProjectionTerm]),
	fail.

check_query_generation(Conjunctions, ProjectionTerm, Queries) :-
	query_generation(Conjunctions, ProjectionTerm, Queries),
	!.
check_query_generation(Conjunctions, ProjectionTerm, _Queries) :-
	error_message("in ~w or ~w", [ProjectionTerm, Conjunctions]),
	fail.

% ---------------------------------------------------------------------------

:- pred projection_term_variables(ProjectionTerm, Dict)

# "Extracts all variables from the @var{ProjectionTerm} and places
   them into the @var{Dict} as a dict/4 term with their Identifier, a
   non instantiated RangeVar and Attribute argument, and the keyword
   existential for the type of quantification.".

projection_term_variables('$const(_)$', []).
projection_term_variables('$var$'(VarId), [dict(VarId, _, _, _, existential)]).
projection_term_variables(ProjectionTerm, ProjectionTermVariables) :-
	ProjectionTerm =.. [Functor|ProjectionTermList],
	\+ (Functor = '$var$'),
	\+ (ProjectionTermList = []),
	projection_list_vars(ProjectionTermList, ProjectionTermVariables).

projection_list_vars([], []).
projection_list_vars(['$var$'(VarId)|RestArgs], [dict(VarId, _, _, _, existential)|RestVars]) :-
	projection_list_vars(RestArgs, RestVars).
projection_list_vars(['$const$'(_)|RestArgs], Vars) :-
	projection_list_vars(RestArgs, Vars).

% ---------------------------------------------------------------------------

:- pred translate_projection(ProjectionTerm, Dict, SelectList)

# "Translates the projection term.".

translate_projection('$var$'(VarId), Dict, SelectList) :-
	projection_arguments(['$var$'(VarId)], SelectList, Dict).
translate_projection('$const$'(Const), _, ['$const$'(Const)]).
translate_projection(ProjectionTerm, Dict, SelectList) :-
	ProjectionTerm =.. [Functor|Arguments],
	\+ (Functor = '$var$'),
	\+ (Functor = '$const$'),
	\+ (Arguments = []),
	projection_arguments(Arguments, SelectList, Dict).

projection_arguments([], [], _).
projection_arguments([Arg|RestArgs], [Att|RestAtts], Dict) :-
	retrieve_argument(Arg, Att, Dict),
	projection_arguments(RestArgs, RestAtts, Dict).


% ---------------------------------------------------------------------------

:- pred retrieve_argument(Argument, SQLAttribute, Dictionary) 

# "Retrieves the mapping of an argument to the appropriate SQL
  construct, i.e.,

  @begin{itemize}
  @item qualified attribute names for variables in base goals,

  @item arithmetic expressions for variables in arithmetic goals,

  @item constant values for constants.
  @end{itemize}
". 

retrieve_argument('$var$'(VarId), Attribute, Dict) :-
	lookup(VarId, Dict, TableName, AttName, _),
	( TableName = is ->
	    Attribute = AttName
	; Attribute = att(TableName, AttName)
	).
retrieve_argument('$const$'(Constant), '$const$'(Constant), _).

% ---------------------------------------------------------------------------

:- doc(doinclude, translate_conjunction/5).

:- pred translate_conjunction(Conjunction, SQLFrom, SQLWhere, Dict, NewDict)

# "Translates a conjunction of goals (represented as a list of goals
   preceeded by existentially quantified variables) to
   @concept{FROM-clauses} and @concept{WHERE-clauses} of an SQL query.
   A dictionary containing the associated SQL table and attribute
   names is built up as an accumulator pair (arguments @var{Dict} and
   @var{NewDict}).".


translate_conjunction('$var$'(VarId)^Goal, SQLFrom, SQLWhere, Dict, NewDict) :-
	% --- add info on existentially quantified variables to dictionary here --
	add_to_dictionary(VarId, _, _, _, existential, Dict, TmpDict),
	translate_conjunction(Goal, SQLFrom, SQLWhere, TmpDict, NewDict).
translate_conjunction(Goal, SQLFrom, SQLWhere, Dict, NewDict) :-
	Goal \= (_, _),
	check_translate_goal(Goal, SQLFrom, SQLWhere, Dict, NewDict).
translate_conjunction((Goal, Conjunction), SQLFrom, SQLWhere, Dict, NewDict) :-
	check_translate_goal(Goal, FromBegin, WhereBegin, Dict, TmpDict),
	translate_conjunction(Conjunction, FromRest, WhereRest, TmpDict, NewDict),
	append(FromBegin, FromRest, SQLFrom),
	append(WhereBegin, WhereRest, SQLWhere).

check_translate_goal(Goal, SQLFrom, SQLWhere, Dict, NewDict) :-
	translate_goal(Goal, SQLFrom, SQLWhere, Dict, NewDict),
	!.
check_translate_goal(Goal, _SQLFrom, _SQLWhere, _Dict, _NewDict) :-
	error_message("in database goal ~w", [Goal]),
	fail.

% ---------------------------------------------------------------------------

:- doc(doinclude, translate_goal/5).

:- pred translate_goal(Goal, SQLFrom, SQLWhere, Dict, NewDict) 

# "Translates:

   @begin{itemize} 

   @item a @concept{positive database goal} to the associated FROM-
         and WHERE clause of an SQL query,

   @item a @concept{negated database goal} to a negated existential
         subquery,

   @item an @concept{arithmetic goal} to an arithmetic expression or
         an aggregate function query,

   @item a @concept{comparison goal} to a comparison expression, and 

   @item a @concept{negated comparison goal} to a comparison
         expression with the opposite comparison operator.

   @end{itemize} ".

translate_goal(SimpleGoal, [SQLFrom], SQLWhere, Dict, NewDict) :-
	% --- positive goal binds variables - bindings are held in the dictionary -
	functor(SimpleGoal, Functor, Arity),
	translate_functor(Functor, Arity, SQLFrom),
	SimpleGoal =.. [Functor|Arguments],
	translate_arguments(Arguments, SQLFrom, 1, SQLWhere, Dict, NewDict).
translate_goal(Result is Expression, [], SQLWhere, Dict, NewDict) :-
	translate_arithmetic_function(Result, Expression, SQLWhere, Dict, NewDict).
translate_goal(\+(NegatedGoals), [], SQLNegatedSubquery, Dict, Dict) :-
	% --- negated goals do not bind variables - hence Dict is returned unchanged
	functor(NegatedGoals, Functor, _),
	\+(comparison(Functor, _)),
	translate_conjunction(NegatedGoals, SQLFrom, SQLWhere, Dict, _),
	SQLNegatedSubquery = [negated_existential_subquery([*], SQLFrom, SQLWhere)].
translate_goal(\+(ComparisonGoal), [], SQLCompOp, Dict, Dict) :-
	% --- comparison operations do not bind vars - Dict is returned unchanged
	ComparisonGoal =.. [ComparisonOperator, LeftArg, RightArg],
	comparison(ComparisonOperator, SQLOperator),
	negated_comparison(SQLOperator, SQLNegOperator),
	translate_comparison(LeftArg, RightArg, SQLNegOperator, Dict, SQLCompOp).
translate_goal(ComparisonGoal, [], SQLCompOp, Dict, Dict) :-
	% --- comparison operations do not bind vars - Dict is returned unchanged 
	ComparisonGoal =.. [ComparisonOperator, LeftArg, RightArg],
	comparison(ComparisonOperator, SQLOperator),
	translate_comparison(LeftArg, RightArg, SQLOperator, Dict, SQLCompOp).

% ---------------------------------------------------------------------------

:- doc(doinclude, translate_arithmetic_function/5).

:- pred translate_arithmetic_function(Result, Expression, SQLWhere, Dict, NewDict)

# "Arithmetic functions (left side of is/2 operator is bound to value
   of expression on right side) may be called with either:

   @begin{itemize} 
   @item @var{Result} unbound: then @var{Result} is bound to the
   value of the evaluation of @var{Expression},

   @item @var{Result} bound: then an equality condition is returned
   between the value of @var{Result} and the value of the evaluation
   of @var{Expression}.

   Only the equality test shows up in the WHERE clause of an SQLquery.
   @end{itemize}
".

translate_arithmetic_function('$var$'(VarId), Expression, [], Dict, NewDict) :-
	% assigment of value of arithmetic expression to variable - does not
	% show up in WHERE-part, but expression corresponding to
	% variable must be stored in Dict for projection translation
	evaluable_expression(Expression, Dict, ArithExpression, Type),
	add_to_dictionary(VarId, is, ArithExpression, Type, all, Dict, NewDict).
translate_arithmetic_function('$var$'(VarId), Expression, ArithComparison, Dict, Dict) :-
	% --- test if left side evaluates to right side: return equality comparison
	% Left side consists of qualified attribute, i.e. range variable must not be
	% arithmetic operator is/2 

	lookup(VarId, Dict, PrevRangeVar, PrevAtt, PrevType),
	\+ (PrevRangeVar = is),

	% test whether type of attribute is numeric - if not, there's no sense in 
	% continuing the translation
	check_type_compatible(PrevType, num),
	
	evaluable_expression(Expression, Dict, ArithExpression, ExprType),
	check_type_compatible(ExprType, num),
	ArithComparison = [comp(att(PrevRangeVar, PrevAtt), '=', ArithExpression)].
translate_arithmetic_function('$var$'(VarId), Expression, ArithComparison, Dict, Dict) :-
	% --- test whether left side evals to right side: return equality comparison
	% Left side consists of arithmetic expression, i.e. VarId is stored in Dict
	% as belonging to arithmetic expression which is expressed as 
	% RangeVar-argument of lookup returning is/2. Type information is implicit 
	% through the is/2 functor.
	lookup(VarId, Dict, is, LeftExpr, Type),
	check_type_compatible(Type, num),
	evaluable_expression(Expression, Dict, RightExpr, ExprType),
	check_type_compatible(ExprType, num),
	ArithComparison = [comp(LeftExpr, '=', RightExpr)].
translate_arithmetic_function('$const$'(Constant), Expression, ArithComparison, Dict, Dict) :-
	% --- is/2 used to test whether left side evaluates to right side ---------
	get_type('$const$'(Constant), ConstantType),
	check_type_compatible(ConstantType, num),
	evaluable_expression(Expression, Dict, ArithExpression, ExprType),
	check_type_compatible(ExprType, num),
	ArithComparison = [comp('$const$'(Constant), '=', ArithExpression)].

% ---------------------------------------------------------------------------

:- doc(doinclude, translate_comparison/5).

:- pred translate_comparison(LeftArg, RightArg, CompOp, Dict, SQLComparison) 

# "Translates the left and right arguments of a comparison term into
   the appropriate comparison operation in SQL. The result type of
   each argument expression is checked for type compatibility.".

translate_comparison(LeftArg, RightArg, CompOp, Dict, Comparison) :-
	evaluable_expression(LeftArg, Dict, LeftTerm, LeftArgType),
	evaluable_expression(RightArg, Dict, RightTerm, RightArgType),
	check_type_compatible(LeftArgType, RightArgType),
	Comparison = [comp(LeftTerm, CompOp, RightTerm)].

% ---------------------------------------------------------------------------

:- pred translate_functor(Functor, QualifiedTableName) 

# "Translate_functor searches for the matching relation table name for
   a given functor and creates a unique range variable to result in a
   unique qualified relation table name.".

translate_functor(Functor, Arity, rel(TableName, RangeVariable)) :-
	sql__relation(Functor, Arity, TableName),
	gensym(rel, RangeVariable).

% ---------------------------------------------------------------------------

:- pred translate_arguments(Arguments, RelTable, ArgPos, Conditions, Dict)

# "Organizes the translation of term arguments. One term argument
   after the other is taken from the list of term arguments until the
   list is exhausted.".

translate_arguments([], _, _, [], Dict, Dict).
translate_arguments([Arg|Args], SQLTable, Position, SQLWhere, Dict, NewDict) :-
	translate_argument(Arg, SQLTable, Position, Where, Dict, TmpDict),
	NewPosition is Position + 1,
	translate_arguments(Args, SQLTable, NewPosition, RestWhere, TmpDict, NewDict),
	append(Where, RestWhere, SQLWhere).

% ---------------------------------------------------------------------------

:- pred translate_argument(Argument, RelTable, Position, Condition, Dict)

# "The first occurrence of a variable leads to its associated SQL
   attribute information to be recorded in the Dict. Any further
   occurrence creates an equi-join condition between the current
   attribute and the previously recorded attribute.  Constant
   arguments always translate to equality comparisons between an
   attribute and the constant value.".

translate_argument('$var$'(VarId), rel(SQLTable, RangeVar), Position, [], Dict, NewDict) :-
	sql__attribute(Position, SQLTable, Attribute, Type),
	add_to_dictionary(VarId, RangeVar, Attribute, Type, all, Dict, NewDict).
translate_argument('$var$'(VarId), rel(SQLTable, RangeVar), Position, AttComparison, Dict, Dict) :-
	% --- Var occurred previously - retrieve first occurrence data from dict
	lookup(VarId, Dict, PrevRangeVar, PrevAtt, PrevType),
	sql__attribute(Position, SQLTable, Attribute, Type),
	check_type_compatible(PrevType, Type),
	AttComparison = [comp(att(RangeVar, Attribute), =, att(PrevRangeVar, PrevAtt))].
translate_argument('$const$'(Constant), rel(SQLTable, RangeVar), Position, ConstComparison, Dict, Dict) :-
	% --- Equality comparison of constant value and attribute in table ------
	sql__attribute(Position, SQLTable, Attribute, Type),
	get_type('$const$'(Constant), ConstType),
	check_type_compatible(ConstType, Type),
	ConstComparison = [comp(att(RangeVar, Attribute), =, '$const$'(Constant))].

% ---------------------------------------------------------------------------

:- pred lookup(Key, Dict, Value).

lookup(VarId, Dict, RangeVar, Attribute, Type) :-
	member(dict(VarId, RangeVar, Attribute, Type, Quant), Dict),
	( Quant = all ->
	    true
	; nonvar(RangeVar),
	  nonvar(Attribute)
	).

% ---------------------------------------------------------------------------

:- pred add_to_dictionary(Key, RangeVar, Attribute, Quantifier, Dict, NewDict).

add_to_dictionary(Key, RangeVar, Attribute, Type, _, Dict, Dict) :-
	member(dict(Key, RangeVar, Attribute, Type, existential), Dict).
add_to_dictionary(Key, RangeVar, Attribute, Type, Quantifier, Dict, NewDict) :-
	\+(member(dict(Key, _, _, _, _), Dict)),
	NewDict = [dict(Key, RangeVar, Attribute, Type, Quantifier)|Dict].

% ---------------------------------------------------------------------------

:- doc(doinclude, aggregate_function/3).

:- pred aggregate_function(AggregateFunctionTerm, Dict, AggregateFunctionQuery)

# "Supports the Prolog aggregate function terms listed in
   @pred{aggregate_functor/2} within arithmetic expressions.
   Aggregate functions are translated to the corresponding SQL
   built-in aggregate functions.".

aggregate_function(AggregateFunctionTerm, Dict, AggregateFunctionExpression) :-
	AggregateFunctionTerm =..[AggFunctor, AggVar, AggGoal],
	aggregate_functor(AggFunctor, SQLFunction),
	conjunction(AggGoal, AggConjunction),
	aggregate_query_generation(SQLFunction, AggVar, AggConjunction, Dict, AggregateFunctionExpression).

conjunction(Goal, Conjunction) :-
	convert_to_disj_norm_form(Goal, [Conjunction]).

% ---------------------------------------------------------------------------

:- pred aggregate_query_generation(Function, FunctionVariable, AggGoal, Dict,
   AggregateQuery) 

# "Compiles the function variable (representing the attribute over
   which the aggregate function is to be computed) and the aggregate
   goal (representing the selection and join conditions for the
   computation of the aggregate function) to an SQL aggregate function
   subquery.".

aggregate_query_generation(count, '$const$'('*'), AggGoal, Dict, AggregateQuery) :-
	translate_conjunction(AggGoal, SQLFrom, SQLWhere, Dict, _TmpDict),
	% ATTENTION! It is assumed that in count(*) aggregate query terms there 
	% cannot be free variables because '*' stands for "all arguments"
	AggregateQuery = agg_query(_Function, (count, ['$const$'(*)]), SQLFrom, SQLWhere, []).


aggregate_query_generation(Function, FunctionVariable, AggGoal, Dict, AggregateQuery) :-
	translate_conjunction(AggGoal, SQLFrom, SQLWhere, Dict, TmpDict),
	% --- only vars occurring in the aggregate goal are relevant to the 
	% translation of the function variable and the free variables in the goal.
	% Thus subtract from TmpDict all entries of Dict
	set_difference(TmpDict, Dict, AggDict),
	translate_projection(FunctionVariable, AggDict, SQLSelect),
	translate_grouping(FunctionVariable, AggDict, SQLGroup),
	AggregateQuery = agg_query(Function, SQLSelect, SQLFrom, SQLWhere, SQLGroup).

% ---------------------------------------------------------------------------

:- pred translate_grouping(FunctionVariable, Dict, SQLGroup) 

# "Finds the free variables in the aggregate function term and collects their
   corresponding SQL qualified attributes in the SQLGroup list.".

translate_grouping(FunctionVariable, Dict, SQLGroup) :-
	free_vars(FunctionVariable, Dict, FreeVariables),
	translate_free_vars(FreeVariables, SQLGroup).

% ---------------------------------------------------------------------------

:- pred free_vars(FunctionVariable, Dict, FreeVarList)

# "A Variable is free if it neither occurs as the FunctionVariable,
   nor is stored as existentially quantified (through ^/2 in the
   original goal) in the dictionary. FreeVars contains for each
   variable the relevant attribute and relation information contained
   in the dictionary.".

free_vars(FunctionVariable, Dict, FreeVarList) :-
	projection_term_variables(FunctionVariable, FunctionVariableList),
	findall((Var, Table, Attribute),
		(member(dict(Var, Table, Attribute, _Type, all), Dict),
		 \+(member(dict(Var, _, _, _, _), FunctionVariableList))
		),
		FreeVarList).

% ---------------------------------------------------------------------------

:- pred function_variable_list(FunctionVariable, FunctionVariableList)

# "Extracts the list of variables which occur in the function variable term.

   @bf{Note}: @var{FunctionVariable} may only contain one single variable.".
 

function_variable_list('$var$'(VarId), [VarId]).


% ---------------------------------------------------------------------------

:- pred translate_free_vars(FreeVars, SQLGroup)

# "Translates dictionary information on free variables to SQLGroup of
   aggregate function query.".

translate_free_vars([], []).
translate_free_vars([(_VarId, Table, Attribute)|FreeVars], [att(Table, Attribute)|SQLGroups]) :-
	translate_free_vars(FreeVars, SQLGroups).

% ---------------------------------------------------------------------------

:- pred evaluable_expression(ExpressionTerm, Dictionary, Expression, Type)

# "Constructs SQL arithmetic expressions with qualified attribute
   names from the Prolog arithmetic expression term and the
   information stored in the dictionary.

   The type of an evaluable function is returned in the argument @var{Type}.

   The dictionary is not changed because it is used for lookup only.".
 
evaluable_expression(AggregateFunctionTerm, Dictionary, AggregateFunctionExpression, num) :-
	aggregate_function(AggregateFunctionTerm, Dictionary, AggregateFunctionExpression).

%% MH Unified all operators here. Also using type unions (previously was 
%% hardwired to 'num'). 
evaluable_expression(ArithExpr, Dictionary, SQLArithExpr, EType) :-
	ArithExpr =.. [PrologOp, LeftExp, RightExp],
	arithmetic_functor(PrologOp, SQLOp),
	evaluable_expression(LeftExp, Dictionary, LeftAr, LType),
	evaluable_expression(RightExp, Dictionary, RightAr, RType),
	check_type_union(LType, RType, EType),
	SQLArithExpr =.. [SQLOp, LeftAr, RightAr].

evaluable_expression('$var$'(VarId), Dictionary, att(RangeVar, Attribute), Type) :-
	lookup(VarId, Dictionary, RangeVar, Attribute, Type),
	RangeVar \== is.
evaluable_expression('$var$'(VarId), Dictionary, ArithmeticExpression, Type) :-
	lookup(VarId, Dictionary, is, ArithmeticExpression, Type).
evaluable_expression('$const$'(Const), _, '$const$'(Const), ConstType) :-
	get_type('$const$'(Const), ConstType).
%%%%%% TO SEE: strings are affected by the "atom type to string type" change?

% ----------------------------------------------------------------------------
% Pretty printing of queries
% ----------------------------------------------------------------------------

queries_dstring([Query]) --> !,
	query_dstring(Query),
	";".
queries_dstring([Query|Queries]) -->
	query_dstring(Query),
	" UNION ",
	queries_dstring(Queries).

query_dstring(query([agg_query(Function, Select, From, Where, Group)], _, _)) -->
	% --- ugly rule here: aggregate function only in SELECT Part of query ----
	!,
	query_dstring(agg_query(Function, Select, From, Where, Group)).
query_dstring(query(Select, From, Where)) -->
	clause3_dstring('SELECT', Select, ','),
	clause3_dstring(' FROM', From, ','),
	clause3_dstring(' WHERE', Where, 'AND').
query_dstring(agg_query(Function, Select, From, Where, Group)) -->
	clause4_dstring('SELECT', Function, Select, ','),
	clause3_dstring(' FROM', From, ','),
	clause3_dstring(' WHERE', Where, 'AND'),
	clause3_dstring(' GROUP BY', Group, ',').
query_dstring(negated_existential_subquery(Select, From, Where)) -->
	"NOT EXISTS (",
	query_dstring(query(Select, From, Where)),
	")".

clause4_dstring(Keyword, Function, [Column], Separator) -->
	atom_dstring(Keyword),
	" ",
	atom_dstring(Function),
	"(",
	clause2_dstring([Column], Separator),
	")".

clause3_dstring(_Keyword, [], _) --> [].
clause3_dstring(Keyword, [Column|RestColumns], Separator) -->
	atom_dstring(Keyword),
	" ",
	clause2_dstring([Column|RestColumns], Separator).

clause2_dstring([Item], _) -->
	column_dstring(Item).
clause2_dstring([Item, NextItem|RestItems], Separator) -->
	column_dstring(Item),
	" ",
	atom_dstring(Separator),
	" ",
	clause2_dstring([NextItem|RestItems], Separator).

column_dstring('*') -->
	"*".
column_dstring(att(rel1, Attribute)) --> !, % HACK FOR MySQL!!!
	atom_dstring(Attribute).
column_dstring(att(RangeVar, Attribute)) -->
	atom_dstring(RangeVar),
	".",
	atom_dstring(Attribute).
column_dstring(rel(Relation, rel1)) --> !, % HACK FOR MySQL!!!
	atom_dstring(Relation).
column_dstring(rel(Relation, RangeVar)) -->
	atom_dstring(Relation),
	" ",
	atom_dstring(RangeVar).
column_dstring('$const$'(String)) -->
	{ get_type('$const$'(String), string) }, !,
	"'",
	string_dstring(String),
	"'".
column_dstring('$const$'(Number)) -->
	{ get_type('$const$'(Number), NumType),
	  check_type_compatible(NumType, num)
	},
	atom_dstring(Number).
column_dstring(comp(LeftArg, Operator, RightArg)) -->
	column_dstring(LeftArg),
	" ",
	atom_dstring(Operator),
	" ",
	column_dstring(RightArg).
column_dstring(LeftExpr * RightExpr) -->
	column_dstring(LeftExpr),
	" * ",
	column_dstring(RightExpr).
column_dstring(LeftExpr / RightExpr) -->
	column_dstring(LeftExpr),
	" / ",
	column_dstring(RightExpr).
column_dstring(LeftExpr + RightExpr) -->
	column_dstring(LeftExpr),
	" + ",
	column_dstring(RightExpr).
column_dstring(LeftExpr - RightExpr) -->
	column_dstring(LeftExpr),
	" - ",
	column_dstring(RightExpr).
column_dstring(agg_query(Function, Select, From, Where, Group)) -->
	"(",
	query_dstring(agg_query(Function, Select, From, Where, Group)),
	")".
column_dstring(negated_existential_subquery(Select, From, Where)) -->
	query_dstring(negated_existential_subquery(Select, From, Where)).

string_dstring(S, Xs, Ys) :-
	append(S, Ys, Xs).

atom_dstring(A) -->
	{ atom(A) -> atom_codes(A, S) ; number_codes(A, S) },
	string_dstring(S).

% ----------------------------------------------------------------------------
% Conversion of SQL term to string
% ----------------------------------------------------------------------------

:- pred sqlterm2string(+Queries, -QueryString) :: list(sqlterm) * sqlstring

# "@var{QueryString} is a string representation of the list of
   queries in Prolog-term format in @var{Queries}.".

%% Original code fails on some SQLQueryTerms for which queries_dstring/1 succeeds!
%% Doing this serious kludge instead for now:
sqlterm2string(SQLQueryTerm, SQLQueryString) :-
	queries_dstring(SQLQueryTerm, SQLQueryString, []).

% ----------------------------------------------------------------------------
% ciao version for init_gensym and gensym
% ----------------------------------------------------------------------------

:- pred concat_pred_num(Name, Number, Conc_Pred).

concat_pred_num(Name, Number, Conc_Pred) :-
	 name(Name, String_1),
	 name(Number, String_2),
	 append(String_1, String_2, Concatenation),
	 name(Conc_Pred, Concatenation).

:- pred variable/1 # "Stores the current variable number.".

:- data variable/1.

:- pred rel/1 # "Stores the current relation number.".

:- data rel/1.

:- pred init_gensym(Root).

init_gensym(Root) :-
	 New =.. [Root, 0],
	 set_fact(New).

:- pred gensym(Root, Symbol).

gensym(Root, Symbol) :-
	 var(Symbol),
	 Old =.. [Root, Counter],
	 Old,
	 NewCounter is Counter+1,
	 concat_pred_num(Root, NewCounter, Symbol),
	 New =.. [Root, NewCounter],
	 set_fact(New).

% ----------------------------------------------------------------------------
% auxiliary predicates (most changed to use built-ins...)
% ----------------------------------------------------------------------------

:- pred set_difference(SetA, SetB, Difference) 

# "@var{Difference} = @var{SetA} - @var{SetB}.".

set_difference([], _, []).
set_difference([Element|RestSet], Set, [Element|RestDifference]) :-
	\+(member(Element, Set)),
	set_difference(RestSet, Set, RestDifference).
set_difference([Element|RestSet], Set, RestDifference) :-
	member(Element, Set),
	set_difference(RestSet, Set, RestDifference).

% ----------------------------------------------------------------------------
% Mapping of Prolog operators to SQL operators
% ----------------------------------------------------------------------------

:- doc(doinclude, comparison/2).

:- pred comparison(PrologOperator, SQLOperator) :: atm * atm

# "Defines the mapping between Prolog operators and SQL operators:
@includedef{comparison/2}".

comparison(=, =).
comparison(<, <).
comparison(>, >).
comparison(@<, <).
comparison(@>, >).

:- doc(doinclude, negated_comparison/2).

:- pred negated_comparison(PrologOperator, SQLOperator) :: atm * atm

# "Defines the mapping between Prolog operators and the complementary
   SQL operators: 
@includedef{negated_comparison/2}".

negated_comparison(=, '<>').
negated_comparison(\==, =).
negated_comparison(>, =<).
negated_comparison(=<, >).
negated_comparison(<, >=).
negated_comparison(>=, <).

:- doc(doinclude, arithmetic_functor/2).

:- pred arithmetic_functor(PrologFunctor, SQLFunction) :: atm * atm

# "Defines the admissible arithmetic functions on the Prolog side and
   their correspondence on the SQL side:
@includedef{arithmetic_functor/2}".

arithmetic_functor(+, +).
arithmetic_functor(-, -).
arithmetic_functor(*, *).
arithmetic_functor(/, /).

:- doc(doinclude, aggregate_functor/2).

:- pred aggregate_functor(PrologFunctor, SQLFunction) :: atm * atm

# "Defines the admissible aggregate functions on the Prolog side and
   their correspondence on the SQL side:
@includedef{aggregate_functor/2}".

aggregate_functor(avg, 'AVG').
aggregate_functor(min, 'MIN').
aggregate_functor(max, 'MAX').
aggregate_functor(sum, 'SUM').
aggregate_functor(count, 'COUNT').

% ----------------------------------------------------------------------------
% Type system checking
% ----------------------------------------------------------------------------

check_type_union(TypeA, TypeB, TypeC) :- 
	type_union(TypeA, TypeB, TypeC),
	!.
check_type_union(TypeA, TypeB, _TypeC) :- 
	error_message("incompatible types ~w, ~w", [TypeA, TypeB]),
	fail.

check_type_compatible(TypeA, TypeB) :- 
	type_compatible(TypeA, TypeB),
	!.
check_type_compatible(TypeA, TypeB) :- 
	error_message("incompatible types ~w, ~w", [TypeA, TypeB]),
	fail.
