:- module(mysql_client,
	[
	    mysql_connect/5,
	       dbconnection/1,
	    mysql_query/3,
	    mysql_query_one_tuple/3,
	       dbqueryconnection/1,
            mysql_free_query_connection/1,
	    mysql_fetch/2,
	    mysql_get_tables/2,
	    mysql_table_types/3,
	    mysql_disconnect/1
	],[assertions,regtypes,basicmodes,fsyntax,foreign_interface]).

:- use_module(library(persdb_mysql/db_client_types)).

% ----------------------------------------------------------------------------

:- doc(title,"Low-level socket interface to SQL/ODBC databases").

:- doc(author,"Jose F. Morales").

:- doc(module,"This library provides a low-level interface to MySQL
   using the MySQL C API and the Ciao foreign interface to C.").

% ----------------------------------------------------------------------------

:- regtype dbconnection(H) # "@var{H} a unique identifier of a database
   session connection.".

dbconnection := ~address.

:- doc(dbconnection/1,"@includedef{dbconnection/1}").

:- regtype dbqueryconnection(H) # "@var{H} is a unique identifier of a
   query answer in a database session connection.".

dbqueryconnection := ~address.

:- doc(dbqueryconnection/1,"@includedef{dbqueryconnection/1}").

% ----------------------------------------------------------------------------

:- true pred 
        init(in(N), go(DbConnection)) :: 
              int * address + 
        (foreign(mysql_init), returns(DbConnection)).
:- impl_defined(init/2).

:- true pred connect(
        in(DbConnection0), 
        in(Host),
        in(User),
        in(Passwd),
        in(DbName),
        in(Port), 
        in(X),
        in(Y),
        go(DbConnection)) :: 
        address * atm * atm * atm * atm * int * address * int * address + 
        (foreign(mysql_real_connect), returns(DbConnection)).
:- impl_defined(connect/9).

:- true pred disconnect(in(DbConnection0)) :: address + foreign(mysql_close).
:- impl_defined(disconnect/1).

:- true pred num_rows(in(DbQueryConnection), go(Num)) :: address * int + (foreign, returns(Num)).
:- impl_defined(num_rows/2).

:- true pred num_fields(in(DbQueryConnection), go(Num)) :: address * int + (foreign, returns(Num)).
:- impl_defined(num_fields/2).

:- true pred free_result(in(DbQueryConnection)) :: address + foreign(mysql_free_result).
:- impl_defined(free_result/1).

:- true pred query(in(DbQueryConnection), in(Query), go(R)) :: address * string * int + (foreign(mysql_query), returns(R)).
:- impl_defined(query/3).

:- true pred use_result(in(DbConnection), go(DbQueryConnection)) :: address * address + (foreign(mysql_use_result), returns(DbQueryConnection)).
:- impl_defined(use_result/2).

:- true pred fetch_row(in(DbQueryConnection), go(Row)) :: address * address + (foreign(mysql_fetch_row), returns(Row)).
:- impl_defined(fetch_row/2).

:- true pred fetch_fields(in(DbQueryConnection), go(Fields)) :: address * address + (foreign(mysql_fetch_fields), returns(Fields)).
:- impl_defined(fetch_fields/2).

:- true pred error_string(in(DbConnection), go(Error)) :: address * atm + (foreign(mysql_error), do_not_free(Error), returns(Error)).
:- impl_defined(error_string/2).

:- true pred nth_string(in(N), in(Array), go(String)) :: int * address * string + (foreign, do_not_free(String), returns(String)).
:- impl_defined(nth_string/3).

:- true pred nth_field_type(in(N), in(Fields), go(Type)) :: int * address * atm + (foreign, do_not_free(Type), returns(Type)).
:- impl_defined(nth_field_type/3).

:- use_foreign_source(library(persdb_mysql/mysql_client)).
:- use_foreign_library(mysqlclient).

%% Fixed path to the MySQL client libraries
%% (Automatically generated in core/Manifest/core.hooks.pl)
:- include(library(persdb_mysql/linker_opts_auto)).

% ----------------------------------------------------------------------------

mysql_connect(Host:Port, DbName, User, Passwd, DbConnection) :-
	init(0, DbConnection0),
        null(Null),
        connect(DbConnection0, Host, User, Passwd, DbName, Port, Null, 0, 
                DbConnection),
	\+ null(DbConnection),
        !.
mysql_connect(_Host:_Port, _DbName, _User, _Passwd, _Connection) :-
        throw(error('could not start connection', mysql_connect)), fail.

mysql_disconnect(DbConnection) :- disconnect(DbConnection).

mysql_query(DbConnection, Sentence) := AnswerTerm :-
	Res = ~mysql_query_one_tuple(DbConnection, Sentence),
	( Res = ~null ->
            AnswerTerm = ~mysql_result(DbConnection)
	; ( AnswerTerm = t(~mysql_query_2(Res)) ->
	      true
	  ; AnswerTerm = ~mysql_result(DbConnection)
	  ),
	  free_result(Res)
	).

mysql_result(DbConnection) := AnswerTerm :-
	Error = ~error_string(DbConnection),
	( Error = '' ->
	    AnswerTerm = ok
	; AnswerTerm = err(Error)
	).

mysql_query_2(Res) := [Y|Ys] :-
	tup(Y) = ~mysql_fetch(Res), !,
	Ys = ~mysql_query_2(Res).
mysql_query_2(_) := [].

mysql_query_one_tuple(DbConnection, Sentence) := DbQueryConnection :-
	( ~query(DbConnection, Sentence) = 0 ->
	    DbQueryConnection = ~use_result(DbConnection)
	; DbQueryConnection = ~null
	).
 
mysql_free_query_connection(DbQueryConnection) :-
	free_result(DbQueryConnection).

mysql_fetch(DbQueryConnection) := Tuple :-
	( \+ DbQueryConnection = ~null ->
	    Row = ~fetch_row(DbQueryConnection),
	    Fields = ~fetch_fields(DbQueryConnection)
	; Row = ~null
	),
	( \+ Row = ~null ->
	    FieldsCount = ~num_fields(DbQueryConnection),
	    Tuple = tup(~row_to_tuple(FieldsCount, Fields, Row))
	; DbQueryConnection = ~null,
          Tuple = []
	).

row_to_tuple(FieldsCount, Fields, Row) := ~row_to_tuple_2(0, FieldsCount, Fields, Row).

string_to_t(num, String) := Number :- !, number_codes(Number, String).
string_to_t(_, String) := Atom :- atom_codes(Atom, String).

row_to_tuple_2(X, FieldsCount, _, _) := [] :- X >= FieldsCount, !.
row_to_tuple_2(X, FieldsCount, Fields, Row) := [~string_to_t(~nth_field_type(X, Fields), ~nth_string(X, Row))|~row_to_tuple_2(X + 1, FieldsCount, Fields, Row)].

mysql_get_tables(DbConnection) := ~flatten(Tables) :-
	t(Tables) = ~mysql_query(DbConnection, "SHOW TABLES").
	
mysql_table_types(DbConnection, TableName) := ~get_types(AttTypesList) :-
	atom_codes(TableName, Name),
	t(AttTypesList) = ~mysql_query(DbConnection, "SHOW COLUMNS FROM "||Name).

get_types([]) := [].
get_types([[Id,Type|_]|Xs]) := [[Id,~get_type(Type)]|~get_types(Xs)].
get_types(X) := X.

get_type(X) := ~get_type_2(~atom_codes(X)).

get_type_2("varchar"||_) := string.
get_type_2("text"||_) := string.
get_type_2("int"||_) := int.
get_type_2("float"||_) := flt.

flatten(A) := B :-
	flatten_2(A, [], B).

flatten_2([], Ys) := Ys :- !.
flatten_2([X|Xs], Cs) := As :- !,
	As = ~flatten_2(X, Bs),
	Bs = ~flatten_2(Xs, Cs).
flatten_2(X, Cs) := [X|Cs].
