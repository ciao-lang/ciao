:- module(sqltypes,
	  [
	      sqltype/1, accepted_type/2,
	      get_type/2, 
	      type_compatible/2, type_union/3,
	      sybasetype/1, sybase2sqltypes_list/2, sybase2sqltype/2,
	      postgrestype/1, postgres2sqltypes_list/2, postgres2sqltype/2
	  ],
	  [assertions,regtypes,dcg,isomodes]).


% ----------------------------------------------------------------------------
% Type system
% ----------------------------------------------------------------------------

:- pred type_union(TypeA,TypeB,Union) :: sqltype * sqltype * sqltype

# "@var{Union} is the union type of @var{TypeA} and @var{TypeB}.".

type_union(TypeA,TypeA,TypeA).
type_union(TypeA,TypeB,TypeA) :- 
	subtype(TypeB,TypeA).
type_union(TypeA,TypeB,TypeB) :- 
	subtype(TypeA,TypeB).
type_union(TypeA,TypeB,TypeC) :- 
	subtype(TypeA,TypeC),
	subtype(TypeB,TypeC).


:- pred type_compatible(TypeA,TypeB) :: sqltype * sqltype

# "Checks if @var{TypeA} and @var{TypeB} are compatible types, i.e.,
   they are the same or one is a subtype of the other.".

type_compatible(Type,Type):-
   sqltype(Type).
type_compatible(SubType,Type):-
   subtype(SubType,Type).
type_compatible(Type,SubType):-
   subtype(SubType,Type).


:- pred subtype(SubType,SuperType) :: sqltype * sqltype

#  "Simple type hierarchy checking.".

subtype(SubType,SuperType):-
   is_subtype(SubType,SuperType).
subtype(SubType,SuperType):-
   is_subtype(SubType,InterType),
   subtype(InterType,SuperType).

:- pred accepted_type(SystemType, NativeType) :: sybasetype * sqltype

# "For the moment, tests wether the @var{SystemType} received is a sybase
   or a postgres type (in the future other systems should be supported) and
   obtains its equivalent @var{NativeType} sqltype.".

accepted_type(NativeType, NativeType) :-
	sqltype(NativeType), !.
accepted_type(SystemType, NativeType) :-
	sybasetype(SystemType), !,
	sybase2sqltype(SystemType, NativeType).
accepted_type(SystemType, NativeType) :-
	postgrestype(SystemType), !,
	postgres2sqltype(SystemType, NativeType).
%accepted_type(SystemType, NativeType) :-
%	oracletype(SystemType), !,
%	oracle2sqltype(SystemType, NativeType).
%
% And so on, for every SQL type system

:- regtype sqltype(Type) 

# "@var{Type} is an SQL data type supported by the translator.".

%% Should be ordered from most concrete to most general.
sqltype(int).      %% integer
sqltype(flt).      %% float
sqltype(num).      %% float or integer
sqltype(string).   %% char, varchar, long varchar
sqltype(date).     %% date
sqltype(time).     %% time
sqltype(datetime). %% datetime (timestamp)

:- doc(sqltype/1,"@includedef{sqltype/1}

   These types have the same meaning as the corresponding standard types
   in the @lib{basictypes} library.").

:- pred is_subtype(SubType,SuperType) :: sqltype * sqltype

# "Simple type hierarchy for numeric types.".

is_subtype(int,num).
is_subtype(flt,num).


:- pred get_type(+Constant,Type) :: term * sqltype

# "Prolog implementation-specific definition of type retrievals. CIAO 
   Prolog version given here (ISO).".

get_type('$const$'(Constant),num):-
   number(Constant).
get_type('$const$'(Constant),flt):-
   float(Constant).
get_type('$const$'(Constant),int):-
   integer(Constant).
get_type('$const$'(Constant),string):-
   atom(Constant).
get_type('$const$'(Constant),string):-
   string(Constant).


% ----------------------------------------------------------------------------
% Some common ODBC Drivers' SQL types (translation to our defined sql types)
% ----------------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% Sybase SQL Anywhere 5.0 %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(sybasetype/1,"SQL datatypes supported by Sybase for which a
   translation is defined:

   @includedef{sybasetype/1}").

:- regtype sybasetype(Type) 

# "@var{Type} is an SQL data type supported by Sybase.". 

sybasetype(integer).
sybasetype(numeric).
sybasetype(float).
sybasetype(double).
sybasetype(date).
sybasetype(char).
sybasetype(varchar).
sybasetype('long varchar').
sybasetype(binary).
sybasetype('long binary').
sybasetype(timestamp).
sybasetype(time).
sybasetype(tinyint).

:- pred sybase2sqltype(SybaseType,SQLType) => sybasetype * sqltype

# " @var{SybaseType} is a Sybase SQL type name, and @var{SQLType} is its
    equivalent SQL-type name in CIAO.".

sybase2sqltype(integer,int):-!.
sybase2sqltype(numeric, num):-!.
sybase2sqltype(float, flt):-!.
sybase2sqltype(double, flt):-!.
sybase2sqltype(date, date):-!.
sybase2sqltype(timestamp, datetime):-!.
sybase2sqltype(time, time):-!.
sybase2sqltype(tinyint, int):-!.
sybase2sqltype(_T,string). %% [long] binary, char, [long] varchar, 

:- pred sybase2sqltypes_list(SybaseTypesList, SQLTypesList) => list * list

# " @var{SybaseTypesList} is a list of Sybase SQL
    types. @var{PrologTypesList} contains their equivalent SQL-type
    names in CIAO.".

sybase2sqltypes_list([],[]).
sybase2sqltypes_list([SybaseType|SybaseTypes], [PrologType|PrologTypes]):-
	sybase2sqltype(SybaseType, PrologType),
	sybase2sqltypes_list(SybaseTypes, PrologTypes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% postgreSQL 6.4 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(postgrestype/1,"SQL datatypes supported by postgreSQL for which a
   translation is defined:

   @includedef{postgrestype/1}").

:- regtype postgrestype(Type) 

# "@var{Type} is an SQL data type supported by postgres.". 

postgrestype(int2).
postgrestype(int4).
postgrestype(int8).
postgrestype(float4).
postgrestype(float8).
postgrestype(date).
postgrestype(timestamp).
postgrestype(time).
postgrestype(char).
postgrestype(varchar).
postgrestype(text).
postgrestype(bool).
%% other postgres types, not contempled now: geometric, money, user-defined

:- pred postgres2sqltype(PostgresType,SQLType) => postgrestype * sqltype

# " @var{PostgresType} is a postgres SQL type name, and @var{SQLType} is its
    equivalent SQL-type name in CIAO.".

postgres2sqltype(int8,int):-!. 
% Order is relevant: e.g. a CIAO int will be defined as an int8 in postgreSQL
postgres2sqltype(int4,int):-!.
postgres2sqltype(int2,int):-!.
postgres2sqltype(float4, flt):-!.
postgres2sqltype(float8, flt):-!.
postgres2sqltype(date, date):-!.
postgres2sqltype(timestamp, datetime):-!.
postgres2sqltype(time, time):-!.
postgres2sqltype(_T,string). %% bool, char, varchar, text

:- pred postgres2sqltypes_list(PostgresTypesList, SQLTypesList) => list * list

# " @var{PostgresTypesList} is a list of postgres SQL
    types. @var{PrologTypesList} contains their equivalent SQL-type
    names in CIAO.".

postgres2sqltypes_list([],[]).
postgres2sqltypes_list([PostgresType|PostgresTypes], [PrologType|PrologTypes]):-
	postgres2sqltype(PostgresType, PrologType),
	postgres2sqltypes_list(PostgresTypes, PrologTypes).



%% use : 
% ?- date(D, "99-12-2  ", R)
% D = date(99,12,2),
% R = [32,32] ? 
% yes

%date(date(Year,Month,Day)) -->
%	num1or2(Year),
%	("-" ; "/"),
%	num1or2(Month),
%	("-" ; "/"),
%	num1or2(Day).

%num1or2(Num) -->
%        [D1,D2],
%        {
%            number_codes(Num,[D1,D2])
%        }.
%num1or2(Num) -->
%        [0'0,D2],
%        {
%            number_codes(Num,[D2])
%        }.
%num1or2(Num) -->
%        [D],
%        { 
%            number_codes(Num,[D])
%        }.
