:- module(sql_types,
	[
	    sql_type/1
	    
	],
	[assertions,regtypes,isomodes]).

%% TO DO:
%% there must be a way of describing the database specific types instead of
%% embedding them in this module. 
%% Every specific database must have an associated prolog source to deal with 
%% specific types.

% ----------------------------------------------------------------------------
% SQL92 standard type system.
% ----------------------------------------------------------------------------

% ----------------------------------------------------------------------------
% SQL92 types.
% ----------------------------------------------------------------------------

:- prop sql_type(Type) :: term + regtype

	# "Type definition for SQL types definitions. Are not included
           sql type abbreviations nor alternative spellings.".

sql_type(character(N)):-
	int(N),
	N > 0,
	max_characters(Max),
	N =< Max.

sql_type(character_varying(N)):-
	int(N),
	N > 0,
	max_characters(Max),
	N =< Max.
	
sql_type(bit(N)):-
	int(N),
	N > 0,
	max_bits(Max),
	N =< Max.

sql_type(bit_varying(N)):-
	int(N),
	N > 0,
	max_bits(Max),
	N =< Max.

sql_type(numeric(P,Q)):-
	int(P),
	int(Q),
	P > 0,
	Q >= 0,
	P > Q,
	max_numerics(Max),
	P =< Max.

sql_type(decimal(P,Q)):-
	int(P),
	int(Q),
	P > 0,
	Q >= 0,
	P > Q,
	max_numerics(Max),
	P =< Max.
sql_type(integer).
sql_type(smallint).
sql_type(float(P)):-
	int(P),
	P >= 0,
	max_numerics(Max),
	P =< Max.
sql_type(date).
sql_type(time(P)):-
	int(P),
	P >= 0,
	max_date_decimals(Max),
	P =< Max.
sql_type(time).
sql_type(datetime).
sql_type(datetime(P)):-
	int(P),
	P >= 0,
	max_date_decimals(Max),
	P =< Max.

%% NOTE: These limits should be implementation defined.
max_characters(255).
max_bits(255).
max_numerics(30).
max_decimals(20).
max_date_decimals(10).

% ----------------------------------------------------------------------------
% SQL92 scalar data types.
% ----------------------------------------------------------------------------

:- pred character(Length,Value) :: int * string # "".

 %% ---------------------------------------------------------------------------
