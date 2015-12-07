:- module(db_client_types,
	[
	    socketname/1,
	    dbname/1,
	    user/1,
	    passwd/1,
	    answertableterm/1,
	    tuple/1,
	    answertupleterm/1,
	    sqlstring/1
	],[assertions,regtypes,basicmodes]).

% ----------------------------------------------------------------------------

:- doc(title,"Types for the Low-level interface to SQL databases").

:- doc(author,"Daniel Cabeza").
:- doc(author,"Manuel Carro").
:- doc(author,"Ignacio Caballero").
:- doc(author,"Manuel Hermenegildo").

:- doc(module,"This module implement the types for the low level
   interface to SQL databases").

% ----------------------------------------------------------------------------

:- regtype socketname(IPP) # "@var{IPP} is a structure describing a complete TCP/IP port address.".

socketname( IPAddress : PortNumber ) :- 
 	atm(IPAddress),
 	int(PortNumber).

:- doc(socketname/1,"@includedef{socketname/1}").

% ----------------------------------------------------------------------------

:- regtype dbname(DBId) # "@var{DBId} is the identifier of an database.".

dbname(DBId) :- 
	atm(DBId).

:- doc(dbname/1,"@includedef{dbname/1}").

:- regtype user(User) # "@var{User} is a user name in the database.".

user(User) :- 
	atm(User).

:- doc(user/1,"@includedef{user/1}").

:- regtype passwd(Passwd) # "@var{Passwd} is the password for the user
   name in the database.".

passwd(Passwd) :- 
	atm(Passwd).

:- doc(passwd/1,"@includedef{passwd/1}").

% ----------------------------------------------------------------------------

:- regtype answertupleterm(X) #  "@var{X} is a predicate containing a tuple.".

answertupleterm([]).
answertupleterm(tup(T)) :-
	tuple(T).

:- doc(answertupleterm/1,"@includedef{answertupleterm/1}").

% ----------------------------------------------------------------------------

:- regtype sqlstring(S) # "@var{S} is a string of SQL code.".

sqlstring( S ) :- 
	string(S).

:- doc(sqlstring/1,"@includedef{sqlstring/1}").

:- doc(answertableterm/1,"Represents the types of responses that
   will be returned from the database interface. These can be a
   set of answer tuples, or the atom @tt{ok} in case of a successful
   addition or deletion.").

:- regtype answertableterm(AT) # "@var{AT} is a response from the 
   database interface.".

answertableterm(ok).
answertableterm(t(Answers)) :-
	list(Answers,tuple).
answertableterm(err(Answer)) :-
	term(Answer).	

:- doc(answertableterm/1,"@includedef{answertableterm/1}").

:- regtype tuple(T) # "@var{T} is a tuple of values from the database
   interface.".

tuple(T) :-
	list(T,atm).

:- doc(tuple/1,"@includedef{tuple/1}").
