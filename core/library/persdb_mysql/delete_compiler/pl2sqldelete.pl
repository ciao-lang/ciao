:- module(pl2sqldelete,
	[
	    pl2sqlDelete/2,
	    substringAfterPattern/3
	],[assertions,isomodes]).

:- use_module(library(persdb_mysql/pl2sql), 
	[pl2sqlstring/3,pl2sqlterm/3]).
:- use_module(library(lists)).
:- use_module(library(messages)).

% ---------------------------------------------------------------------------

:- pred substringAfterPattern(+String,+Pattern,-Substring) :: string * string * string

   # "@var{Substring} is the right part of @var{String} located after the
      substring @var{Pattern}.".


substringAfterPattern(String,Pattern,Substring):-
	match_string(Pattern,String,Substring),
	!.
substringAfterPattern([_Char1|String],Pattern,Substring):-
	substringAfterPattern(String,Pattern,Substring).

match_string([],Rest,Rest).
match_string([H|T1],[H|T2],Rest) :-
	prefix(T1,T2,Rest).
match_string(S,[_H|T],Rest) :-
	match_string(S,T,Rest).

prefix([],Rest,Rest).
prefix([H|T1],[H|T2],Rest) :-
	prefix(T1,T2,Rest).

pl2sqlDelete(SimplePredicate,SQLDeleteString):-
	pl2sqlstring(SimplePredicate,SimplePredicate,SQLSelectString),
	!,
	substringAfterPattern(SQLSelectString,"FROM ",Rest),
	append("DELETE FROM ",Rest,SQLDeleteString).
%%%%%% TO MAKE: think about deleting complex predicates
pl2sqlDelete(Pred,_):-
	error_message("SQL translation failed for ~q ",[Pred]).
