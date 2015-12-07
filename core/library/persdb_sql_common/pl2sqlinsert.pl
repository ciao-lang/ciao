:- module(pl2sqlinsert,[pl2sqlInsert/2]).
:- use_package(assertions).
:- use_module(library(lists), [append/3]).

:- multifile [sql__relation/3,sql__attribute/4].
:- data [sql__relation/3,sql__attribute/4].

pl2sqlInsert(ConstantTuple,SQLInsertString):-
	ConstantTuple=..[PredName|AttrValues],
	constants_list(AttrValues,AttrValues),
	%% all elements must be constant to be inserted
        sql__relation(PredName,_Arity,TableName),
	attributes_list(TableName,AList),
	sqlInsertString(TableName,AList,AttrValues,SQLInsertString).

sqlInsertString(TableName,AttrList,AttrValues,InsertString) :-
	atom_codes(TableName,TableString),
	stringsList2stringEnumeration(AttrList,AttributesString),
	betweenBrackets(AttributesString,AttributesTuple),
	append(TableString," ",TabStrSp),
	append(TabStrSp,AttributesTuple,StringAfterInto),
	append("INSERT INTO ",StringAfterInto,IntoString),
	valuesList2stringEnumeration(AttrValues,StringAfterValues),
	betweenBrackets(StringAfterValues,ValuesTuple),
	append(ValuesTuple,";",ValStr),   %% SQL sentence ended by ';'
	append(" VALUES ",ValStr,ValuesString),
	append(IntoString,ValuesString,InsertString).

stringsList2stringEnumeration([],"").
stringsList2stringEnumeration([Str],Str):-
	!.
stringsList2stringEnumeration([Str1|Rest],NewStr):-
	stringsList2stringEnumeration(Rest,Str),
	append(Str1,",",Str_Comma),
	append(Str_Comma,Str,NewStr).

valuesList2stringEnumeration([],"").
valuesList2stringEnumeration([Value],String):-
	number(Value),
	!,
	number_codes(Value,String).
valuesList2stringEnumeration([Value],String):- 
	atom(Value),
	!,
	atom_codes(Value,StringValue),
	betweenApostrophes(StringValue,String).
	
valuesList2stringEnumeration([Val1|Rest],NewStr):-
	valuesList2stringEnumeration([Val1],Str1),
	append(Str1,",",Str1_Comma),
	valuesList2stringEnumeration(Rest,Str),
	append(Str1_Comma,Str,NewStr).

betweenBrackets(Str,BrackStr):-
%	append("(",Str,OpenStr),
	append([0'(|Str],")",BrackStr).
betweenApostrophes(Str,ApStr):-
	replaceEscapeSeqs(Str,Str0),
%	append("'",Str0,Str1),
	append([0''|Str0],"'",ApStr).

replaceEscapeSeqs([],[]).
replaceEscapeSeqs([0'\\,0''|Xs],[0'\\,0''|Ys]):- % Do not escape it if it is
	replaceEscapeSeqs(Xs,Ys), !.           % already escaped.
replaceEscapeSeqs([0'\\,0'"|Xs],[0'\\,0'"|Ys]):- % Do not escape it if it is
	replaceEscapeSeqs(Xs,Ys), !.           % already escaped.
replaceEscapeSeqs([0'\\,0'\\|Xs],[0'\\,0'\\|Ys]):- % Do not escape it if it is
	replaceEscapeSeqs(Xs,Ys), !.           % already escaped.
replaceEscapeSeqs([0''|Xs],[0'\\,0''|Ys]):-
	replaceEscapeSeqs(Xs,Ys), !.
replaceEscapeSeqs([0'"|Xs],[0'\\,0'"|Ys]):-
	replaceEscapeSeqs(Xs,Ys), !.
replaceEscapeSeqs([0'\\|Xs],[0'\\,0'\\|Ys]):-
	replaceEscapeSeqs(Xs,Ys), !.
replaceEscapeSeqs([X|Xs],[X|Ys]):-
	replaceEscapeSeqs(Xs,Ys).

constants_list([],[]).
constants_list([Head|Tail],[Head|CLTail]):-
%%	atom(Head),!,
	nonvar(Head),!,
	constants_list(Tail,CLTail).
constants_list([_Head|Tail],CLTail):-
	constants_list(Tail,CLTail).

attributes_list(TableName,[]):-
	sql__relation(_PredName,0,TableName),!.
attributes_list(TableName,List):-
	sql__relation(_PredName,Arity,TableName), %% Arity is >0
	attrs_list(TableName,0,Arity,List).

attrs_list(_TableName,Arity,Arity,[]):-
	!.
attrs_list(TableName,Location,Arity,[AttStringName|List]) :-
	LocationPlus1 is Location+1,
	sql__attribute(LocationPlus1,TableName,AttName,_AttType),
	atom_codes(AttName,AttStringName),
	attrs_list(TableName,LocationPlus1,Arity,List).
