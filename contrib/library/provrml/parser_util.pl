:- module(parser_util, [
	at_least_one/4,
	at_least_one/5,
	fillout/4,
	fillout/5,
	create_node/3,
	create_field/3,
	create_field/4,
	create_field/5,
	create_directed_field/5,
	correct_commenting/4,
	create_parse_structure/1,
	create_parse_structure/2,	
	create_parse_structure/3,
	create_environment/4,
	insert_comments_in_beginning/3,
	get_environment_name/2,
	get_environment_type/2,
	get_row_number/2,
	add_environment_whitespace/3,
	get_indentation/2,
	inc_indentation/2,
	dec_indentation/2,
	add_indentation/3,
	reduce_indentation/3,
	push_whitespace/3,
	push_dictionaries/3,
	get_parsed/2,
	get_environment/2,
	inside_proto/1,
	get_dictionaries/2,
	strip_from_list/2,
	strip_from_term/2,
	strip_clean/2,
	strip_exposed/2,
	strip_restricted/2,
	strip_interface/2,
	set_parsed/3,
	set_environment/3,
	%set_dictionaries/3,
	insert_parsed/3,
	reverse_parsed/2,
	stop_parse/2,
	look_first_parsed/2,
	get_first_parsed/3,
	remove_code/3,
	look_ahead/3
	],[assertions, isomodes, iso, dcg]).


:- doc(author, "G@..{o}ran Smedb@..{a}ck").


%:- include(library(types)).

:- use_module(library(lists)).
:- use_module(library(provrml/dictionary_tree)).
:- use_module(library(provrml/internal_types)).
%%%:- use_module(i_o).

:- set_prolog_flag(multi_arity_warnings, off).
%%:- discontiguous([token_read/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred stop_parse(+TermIn,-TermOut)
:: term * term
# "The predicate will bind the invalue to the outvalue, used to 
   terminate a parsing.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop_parse(S,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred create_node(+NodeTypeId,+Parse,-Node)
:: atm * parse * term
# "The predicate will construct a node term with 
   the read guts which is inside the parse structure. A node
   consists of its name and one argument, a list of its fields.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_node(NodeTypeId,Parse_structure,Node) :-
	get_parsed(Parse_structure,Guts),
	Node =.. [NodeTypeId,Guts].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred create_field(+FieldNameId,+Arguments,-Field)
:: atm * term * term
# "The predicate will construct a field with the Id as the fieldname
   and the arguments as they are, in a double list, which results in 
   a single list or a single list which will result in free arguments.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_field(FieldNameId, Arguments, Field) :-
	Field =.. [FieldNameId|Arguments].

%%This predicate will probably not be used, no testing done to confirm.
create_field(FieldNameId, [Arg], Field) :-
	extract_arguments_from_list(Arg, Arguments),
	Field =.. [FieldNameId,Arguments].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred create_field(+FieldAccess,+FieldType,+FieldId, -Field)
:: atm * atm * atm * term
# "The predicate will construct a field with its access type
   as the name with type and id as arguments.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_field(FieldAccess,FieldType,FieldId, Field) :-
	Field =.. [FieldAccess,FieldType,FieldId].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred create_field(+FieldAccess,+FieldType,+FieldId,+Fieldvalue,-Field)
:: atm * atm * atm * term * term
# "The predicate will construct a field with its access type
   as the name with type, id and value as arguments.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_field(FieldAccess,FieldType,FieldId,FieldValue, Field) :-
	Field =.. [FieldAccess,FieldType,FieldId,FieldValue].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred create_directed_field(+Access,+Type,+Id0,+Id1,-Field)
:: atm * atm * atm * atm * term
# "The predicate will construct a directed field with the key word
   IS in the middle. Its access type
   as the name with type, from id0 and to id1 as arguments.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_directed_field(FieldAccess,FieldType,FieldId0,FieldId1, Field) :-
	Field =.. [FieldAccess,FieldType,FieldId0,'IS',FieldId1].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred extract_arguments_from_list(+ArgList, -NewArgList)
:: list(term) * list(term)
# "The predicate will strip off the old field names and return 
   all the arguments from the fields in a new list.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract_arguments_from_list(Arg, Arguments) :-
	extract_arguments_from_list(Arg, [], Arguments).

extract_arguments_from_list([Old_field|Rest], In, Result) :-
	Old_field =.. [_Old_name|Arguments],
	append(In, Arguments, Out),
	extract_arguments_from_list(Rest, Out, Result).

extract_arguments_from_list([], Arguments, Arguments).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% get_NodeName(Node,NodeName) :-
% 	Node =.. [NodeName,_Guts].

:- discontiguous([create_parse_structure/1,
	create_parse_structure/2,
	get_parsed/2,
	set_environment_whitespace/3,
	get_indentation/2,
	add_environment_indentation/3,
	reduce_environment_indentation/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred create_parse_structure(-Parse)
:: parse
# "The predicate will construct the parse structure with its three
   fields: the parsing list, the environment structure, and the 
   dictionaries.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_parse_structure(parse([],Env,Dictionaries)) :-
	create_dictionaries(Dictionaries),
	create_whitespace_environment(Env).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred create_parse_structure(+ParseIn,-ParseOut)
:: parse * parse
# "The predicate will construct a parse structure with its three
   fields: the parsing list, the environment structure, and the 
   dictionaries. It will reuse the environment and the dictionaries
   from the input.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_parse_structure(parse(_Parsed,Env,Dic),parse([],Env,Dic)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred create_parse_structure(+ParsedList,-ParseOut)
:: list(term) * parse
# "The predicate will construct a parse structure with its three
   fields: the parsing list, the environment structure, and the 
   dictionaries. It will use the list of parsed items in its structure.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_parse_structure(Parsed,parse(Parsed,E,D)) :-
	create_dictionaries(D),
	create_whitespace_environment(E).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred create_parse_structure(+ParsedList,+ParseIn,-ParseOut)
:: list(term) * parse * parse
# "The predicate will construct a parse structure with its three
   fields: the parsing list, the environment structure, and the 
   dictionaries. It will use the list of parsed items in its structure
   and the environment and the dictionary from the parse structure
   given.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_parse_structure(Parsed,parse(_Parsed,Env,Dic),parse(Parsed,Env,Dic)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred create_environment(+Parse,+EnvType,+Name,-EnvStruct)
:: parse * atm * atm * environment
# "The predicate will construct an environment structure based
   on the information in the parse structure. Well only the white-
   space information will be reused. The are three types of environments
   'PROTO', 'EXTERNPROTO', and 'DEF'.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_environment(parse(_,env(_,_,W),_),'PROTO',Name,
	           env('PROTO',Name,W)).

create_environment(parse(_,env(_,_,W),_),'EXTERNPROTO',Name,
	           env('EXTERNPROTO',Name,W)).

create_environment(parse(_,env(_,_,W),_),'DEF',Name,
	           env('DEF',Name,W)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred create_whitespace_environment(-Env)
:: environment
# "The predicate will construct an environment with an empty
   whitespace structure.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_whitespace_environment(env(_,_,Ws)) :-
	create_whitespace(Ws).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred create_whitespace(-WhitespaceStructure)
:: whitespace
# "The predicate will create a whitespace structure, empty and ready
   to use.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
create_whitespace(ws(0,0)).
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred create_whitespace(+Whitespaces,+Indentations,-WhitespaceStructure)
:: num * num * whitespace
# "Will create a whitespace structure with the given whitespace and 
   the given indentation.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_whitespace(White,Indent,ws(White,Indent)).
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_parsed(+ParseStructure,-ListOfParsed)
:: parse * list(term)
# "The predicate will return a list of the parsed terms that is inside 
   the parse structure.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_parsed(parse(P,_,_),P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_parsed(+ParseStructure,-EnvironmentStructure)
:: parse * environment
# "The predicate will return the environment of the parse structure.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_environment(parse(_,E,_),E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_parsed(+ParseStructure,-Dictionaries)
:: parse * dictionary
# "The predicate will return dictionary used within the parse structure.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_dictionaries(parse(_,_,D),D) :-
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred set_environment(+Environment,+ParseIn,-ParseOut)
:: environment * parse * parse
# "The modificator will return a parse structure with the environment given 
   with the other properties from the first parse structure.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

set_environment(E,parse(P,_,D),parse(P,E,D)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_environment_name(+Environment,-Name)
:: environment * atm
# "The predicate will return the enviroment name.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_environment_name(env(_Type,Name,_Ws),Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_environment_type(+Environment,-Type)
:: environment * atm
# "The predicate will return the enviroment type, one of the three:
   'PROTO', 'EXTERNPROTO', and 'DEF'.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_environment_type(env(Type,_Name,_Ws),Type).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_environment_whitespace(+Environment,-WhitespaceStruct)
:: environment * whitespace
# "The predicate will return the enviroment whitespace structure.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_environment_whitespace(env(_T,_N,W),W).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred set_environment_whitespace(+WhitespaceStruct,+EnvironmentIn,-EnvironmentOut)
:: whitespace * environment * environment
# "The predicate will set the whitespace to an environment by the whitespace 
   structure as the first argument.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

set_environment_whitespace(W,env(T,N,_W_old),env(T,N,W)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred set_environment_whitespace(+WhitespaceStruct,+ParseIn,-ParseOut)
:: whitespace * parse * parse
# "The predicate will set the whitespace to a parse structure by the whitespace 
   structure as the first argument.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

set_environment_whitespace(W,parse(P,E,D),parse(P,E_new,D)) :-
	set_whitespace(W,E,E_new).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred set_whitespace(+WhitespaceStruct,+EnvironmentIn,-EnvironmentOut)
:: whitespace * environment * environment
# "The predicate will set the whitespace to an environment by the whitespace 
   structure as the first argument, used internally and ought to be changed
   to the set_environment_whitespace/2.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

set_whitespace(W,env(T,N,_),env(T,N,W)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred set_parsed(+ParseIn,+NewParseList,-ParseOut)
:: parse * list(term) * parse
# "The predicate will create a new parse structure from the first parse
   structure with the parse list from the second argument.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_parsed(parse(_,E,D),P,parse(P,E,D)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred set_dictionaries(+Dic,+Parse,-Parse) 
:: dictionary * parse * parse
# "The predicate will set the input directory to the parseing structure 
   input   argument. The resulting parsing structure will be returned.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_dictionaries(D,parse(P,E,_),parse(P,E,D)) :-
	is_dictionaries(D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred push_dictionaries(+Parse,+Parse,-Parse) 
:: parse * parse * parse
# "The predicate will set the first parse structure's directory 
   to the second parsing structure argument. 
   The resulting parsing structure will be returned.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

push_dictionaries(Trash,In,Out) :-
	get_dictionaries(Trash,D),
	set_dictionaries(D,In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_environment_whitespace_row(+Environment,-Row)
:: environment * num
# "The predicate will return the row number from the environment 
   structure. This internal predicate is only used by the 
   get_row_number/2.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_environment_whitespace_row(env(_T,_N,W),R) :-
	get_whitespace_row(W,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_row_number(+Parse,-Row)
:: parse * num
# "The predicate will return the row number from the parse 
   structure. The row number is not fully implemented.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_row_number(parse(_,E,_),Row) :-
	get_environment_whitespace_row(E,Row).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_whitespace_row(+Whitespace,-Row)
:: whitespace * num
# "The prdicate return the row number from the whitesoace structure.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_whitespace_row(ws(R,_Ind),R).
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred set_whitespace_row(+Row,-Whitespace)
:: num * whitespace 
# "The predicate return the whitespace structure with the row number set.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_whitespace_row(R,ws(R,_Ind)).
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred set_whitespace_row(+Row,+Whitespace,-Whitespace)
:: integer * whitespace * whitespace
# "The predicate will from a row number and a whitespace structure
   return a new structure with the content.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_whitespace_row(R,ws(_OldRow,Ind),ws(R,Ind)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_indentation(+Whitespace,-Indentation)
:: whitespace * num
# "The predicate will return the indentation depth from a
   whitespace structure.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_indentation(ws(_R,Ind),Ind).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_indentation(+Parse,-Indentation)
:: parse * num
# "The predicate will return the indentation depth from a
   parse structure.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_indentation(Parse,Ind) :-
	get_environment(Parse,Env),
	get_environment_whitespace(Env,Ws),
	get_indentation(Ws,Ind).
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred set_indentation(+Ind,-Whitespace)
:: num * whitespace
# "The predicate will set the indentation, well create
   a whitespace structure with the indentation we could say.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_indentation(Ind,ws(_R,Ind)).
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred set_indentation(+Ind,+Whitespace,-Whitespace)
:: num * whitespace * whitespace
# "The predicate will set the indentation to a old whitespace 
   structure. The returned structure will have the old whitespace 
   values and with the new indentation.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_indentation(Ind,ws(R,_OldInd),ws(R,Ind)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred increase_row(+EnvIn,+Inc,-EnvOut)
:: environment * num * environment
# "The predicate will increase the row number and add it to the 
   environment.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

increase_row(Env,Inc,Out) :-
	get_environment_whitespace(Env,Ws),
	get_whitespace_row(Ws,Row),
	New is Row + Inc,
	set_whitespace_row(New,Ws,White_new),
	set_whitespace(White_new,Env,Out).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred add_environment_whitespace(+EnvIn,+WhiteSpaceList,-EnvOut)
:: environment * list(atm) * environment 
# "The predicate will add the new whitespace that is collected 
   in a list of whitespaces to the environment.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_environment_whitespace(In,White,Out) :-
	count_row(White,Rows),
	add_environment_whitespace_row(In,Rows,Out).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred add_environment_whitespace_row(+EnvIn,+Rows,-EnvOut)
:: environment * num * environment 
# "The predicate will add the new number of rows to the environment.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_environment_whitespace_row(In,Rows,Out) :-
	get_environment(In,Env),
	increase_row(Env,Rows,New_env),
	set_environment(New_env,In,Out).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred push_whitespace(+ParseWithWhitespace,+ParseIn,-ParseOut)
:: parse * parse * parse
# "The predicate will add the whitespace values from one parse
   structure to another one, resultin in the output, with the values
   from the second parse structure with the whitespace from the first
   added.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

push_whitespace(Trash,Save,Out) :-
	get_environment(Trash,Env),
	get_environment_whitespace(Env,Ws),
	get_whitespace_row(Ws,R0),
	get_indentation(Ws,Indent),
	add_indentation(Save,Indent,Save0),	
	add_environment_whitespace_row(Save0,R0,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred add_environment_indentation(+EnvIn,+Add,-EnvOut)
:: environment * num * environment
# "The predicate will add the indentation to an environment structure 
   return the resulting one.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_environment_indentation(Env,Add,Out) :-
	get_environment_whitespace(Env,Ws),
	get_indentation(Ws,Indent),
	New_indent is Indent + Add,
	set_indentation(New_indent,Ws,New_ws),
	set_environment_whitespace(New_ws,Env,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred reduce_environment_indentation(+EnvIn,+Reduce,-EnvOut)
:: environment * num * environment
# "The predicate will reduce the indentation of an environment structure 
   return the resulting one with the indentation reduced the required 
   number of steps.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reduce_environment_indentation(Env,Reduce,Out) :-
	get_environment_whitespace(Env,Ws),
	get_indentation(Ws,Indent),
	( Indent >= Reduce
	->New_indent is Indent - Reduce
	; New_indent = 0
	),
	set_indentation(New_indent,Ws,New_ws),
	set_environment_whitespace(New_ws,Env,Out).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred add_environment_indentation(+ParseIn,+Add,-ParseOut)
:: parse * num * parse
# "The predicate will add the indentation to an parse structure 
   the asked number of steps, will return the resulting parse structure.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_indentation(In,Add,Out) :-
	get_environment(In,Env),
	add_environment_indentation(Env,Add,Env_new),
	set_environment(Env_new,In,Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred reduce_environment_indentation(+ParseIn,+Reduce,-ParseOut)
:: parse * num * parse
# "The predicate will reduce the indentation of an parse structure 
   return the resulting one with the indentation reduced the required 
   number of steps.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reduce_indentation(In,Reduce,Out) :-
	get_environment(In,Env),
	reduce_environment_indentation(Env,Reduce,New_env),
	set_environment(New_env,In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred inc_indentation(+ParseIn,-ParseOut)
:: parse * parse
# "Will increase the indentation with one step to a parse structure.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inc_indentation(In,Out) :-
	add_indentation(In,1,Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred dec_indentation(+ParseIn,-ParseOut)
:: parse * parse
# "Will decrease the indentation with one step to a parse structure.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dec_indentation(In,Out) :-
	reduce_indentation(In,1,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred inside_proto(+Parse)
:: parse
# "The predicate will answer to the question: are we parsing inside
   a PROTO/EXTERNPROTO.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inside_proto(In) :-
	get_environment(In,E),
	get_environment_type(E,Name),
	(  Name == 'PROTO'
        ;  Name == 'EXTERNPROTO'
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred correct_commenting(+Place,+Comment,+ParsedIn,-ParsedOut)
:: atm * struct * term * term
# "The predicate places the comment 'before' or 'after' the parsed
   term. This results in a list with the term and the comment or in 
   just returning the term.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

correct_commenting(before,Comment,Term,Out) :-
	( Comment == []
	->Out = Term
	; append(Comment,[Term],Out)
	).

correct_commenting(after,Comment,Term,Out) :-
	( Comment == []
	->Out = Term
	; append([Term],Comment,Out)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred strip_from_list(+ListWithComments,-CleanList)
:: list(term) * list(term)
# "The predicate will strip the list from comments and
   return a list without any comments.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

strip_from_list([],[]).

strip_from_list([E|Rest],Output) :-
	list(E),
	strip_from_list(E,E0),
	strip_from_list(Rest,R0),
	append([E0],R0,Output).

strip_from_list([E|Rest], Output) :-
	atomic(E),
	name(E,List),
	List = [35|_],
	strip_from_list(Rest,Output).

strip_from_list([E|Rest], [E|Output]) :-
	strip_from_list(Rest,Output).

strip_from_list(A,A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred strip_clean(+ParsedIn,-ParsedOut)
:: term * term
# "The predicate will return a striped list or a single atom
   if there was no comments and no more items in the list.
   It will also return a atom if there is comments and only one 
   other element.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

strip_clean(Atom,Atom) :-
	atomic(Atom).

strip_clean(List,Clean) :-
	list(List),
	strip_from_list(List,Out),
	( list(Out)
	-> Out = [More],
	   strip_clean(More,Clean)
	;  Clean = Out
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred strip_from_term(+Term,-Stripped)
:: term * term
# "The predicate will remove comments from a term, it will reduce its
   arguments    if there are comments as arguments.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

strip_from_term(Term,Stripped) :-
	compound(Term),
	Term =.. [Head|List],
	strip_from_list(List,Str),
	Stripped =.. [Head|Str].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred strip_interface(+Interface,-StrippedInterface)
:: list(term) * list(term)
# "The predicate will remove comments from the interface
   that we read for the PROTOtype. This will help us when setting the
   properties.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

strip_interface(Interface,Stripped) :-
	strip_from_list(Interface,Pure),
	strip_interface0(Pure,Stripped).

strip_interface0([],[]).
strip_interface0([Node|Interface], [New|Pure]) :-
	strip_exposed(Node,New),
	strip_interface(Interface,Pure).

strip_interface0([Node|Interface], [New|Pure]) :-
	strip_restricted(Node,New),
	strip_interface(Interface,Pure).

strip_restricted(Field,New) :-
	Field =.. [Acc,Type,Id],
	strip_clean(Type,Type_new),
	strip_clean(Id,Id_new),
	New =.. [Acc,Type_new,Id_new].

strip_exposed(Field,New) :-
	Field =.. [Acc,Type,Id,Value],
	strip_clean(Type,Type_new),
	strip_clean(Id,Id_new),
	strip_from_list(Value,Value_new),
	New =.. [Acc,Type_new,Id_new,Value_new].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred insert_parsed(+ListOfParsed,+ParseIn,-ParseOut)
:: list(term) * parse * parse
# "The predicate will append the newly parsed terms to the old
   that we have in the parse structure.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_parsed(Parsed,In,Out) :-
	get_parsed(In,Old_parsed),
	append(Parsed, Old_parsed, New_parsed),
	set_parsed(In,New_parsed,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred insert_comments_in_beginning(+Comment,+ParseIn,-ParseOut)
:: struct * parse * parse
# "We add the comment in the beginneing of the parsed, to get the 
   proper look.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_comments_in_beginning(Com,In,Out) :-
	get_parsed(In,Old_parsed),
	append(Com, Old_parsed, New_parsed),
	set_parsed(In,New_parsed,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred reverse_parsed(+ParseIn,-ParseOut)
:: parse * parse
# "The returned parse structure is the same as the input with the 
   difference that the parsed terms are in reverse order.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reverse_parsed(Parsed,Reversed) :-
	get_parsed(Parsed,P),
	reverse(P,Rev),
	set_parsed(Parsed,Rev,Reversed).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred look_first_parsed(+Parse,-First)
::  parse * term
# "Look at the first item in the parse structure.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

look_first_parsed(In,First) :-
	get_parsed(In,[First|_More]).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_first_parsed(+ParseIn,-ParseOut,-First)
::  parse * parse * term
# "Get the first item in the parse structure and return
   the parse structure with the item removed.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_first_parsed(In,Out,First) :-
	get_parsed(In,[First|More]),
	set_parsed(In,More,Out).

%If there is no more
get_first_parsed(In,In,[]) :-
	get_parsed(In,[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred look_ahead(+Name,+Parsed,-Parsed)
:: atm * list(term) * list(term)
# "This predicate is used normally by the CDG and the two last arguments
   will therefore be the same because we don't remove the parsed.
   The name is the name inside a term, the first argument.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

look_ahead(Name, [Ahead|Rest], [Ahead|Rest]) :-
	Ahead =.. [_Token,Name|_More].

%Otherwise there is no more input or parenthesis.
%look_ahead(Name, [Ahead|Rest], [Ahead|Rest]) :-
%	Ahead =.. [Name|_More].


remove_code(Stop_sign) -->
	[Stop_sign].
	
remove_code(Stop_sign) -->
	[Skipped],
	{write(Skipped),nl},
	remove_code(Stop_sign).

remove_code(_Stop_sign) -->
	[].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred count_row(+WhitespaceList,-Rows)
:: list(atm) * num
# "The returned number is the number of rows we have in the whitespace 
   list.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

count_row(Whitespace,Rows) :-
	count_row(Whitespace,0,Rows).

count_row([],R,R).
count_row([Num|Rest],In,Result) :-
	( ( Num == 10; 
	    Num == 13 )
	->  Out is In + 1,
	    count_row(Rest,Out,Result)
	;
	    count_row(Rest,In,Result)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Can catch zero or more of each.
:- pred fillout(+ParseIn,-ParseOut,-ResultingList,L,T)
:: parse * parse * list(term) * list * list
# "If there are whitespaces and comments, zero or more of each,
   we add them to the resulting list. This read all comments and 
   all whitespace.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fillout(In,Out,Fill) -->
	at_least_one(In,In0,C),
	fillout(In0,Out,C1),
	{append(C,C1,Fill)}.

fillout(In,In,[]) -->
	[].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Can catch zero or more of each.
:- pred fillout(+ParseIn,-ParseOut,L,T)
:: parse * parse * list * list
# "If there are whitespaces and comments, zero or more of each.
   This read all comments and all whitespace. The comments and whitespace
   will not be returned.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fillout(In,Out) -->
	at_least_one(In,In0),
	fillout(In0,Out).

fillout(In,In) --> 
	[].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred at_least_one(+ParseIn,-ParseOut,L,T)
:: parse * parse * list * list
# "One or more whitespace or comment have to be read, for the moment
   there are no whitespaces to be read so we only stick with the comments.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

at_least_one(In,Out) -->
	whitespace(In,In0),
	comment_more(In0,Out).

at_least_one(In,Out) -->
	comment(In,In0),
	whitespace_more(In0,Out).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred at_least_one(+ParseIn,-ParseOut,-ListOfRead,L,T)
:: parse * parse * list(term) * list * list
# "One or more whitespace or comment have to be read, the result
   will be given in the list.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

at_least_one(In,Out,Com) -->
	whitespace(In,Out),
	comment_more(Com).

at_least_one(In,Out,Com) -->
	comment(Com),
	whitespace_more(In,Out).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred comment(+ParseIn,-ParseOut,L,T)
:: parse * parse * list * list
# "We read at least one comment according to the rules for them.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

comment(In,Out) -->
	[comment(Com)],
	{insert_parsed([Com],In,In0)},
	comment_more(In0,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred comment_more(+ParseIn,-ParseOut,L,T)
:: parse * parse * list * list
# "The predicate will read zero or more comments. The difference between
   this predicate and the comment/5 is that thi one do not require a comment.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

comment_more(In,Out) -->
	[comment(Com)],
	comment_more(More),
	{insert_parsed([Com|More],In,Out)}.

comment_more(In,In) -->
	[].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred comment(-ListOfComments,L,T)
:: list(term) * list * list
# "The predicate will one or more comments, using DCG it will not need 
   more arguments.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

comment(Com) -->
	[comment(C)],
	comment_more(More),
	{append([C],More,Com)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred comment_more(-ListOfComments,L,T)
:: list(term) * list * list
# "The predicate will zero or more comments, using DCG it will not need 
   more arguments.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

comment_more([C|More]) -->
	[comment(C)],
	comment_more(More).

comment_more([]) -->
	[].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred whitespace(+ParseIn,-ParseOut,L,T)
:: parse * parse * list * list
# "Will read one or more whitespaces according to the tokens indicating
   whitespaces, not used for the moment.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

whitespace(In,Out) -->
	[whitespace(W)],
	whitespace_more(More),
	{append(W,More,WhiteSpace),
	 add_environment_whitespace(In,WhiteSpace,Out)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred whitespace_more(-ListOfWhitespace,L,T)
:: list(term) * list * list
# "Will read zero or more whitespaces, using DCG.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

whitespace_more(White) -->
	[whitespace(W)],
	whitespace_more(More),
	{append(W,More,White)}.
	
whitespace_more([]) -->
	[].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred whitespace_more(+ParseIn,-ParseOut,L,T)
:: parse * parse * list * list
# "Will read zero or more whitespaces according to the tokens indicating
   whitespaces, not used for the moment.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
whitespace_more(In,Out) -->
	[whitespace(W)],
	whitespace_more(More),
	{append(W,More,WhiteSpace),
	 add_environment_whitespace(In,WhiteSpace,Out)}.

whitespace_more(In,In) -->
	[].
