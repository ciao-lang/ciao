:- module(generator_util,
	[   reading/4,
	    reading/5,
	    reading/6,
	    open_node/6,
	    close_node/5,
	    close_nodeGut/4,
	    open_PROTO/4,
	    close_PROTO/6,
	    open_EXTERNPROTO/5,
	    close_EXTERNPROTO/6,
	    open_DEF/5,
	    close_DEF/5,
	    open_Script/5,
	    close_Script/5,
	    decompose_field/3,
	    indentation_list/2,
	    start_vrmlScene/4,
	    remove_comments/4
	],[dcg, assertions, nortchecks, isomodes]).

:- doc(author, "G@..{o}ran Smedb@..{a}ck").


%:- use_module(engine(basic_props)).
:- use_module(library(provrml/provrmlerror), [error_vrml/1]).
:- use_module(library(provrml/provrml_io), [out/3]).
:- use_module(library(provrml/field_value), [parse/1]).
:- use_module(library(provrml/field_value_check), [mfstringValue/7]).
:- use_module(library(provrml/lookup), 
        [lookup_check_node/4,
         lookup_check_field/6,
         lookup_route/5,
         lookup_field_access/4,
         lookup_check_interface_fieldValue/8,
         lookup_set_prototype/4,
         lookup_set_def/3]).
:- use_module(library(provrml/parser_util), 
        [look_first_parsed/2,
         get_first_parsed/3,
         get_indentation/2,
         get_parsed/2,
         create_parse_structure/3,
         push_dictionaries/3,
         inc_indentation/2,
         dec_indentation/2,
         inside_proto/1,
         strip_clean/2,
         set_environment/3,
         create_environment/4
        ]).

:- set_prolog_flag(multi_arity_warnings, off).
:- discontiguous([reading/4,reading/5,reading/6]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred reading(+Entrance,+ParseIn,-ParseOut,L,T)
:: atm * parse * parse * list * list
# "This predicate is a general predicate in this help module. The first argument
   is a key word to direct the input to the right entrance. When I say
   output in the following predicates I am refering to the out predicate which
   actually will use the features of DCG and we will add the output terms
   in a list for later output. The list is hidden to the user but is in
   this case the fourth argument will be the list and the fifth will be
   the resulting list, hopefully empty after the generation. Then we should have
   read all the terms.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred reading(+Empty,+ParseIn,-ParseOut,L,T)
:: atm * parse * parse * list * list
# "This predicate check if we have ran out of input and is ready to 
   terminate the reading.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reading(empty,In,In) -->
	{ 
            get_parsed(In,Parsed),
            Parsed == []
	}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred reading(+Header,+ParseIn,-ParseOut,L,T)
:: atm * parse * parse * list * list
# "This predicate read the header and after the header we can have
   more information, that is comments.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reading(header,In,Out) -->
	{ get_first_parsed(In,Out,Comment),
	  name(Comment,List),
	  List = [35,86,82,77,76,32,86,50,46,48,32,117,116,102,56|_More]
	},
	out([Comment,'\n\n']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred reading(+NULL,+ParseIn,-ParseOut,L,T)
:: atm * parse * parse * list * list
# "This predicate accepts the special key word 'NULL' and will output that.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reading('NULL',In,Out) -->
	{ get_first_parsed(In,Out,First),
	  'NULL' == First
	},
	out(['NULL\n']).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred reading(+Comment,+ParseIn,-ParseOut,L,T)
:: atm * parse * parse * list * list
# "This predicate will read a comment.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reading(comment,In,Out) -->
	{ get_first_parsed(In,Out,Comment),
	  atomic(Comment),
	  name(Comment,List),
	  List = [35|_More],
	  indentation_list(In,Indent)
	},
	out(Indent),
	out([Comment]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred reading(+DEF,+Parse,L,T)
:: atm * parse * list * list
# "This predicate will check if we have the special key word.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reading('DEF',In) -->
	{ look_first_parsed(In,First),
	  get_name(First,Name),
	  'DEF' == Name
	}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred reading(+USE,+ParseIn,-ParseOut,L,T)
:: atm * parse * parse * list * list
# "This predicate will read the 'USE' key word. ".
%  A complete check of the key word should be done, but isn't.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reading('USE',In,Out) -->
	{ get_first_parsed(In,Out,First),
	  decompose_field(First,Name,[NodeName]),
	  'USE' == Name,
	  indentation_list(In,Indent)
	},
	out(Indent),
	out(['USE ',NodeName]).
	%checkup the NodeName and write when done
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred reading(+IS,+NodeTypeId,+ParseIn,-ParseOut)
:: atm * atm * parse * parse
# "This predicate will refer to a formerly introduced interface.
   We do a checkup of the access type and output the values.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reading('IS',NodeTypeId,In,Out) -->
	{ inside_proto(In),
	  get_first_parsed(In,In0,First0),
	  get_first_parsed(In0,In1,First1),
	  First1 == 'IS',
	  get_first_parsed(In1,Out,First2),
	  strip_clean(First0,Clean0),
	  strip_clean(First2,Clean2),
	  lookup_field_access(In,NodeTypeId,Clean2,Clean0),
	  indentation_list(In,Indent)
	},
	out(Indent),
	out([First0,' IS ',First2,'\n']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred reading(+Node,+Parse,L,T)
:: atm * parse * list * list
# "This predicate will read a node so we will check the properties of 
   that one and then continue the progress in the generation.".  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reading(node,In) -->
	{ look_first_parsed(In, Node),
	  get_name(Node,Name)
	},
	lookup_check_node(In,Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred reading(+Script,+Parse,L,T)
:: atm * parse * list * list
# "This predicate read a script and will then continue the generation.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reading('Script',In) -->
	{ look_first_parsed(In, First),
	  get_name(First,Name),
	  'Script' == Name
	}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred reading(+NodeGut,+NodeName,+ParseIn,-ParseOut)
:: atm * atm * parse * parse
# "This predicate will read a node gut and will check the field 
   according to the name.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reading(nodeGut,Name,In,Out) -->
	{ get_first_parsed(In,In0,Field)
	},
	lookup_check_field(In0,Out,Name,Field),
	out(['\n']).	  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred reading(+PROTO,+Parse,L,T)
:: atm * parse * list * list
# "This predicate will read a prototype, check that the term name is the 
   one, 'PROTO'.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reading('PROTO',In) -->
	{ look_first_parsed(In,First),
	  get_name(First, Name),
	  'PROTO' == Name
	}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred reading(+EXTERNPROTO,+Parse,L,T)
:: atm * parse * list * list
# "This predicate read a term with the name given as the first argument.". 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reading('EXTERNPROTO',In) -->
	{ look_first_parsed(In,First),
	  get_name(First, Name),
	  'EXTERNPROTO' == Name
	}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred reading(+MfstringValue,+ParseIn,-ParseOut,L,T)
:: atm * parse * parse * list * list
# "This predicate will read the term multi field string value. Then
   it will continue the generation in the field_value_check module.
   There it will read the string values and generate the code.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reading(mfstringValue,In,Out) -->
	{ get_parsed(In,MFStringValue),
	  indentation_list(In,Indent)
	},
	out(Indent),
	out([']\n']),
	mfstringValue(MFStringValue,In,Out,_,[]).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred reading(+ExposedField,+ParseIn,-ParseOut,L,T)
:: atm * parse * parse * list * list
# "This predicate will read an exposedField and do the checkup for 
   the interface with all its components.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reading(exposedField,In,Out) -->
	{ get_first_parsed(In,In0,First),
	  get_name(First, Name),
	  exposedField == Name,
	  decompose_term(First,[Acc,Type,Id,Value])
	},
	lookup_check_interface_fieldValue(In0,Out,Acc,Type,Id,Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred reading(+RestrictedInterfaceDeclaration,+ParseIn,-ParseOut,L,T)
:: atm * parse * parse * list * list
# "This predicate will read the declaration for a restricted field
   and do the checkup accordingly if necessary.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% There are different types of field and exposedField but that will
% be handled in writing.
reading(restrictedInterfaceDeclaration,In,Out) -->
	{ get_first_parsed(In,In0,First),
	  get_name(First, Name),
	  ( field == Name
	  ; eventIn == Name
	  ; eventOut == Name
	  )
	},
	( { Name == field }
	->{ decompose_term(First,[Acc,Type,Id,Value]) },
          lookup_check_interface_fieldValue(In0,Out,Acc,Type,Id,Value)
	; { decompose_term(First,[Acc,Type,Id]),
	    indentation_list(In0,Indent),
	    In0 = Out
	  },
	  out(Indent),
	  out([Acc,' ',Type,' ',Id,'\n'])
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred reading(+ExternInterfaceDeclaration,+ParseIn,-ParseOut,L,T)
:: atm * parse * parse * list * list
# "For reading an external interface declaration we see that we have
   three arguments in the term and that we have special access key word.
   We then outputs the declaration.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reading(externInterfaceDeclaration,In,Out) -->
	{ get_first_parsed(In,Out,First),
	  decompose_term(First,[Acc,Type,Id]),
	  ( field == Acc
	  ; eventIn == Acc
	  ; eventOut == Acc
	  ; exposedField == Acc
	  ),
	  indentation_list(In,Indent)
	},
	out(Indent),
	out([Acc,' ',Type,' ',Id,'\n']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred reading(+ROUTE,+ParseIn,-ParseOut,L,T)
:: atm * parse * parse * list * list
 # "Reading a ROUTE and we split the term into its parts. There might be comments
    in the different fields and therefore we have to strip_clean the fields to 
    get them without a possible list containing comments, this for the checkup
    of the routing parameters.
    We then do an output of the values, then with the comments.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reading('ROUTE',In,Out) -->
	{ get_first_parsed(In,Out,First),
	  decompose_term(First,[Name,NodeOut0,FieldOut0,'TO',NodeIn0,FieldIn0]),
	  'ROUTE' == Name,
	  strip_clean(NodeOut0,NodeOut),
	  strip_clean(FieldOut0,FieldOut),
	  strip_clean(NodeIn0,NodeIn),
	  strip_clean(FieldIn0,FieldIn),
	  lookup_route(In,NodeOut,FieldOut,NodeIn,FieldIn)
	},
	out(['ROUTE ',NodeOut0,'.',FieldOut0,' TO ',NodeIn0,'.',FieldIn0,'\n']).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred reading(+Error_X,+ParseIn,-ParseOut,L,T)
:: struct * parse * parse * list * list
# "The predicate will call a proper error message after fetching
   some values like name.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reading(error_nodeGut(Name),In,Out) -->
	{ get_first_parsed(In,Out,First),
	  error_vrml(nodeGuts(Name,First))
	}.

reading(error_nodeDeclaration,In,Out) -->
	{ get_first_parsed(In,Out,First),
	  error_vrml(nodeDeclaration(First))
	}.

reading(error_header,In,Out) -->
	{ get_first_parsed(In,Out,First),
	  error_vrml(header(First))
	}.

reading(error_declaration,In,Out) -->
	{ get_first_parsed(In,Out,First),
	  error_vrml(declarations(First))
	}.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred indentation_list(+Parse,-IndList)
:: parse * list(atm)
# "This predcate will construct a list with indentations to be output
   before text. The information of the indentations is inside the parse
   structure.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

indentation_list(In,List) :-
	get_indentation(In,Indent),
	indentation_list_num(Indent,List).

indentation_list_num(0,[]).
indentation_list_num(Nr,['   '|More]) :-
	Nr > 0,
	New is Nr - 1,
	indentation_list_num(New, More).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
count_elements([],0).
count_elements([_|R],Inc) :-
	count_elements(R,New),
	Inc is New + 1.

names_to_steps([],0).
names_to_steps([Name|Rest],Ind) :-
	name(Name,List),
	count_elements(List,Count),
	names_to_steps(Rest,New),
	Ind is Count + New.
	
names_to_indentation_steps(Names,Steps) :-
	names_to_steps(Names,Letters),
	Steps is Letters // 4.
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred open_node(+ParseIn,-ParseOut,-NodeGutsParseStruct,-NodeNameId,L,T)
:: parse * parse * parse * atm * list * list
# "The predicate will open a node to extract its name and its guts.
   The guts will then be added to a new parse structure to be emptied
   in the above module, generator.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

open_node(In,Out,NodeGutsStruct,NodeNameId) -->
	{ get_first_parsed(In,In0,First),
	  decompose_field(First,NodeNameId,[NodeGuts]),
	  inc_indentation(In0,Out),
	  create_parse_structure(NodeGuts,Out,NodeGutsStruct)
	}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred close_node(+ParseNodeStruct,+ParseIn,-ParseOut,L,T)
:: parse * parse * parse * list * list
# "The predicate will end the generation from the node and 
   will do that by adding all the new posts in the dictionary, like
   new declarations and nodes, to the already used dictionary.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

close_node(NodeStruct,In,Out) -->
	{ push_dictionaries(NodeStruct,In,Out0),
	  dec_indentation(Out0,Out)
	}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred close_nodeGut(+ParseIn,-ParseOut,L,T)
:: parse * parse * list * list
# "The predicate will perform all the actions needed terminate the
   reading of the node guts.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

close_nodeGut(In,Out) -->
	{ get_parsed(In,Parsed),
	  Parsed == [],
	  dec_indentation(In,In0),
	  indentation_list(In0,Indent)
	},
	out(Indent),
	out(['}\n']),
	{ inc_indentation(In0,Out)
	}.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred open_PROTO(+Parse,-ProtoParse,L,T)
:: parse * parse * list * list
# "This predicate will construct a parse structure with the prototype
   information, the interface only. It can thereafter be used in further
   code generation. The scene will be opened afterwards.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

open_PROTO(In,DeclIn) -->
	{ look_first_parsed(In,First),
	  decompose_term(First,['PROTO',NodeTypeId,Interface,_Scene]),
	  inc_indentation(In,In0),
	  create_environment(In0,'PROTO',NodeTypeId,Env),
	  create_parse_structure(Interface,In0,DeclIn0),
	  set_environment(Env,DeclIn0,DeclIn),	
	  indentation_list(In,Indent)
	},
	out(Indent),
	out(['PROTO ', NodeTypeId,' [\n']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred start_vrmlScene(+Parse,-ParseScene,L,T)
:: parse * parse * list * list
# "The predicate will construct a parse structure with the prototype
   scene and do the setups.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_vrmlScene(In,SceneIn) -->
	{
	  look_first_parsed(In,First),
	  decompose_term(First,['PROTO',NodeTypeId,Interface,Scene]),
	  inc_indentation(In,In0),
	  create_environment(In0,'PROTO',NodeTypeId,Env),
	  create_parse_structure(Scene,In0,SceneIn0),
	  set_environment(Env,SceneIn0,SceneIn),
	  lookup_set_prototype(In,NodeTypeId,Interface,Scene),
	  indentation_list(In0,Indent)
	},
	out(Indent),
	out(['] {\n']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred close_PROTO(?DeclParse,+SceneParse,+ParseIn,-ParseOut,L,T)
:: parse * parse * parse * parse * list * list
# "The predicate will push the dictionaries with its new information
   when gone through the scenery. The output parse structure will 
   contain all the new information.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	  
close_PROTO(_DeclIn,SceneIn,In,Out) -->
	{ 
	  get_first_parsed(In,In0,_First),
	  push_dictionaries(SceneIn,In0,In1),
	  dec_indentation(In1,Out),
	  indentation_list(Out,Indent)
	},
	out(Indent),
	out(['\n\n']).

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred open_EXTERNPROTO(+Parse,-DeclParse,-StringParse,L,T)
:: parse * parse * parse * list * list
# "This predicate will construct parse structures with the prototype
   information, the interface and the strings. It can thereafter be 
   used in further code generation.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
open_EXTERNPROTO(In,DeclIn,StringIn) -->
	{ look_first_parsed(In,First),
	  decompose_term(First,['EXTERNPROTO',NodeTypeId,Interface,String]),
	  inc_indentation(In,In0),
	  create_environment(In0,'EXTERNPROTO',NodeTypeId,Env),
	  create_parse_structure(Interface,In0,DeclIn0),
	  create_parse_structure(String,In0,StringIn0),
	  set_environment(Env,DeclIn0,DeclIn),	
	  set_environment(Env,StringIn0,StringIn),
	  indentation_list(In,Indent)
	},
	out(Indent),
	out(['EXTERNPROTO ', NodeTypeId,' [\n']).	  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred close_EXTERNPROTO(?ParseDeclIn,?ParseStringIn,+ParseIn,-ParseOut,L,T) 
:: parse * parse * parse * parse * list * list
# "The predicate will end the generating of the external prototype
   and do checkup if there was correct.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

close_EXTERNPROTO(_DeclIn,_StringIn,In,Out) -->
	{ 
	  get_first_parsed(In,In0,First),
	  decompose_term(First,['EXTERNPROTO',NodeTypeId,Interface,String]),
	  lookup_set_prototype(In,NodeTypeId,Interface,String),
	  dec_indentation(In0,Out)
	}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred open_DEF(+ParseIn,-ParseOut,-ParseNode,L,T)
:: parse * parse * parse * list * list
# "The predicate will open and do the settings to generate the
   code for a definition of a node.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

open_DEF(In,Out,NodeIn) -->
	{ 
	  get_first_parsed(In,In0,First),
	  decompose_term(First,['DEF',Name,Node]),
	  lookup_set_def(In0,Name,Node),
	  create_parse_structure(Node,In0,NodeIn0),
	  create_environment(In0,'DEF',Name,Env),
	  set_environment(Env,NodeIn0,NodeIn),
	  indentation_list(In0,Indent),
	  inc_indentation(In0,Out)
	},
	out(Indent),
	out(['DEF ',Name,'\n']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred close_DEF(+ParseNode,+ParseIn,-ParseOut,L,T)
:: parse * parse * parse * list * list
# "The predicate will push the new dictionary information from 
   the node definition to the output parse structure combining
   the information in the old parse structure with the newly received.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	    
close_DEF(NodeIn,In,Out) -->
	{ push_dictionaries(NodeIn,In,Out)
	}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred open_Script(+ParseIn,-ParseOut,-ScriptParse,L,T)
:: parse * parse * parse * list * list
# "The predicate will create a parse structure with the script guts.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

open_Script(In,Out,ScriptIn) -->
	{  get_first_parsed(In,In0,First),
	  decompose_field(First,_Script,[ScriptGuts]),
	  inc_indentation(In0,Out),
	  create_parse_structure(ScriptGuts,Out,ScriptIn)
	}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred close_Script(+ScriptParse,+ParseIn,-ParseOut,L,T)
:: parse * parse * parse * list * list
# "This predicate will update the dictionaries after generating
   code for the script.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

close_Script(ScriptStruct,In,Out) -->
	{ push_dictionaries(ScriptStruct,In,Out0),
	  dec_indentation(Out0,Out)
	}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred remove_comments(+Value,-CommentsBefore,-ValueClean,-CommentsAfter)
:: list(atm) * list(atm) * atm * list(atm)
# "The predicate will remove comments and return the comments
   before and after the pure value.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_comments([],[],[],[]).
remove_comments(List,Before,Value,After) :-
	list(List),
	get_comments(List,Before,[Value|After]).

get_comments([],[],[]).
get_comments([C|Rest],[C|Before],After):-
	atomic(C),
	name(C,[35|_Rest]),
	get_comments(Rest,Before,After).

get_comments([Value|After],[],[Value|After]).

	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	    
	  


%%%%%%%%%%%%%%%%
decompose_field(Field,Name,Guts) :-
	Field =..[Name|Guts].

% get_name(Term,Name) -->
% 	{ get_name(Term,Name) }.

get_name(Term,Name) :-
	decompose_term(Term,[Name|_Rest]).

decompose_term(Term,List) :-
	Term =.. List.
