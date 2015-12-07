:- module(provrml_parser, [parser/2,nodeDeclaration/4,field_Id/1], 
	[assertions,isomodes,dcg,regtypes,iso]).

:- doc(author, "G@..{o}ran Smedb@..{a}ck").

:- use_module(library(lists)).
:- use_module(library(provrml/lookup)).
:- use_module(library(provrml/field_value)).
:- use_module(library(provrml/tokeniser)).
:- use_module(library(provrml/parser_util)).
:- use_module(library(provrml/possible)).
:- use_module(library(provrml/provrmlerror)).

%:- discontiguous([token_read/3]).
%:- set_prolog_flag(multi_arity_warnings, off).
%%%:- use_module(internal_types).
%%%:- use_module(i_o).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred parser(+VRML,-Terms)
	:: string * list(term)
        # "The parser uses a tokeniser to read the input text string of 
           VRML code and returns a list with the corresponding terms.
           The tokens will be read in this parser as the grammar says.
           The parser is according to the specification of the VRML
           grammar, accept that it is performed over tokens in sted of
           the actual code.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parser(VRML, Terms) :-
	tokeniser(VRML, Tokens),
	create_parse_structure(Parse),
       	catch( vrmlScene_first(Parse,Out,Tokens,[]), Msg, output_error(Msg)),
	reverse_parsed(Out,Parse_out),
	get_parsed(Parse_out,Terms),
	!.	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
vrmlScene_first(In,Out) -->
	header(In,Out0),
	vrmlScene(Out0,Out).

%%%%%%%%%%%%%%%%
vrmlScene(In,Out) -->
	declarations(In, Out).

vrmlScene(_In,_Out) -->
	{ error_vrml(declarations) }.

%%%%%%%%%%%%%%%%
%We read comment('#VRML V2.0 utf8') and the following
header(In,Out) -->
	[comment(Header)],
	{name(Header, Header_list),
	 Header_list = [35,86,82,77,76,32,86,50,46,48,32,117,116,102,56|_More],
	 insert_parsed([Header],In,Out)},
	 !.

header(_In,_Out) -->
	look_ahead(Ahead),
	{ error_vrml(header(Ahead)) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
declarations(In,Out) -->
	[],
	{stop_parse(In,Out) }.

declarations(In,Out) -->
	continue(declaration),
	declaration(In,Out0),
	declarations(Out0,Out).

declarations(_In,_Out) -->
	{ error_vrml(declarations) }.

%%%%%%%%%%%%%%%%
declaration(In,Out) -->
	at_least_one(In,Out).%	declaration(In0,Out).

declaration(In, Out) -->
	continue(protoDeclaration),
	protoDeclaration(In, Out).

declaration(In, Out) -->
	continue(routeDeclaration),
	routeDeclaration(In, Out).

declaration(In,Out) -->
	[id('NULL')],
	!,
	fillout(In,In0,Com),
	{insert_parsed(['NULL'|Com],In0, Out)}.

declaration(In, Out) -->
	nodeDeclaration(In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred nodeDeclaration(+Parse_in,-Parse_out,L,T)
	:: parse * parse * list * list
        # "The predicate is also accepted as a node field as has to be 
           accessed from the module that reads field values, i.e., 
           field_value.pl".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nodeDeclaration(In,Out) -->
	continue(nodeDeclaration),
	[id('DEF')],
	!,
	nodeDeclaration_DEF(In,Out).

nodeDeclaration(In,Out) -->
	continue(nodeDeclaration),
	[id('USE')],
	 !,
	nodeDeclaration_USE(In,Out).

nodeDeclaration(In,Out) -->
	continue(node),
	node(In,Out).

nodeDeclaration(In,Out) -->
	at_least_one(In,In0),
	nodeDeclaration(In0,Out).

nodeDeclaration(_,_) -->
	look_ahead(Ahead),
	{ error_vrml(nodeDeclaration(Ahead)) }.


%%%%%%%%%%%%%%%%
nodeDeclaration_DEF(In,Out) -->
	fillout(In,In0,Comment),
	nodeNameId(NodeNameId),
	{ create_parse_structure(In0,P) },
	node(P,Node_parsed),
	% DEF inside a PROTO cannot be accessed from outside 
	% DEF outside a PROTO cannot be accessed from a PROTO
	{ get_parsed(Node_parsed,Node),
	  lookup_set_def(In0,NodeNameId,Node),
	  push_whitespace(Node_parsed,In0,In1),
	  correct_commenting(before,Comment,Node,Filling),
	  insert_parsed(['DEF'(NodeNameId,Filling)],In1,Out) },
	  !.

nodeDeclaration_DEF(_In,_Out) -->
	look_ahead(Ahead),
	{ error_vrml(nodeNameId_DEF(Ahead)) }.

%%%%%%%%%%%%%%%%
nodeDeclaration_USE(In,Out) -->
	fillout(In,In0,C0),
	nodeNameId(NodeNameId),
	fillout(In0,In1,C1),
	% is the nodeNameId possible to access inside/outside
	{ append(C0,C1,Comments),
	  correct_commenting(after,Comments,NodeNameId,NodeNameIdComments),
	  insert_parsed(['USE'(NodeNameIdComments)],In1,Out)}.

nodeDeclaration_USE(_In,_Out) -->
	look_ahead(Ahead),
	{ error_vrml(nodeNameId_USE(Ahead)) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
protoDeclaration(In, Out) -->
	proto(In, Out).

protoDeclaration(In, Out) -->
	externproto(In, Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
proto(In, Out) -->
	[id('PROTO')],
	proto_NodeTypeId(In,Out).

%%%%%%%%%%%%%%%%
proto_NodeTypeId(In,Out) -->
	fillout(In,In0,Com),
	nodeTypeId(NodeTypeId),
	!,
	proto_parenthesis_list_open(In0,Out,NodeTypeId,Com).

proto_NodeTypeId(_In,_Out) -->
	look_ahead(Ahead),
	{ error_vrml(proto_NodeTypeId(Ahead)) }.


%%%%%%%%%%%%%%%%
proto_parenthesis_list_open(In,Out,NodeTypeId,Com) -->
	fillout(In,In0,Comment0),
	[parenthesis_list_open],
	!,
	fillout(In0,In1,Comment1),
	{create_environment(In1,'PROTO',NodeTypeId,Env),
	 set_environment(Env,In,In0) },
	 fillout(In0,In1,Comment1),
	{ append(Com,Comment0,Com0),
	 append(Com0,Comment1,Comments) },
	 proto_interfaceDeclarations(In1,Out,NodeTypeId,Comments).

proto_parenthesis_list_open(_In,_Out,NodeTypeId,_Com) -->
	look_ahead(Ahead),
	{ error_vrml(parenthesis_list_open(NodeTypeId,Ahead)) }.

%%%%%%%%%%%%%%%%
proto_interfaceDeclarations(In,Out,NodeTypeId,Comments_front) -->
	{create_parse_structure(In,P)},
	interfaceDeclarations(P,InterfaceDecl_parsed),
	fillout(In,In0,Comments_back),
	[parenthesis_list_close],
         % Set proto entry in a special dictionary
	 % then must the IS in ProtoType match this interface and this only.
	{ push_whitespace(InterfaceDecl_parsed,In0,In1),
	  reverse_parsed(InterfaceDecl_parsed,InterfaceDecl_parsed_rev),
	  get_parsed(InterfaceDecl_parsed_rev,InterfaceDecl) },
	  proto_definition(In1,Out,NodeTypeId,Comments_front,InterfaceDecl,
	                                      Comments_back).

proto_interfaceDeclarations(_In,_Out,NodeTypeId,_Com) -->
	{ error_vrml(interfaceDeclaration(NodeTypeId)) }.

%%%%%%%%%%%%%%%%
proto_definition(In,Out,NodeTypeId,Com_front,InterfaceDecl,Comments_back) -->
	fillout(In,In0,Com),
	[parenthesis_node_open],
	!,
	fillout(In0,In1,Com1),
	{append(Com,Com1,Comments),
	 append(Comments_back,Comments,Com_back)},
	proto_vrmlScene(In1,Out,NodeTypeId,Com_front,InterfaceDecl,Com_back).

proto_definition(_In,_Out,NodeTypeId,_C,_I,_C) -->
	look_ahead(Ahead),
	{ error_vrml(parenthesis_node_open(NodeTypeId,Ahead)) }.

%%%%%%%%%%%%%%%%
proto_vrmlScene(In,Out,NodeTypeId,C_front,InterfaceDecl,C_back) -->
	{create_parse_structure(In,P)},
	vrmlScene(P,ProtoType_parsed),
	[parenthesis_node_close],
	{ get_parsed(ProtoType_parsed,ProtoType),
	  lookup_set_prototype(In,NodeTypeId,InterfaceDecl,ProtoType),
	  append(C_front,InterfaceDecl,Com_Int),
	  append(C_back,ProtoType,Com_Pro),
	 insert_parsed(['PROTO'(NodeTypeId,Com_Int,Com_Pro)],In,Out)}.


proto_vrmlScene(_In,_Out,NodeTypeId,_C_front,_InterfaceDecl,_C_back) -->
	{ error_vrml(proto_vrmlScene(NodeTypeId)) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
interfaceDeclarations(In, Out) -->
	interfaceDeclaration(In, Out).

interfaceDeclarations(In, Out) -->
	continue(interfaceDeclaration),
	interfaceDeclaration(In, Out0),
	interfaceDeclarations(Out0, Out).

interfaceDeclarations(In,_Out) -->
	look_ahead(Ahead),
	{ get_environment(In,Env),
	  get_environment_name(Env,ProtoName), 
	  error_vrml(parenthesis_list_close(ProtoName,Ahead)) }.

%%%%%%%%%%%%%%%%
interfaceDeclaration(In, Out) -->
	restrictedInterfaceDeclaration(In, Out),
	!.

interfaceDeclaration(In, Out) -->
	[id(exposedField)],
	!,
	interfaceDeclaration_fieldTypeId(In,Out,exposedField).

interfaceDeclaration(In,Out) -->
	at_least_one(In,In0),
	interfaceDeclaration(In0,Out).

interfaceDeclaration(In,_Out) -->
	look_ahead(Ahead),
	{ get_environment(In,Env),
	  get_environment_name(Env,ProtoName),
	  error_vrml(interfaceDeclaration(ProtoName,Ahead)) }.

%%%%%%%%%%%%%%%%
interfaceDeclaration_fieldTypeId(In,Out,FieldAccess) -->
	fillout(In,In0,C0),
	fieldTypeId(FieldType),
	{ lookup_fieldTypeId(FieldType) },
	fillout(In0,In1,C1),
	{ append(C0,C1,Comments) },
	interfaceDeclaration_fieldId(In1,Out,FieldType,Comments,FieldAccess).

interfaceDeclaration_fieldTypeId(In,_Out,FieldAcc) -->
	{get_row_number(In,Row),
	 get_environment(In,Env),
	 get_environment_name(Env,ProtoName),
	 error_vrml(interfaceDeclaration_fieldTypeId(ProtoName,FieldAcc,Row))}.

%%%%%%%%%%%%%%%%
interfaceDeclaration_fieldId(In,Out,FieldType,Com,FieldAccess) -->
	fieldId(FieldId),
	fillout(In,In0,Comments),
	{ correct_commenting(after,Comments,FieldId,FieldIdComments)
	},
	interfaceDeclaration_fieldValue(In0,Out,FieldType,Com,FieldIdComments,
	                                        FieldAccess).

interfaceDeclaration_fieldId(In,_Out,FieldType,_Com,FieldAcc) -->
	{get_row_number(In,Row),
	 get_environment(In,Env),
	 get_environment_name(Env,ProtoName),
	 error_vrml(interfaceDeclaration_fieldId(ProtoName,FieldType,
	                                         FieldAcc,Row))}.

%%%%%%%%%%%%%%%%
interfaceDeclaration_fieldValue(In,Out,FieldType,Comments,FieldId,FieldAcc) -->
	fieldValue(In,In0,FieldType,FieldValue),
	{ correct_commenting(after,Comments,FieldType,FieldTypeComments),
	  create_field(FieldAcc,FieldTypeComments,FieldId,FieldValue,Field),
	  insert_parsed([Field],In0,Out) }.

interfaceDeclaration_fieldValue(In,_Out,FieldType, _C,FieldId, FieldAcc) -->
	{get_row_number(In,Row),
	 get_environment(In,Env),
	 get_environment_name(Env,ProtoName),
	 error_vrml(interfaceDeclaration_fieldValue(ProtoName,FieldType, 
	                                            FieldId,FieldAcc,Row))}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
restrictedInterfaceDeclaration(In,Out) -->
	[id(eventIn)],
	!,
	restrictedInterfaceDeclaration_fieldTypeId(In,Out,eventIn).

restrictedInterfaceDeclaration(In,Out) -->
	[id(eventOut)],
	!,
	restrictedInterfaceDeclaration_fieldTypeId(In,Out,eventOut).

restrictedInterfaceDeclaration(In,Out) -->
	[id(field)],
	!,
	interfaceDeclaration_fieldTypeId(In,Out,field).
	
%%%%%%%%%%%%%%%%
restrictedInterfaceDeclaration_fieldTypeId(In,Out,FieldAccess) -->
	fillout(In,In0,C0),
	fieldTypeId(FieldType),
	{ lookup_fieldTypeId(FieldType) },
	fillout(In0,In1,C1),
	{ append(C0,C1,Comments),
	  correct_commenting(after,Comments,FieldType,FieldTypeComments)
	},
	restrictedInterfaceDeclaration_fieldId(In1,Out,FieldTypeComments,
	                                               FieldAccess).

restrictedInterfaceDeclaration_fieldTypeId(In,_Out,FieldAcc) -->
	{get_row_number(In,Row),
	 get_environment(In,Env),
	 get_environment_name(Env,ProtoName),
	 error_vrml(interfaceDeclaration_fieldTypeId(ProtoName,FieldAcc,Row))}.

%%%%%%%%%%%%%%%%
%Well logically it is a bit ugly, they are event[In,Out]Id.
restrictedInterfaceDeclaration_fieldId(In,Out,FieldType,FieldAccess) -->
	fieldId(FieldId),
	fillout(In,In0,Comments),
	{ correct_commenting(after,Comments,FieldId,FieldIdComments),
	  create_field(FieldAccess,FieldType,FieldIdComments,Field),
	  insert_parsed([Field],In0,Out) }.

restrictedInterfaceDeclaration_fieldId(In,_Out,FieldType, FieldAcc) -->
	{get_row_number(In,Row),
	 get_environment(In,Env),
	 get_environment_name(Env,ProtoName),
	 error_vrml(interfaceDeclaration_fieldId(ProtoName,FieldType,
	                                         FieldAcc,Row))}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
externproto(In, Out) -->
	[id('EXTERNPROTO')],
	externproto_NodeTypeId(In,Out).

%%%%%%%%%%%%%%%%
externproto_NodeTypeId(In,Out) -->
	fillout(In,In0,Com),
	nodeTypeId(NodeTypeId),
	externproto_parenthesis_list_open(In0,Out,NodeTypeId,Com),
	!.

externproto_NodeTypeId(_In,_Out) -->
	look_ahead(Ahead),
	{ error_vrml(proto_NodeTypeId(Ahead)) }.
%OBS

%%%%%%%%%%%%%%%%
externproto_parenthesis_list_open(In,Out,NodeTypeId,Com) -->
	fillout(In,In0,Comment0),
	[parenthesis_list_open],
	!,
	fillout(In0,In1,Comment1),
	{create_environment(In1,'EXTERNPROTO',NodeTypeId,Env),
	 set_environment(Env,In,In0) },
	 fillout(In0,In1,Comment1),
	{ append(Com,Comment0,Com0),
	 append(Com0,Comment1,Comments) },
	externproto_interfaceDeclarations(In1,Out,NodeTypeId,Comments).

externproto_parenthesis_list_open(_In,_Out,NodeTypeId,_Com) -->
	look_ahead(Ahead),
	{ error_vrml(parenthesis_list_open(NodeTypeId,Ahead)) }.

%%%%%%%%%%%%%%%%
externproto_interfaceDeclarations(In,Out,NodeTypeId,Comments_front) -->
	{create_parse_structure(In,P)},
	externInterfaceDeclarations(P,InterfaceDecl_parsed),
	fillout(In,In0,Comments_back),
	[parenthesis_list_close],
	 % Set proto entry in a special dictionary
	 % then must the IS in ProtoType match this interface and this only.
	{ push_whitespace(InterfaceDecl_parsed,In0,In1),
	  reverse_parsed(InterfaceDecl_parsed,InterfaceDecl_parsed_rev),
	  get_parsed(InterfaceDecl_parsed_rev,EID) },
	  externproto_mfstringValue(In1,Out,NodeTypeId,Comments_front,EID,
	                                      Comments_back).

externproto_interfaceDeclarations(_In,_Out,NodeTypeId,_Comments_front) -->
	{ error_vrml(interfaceDeclaration(NodeTypeId)) }.

%%%%%%%%%%%%%%%%
% Do a checkup!!!!!!
externproto_mfstringValue(In,Out,NodeTypeId,Com_front,EID,Com_back) -->
	mfstringValue(In,In0,MFString), 
	{ correct_commenting(before,Com_front,EID,EID_0),
	  correct_commenting(after,Com_back,EID_0,EID_out),
	  lookup_set_extern_prototype(In0,NodeTypeId,EID,MFString),
	  create_field('EXTERNPROTO',NodeTypeId,EID_out,MFString,Field),
	 insert_parsed([Field],In0,Out)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
externInterfaceDeclarations(In, Out) -->
	externInterfaceDeclaration(In, Out).

externInterfaceDeclarations(In, Out) -->
	continue(externInterfaceDeclaration),
	externInterfaceDeclaration(In, Out0),
	externInterfaceDeclarations(Out0, Out).

externInterfaceDeclarations(In,_Out) -->
	look_ahead(Ahead),
	{ get_environment(In,Env),
	  get_environment_name(Env,ProtoName), 
	  error_vrml(parenthesis_list_close(ProtoName,Ahead)) }.


%%%%%%%%%%%%%%%%
externInterfaceDeclaration(In,Out) -->
	[id(eventIn)],
	!,
	externInterfaceDeclaration_fieldTypeId(In,Out,eventIn),!.

externInterfaceDeclaration(In,Out) -->
	[id(eventOut)],
	!,
	externInterfaceDeclaration_fieldTypeId(In,Out,eventOut),!.

externInterfaceDeclaration(In,Out) -->
	[id(field)],
	!,
	 externInterfaceDeclaration_fieldTypeId(In,Out,field),!.

externInterfaceDeclaration(In,Out) -->
	[id(exposedField)],
	!,
	externInterfaceDeclaration_fieldTypeId(In,Out,exposedField),!.
	
externInterfaceDeclaration(In,Out) -->
	at_least_one(In,In0),
	externInterfaceDeclaration(In0,Out).

externInterfaceDeclaration(_In,_Out) -->
	{ error_vrml(externInterfaceDeclaration) }.

%%%%%%%%%%%%%%%%
externInterfaceDeclaration_fieldTypeId(In,Out,FieldAccess) -->
	fillout(In,In0,C0),
	fieldTypeId(FieldType),
	{ lookup_fieldTypeId(FieldType) },
	!,
	fillout(In0,In1,C1),
	{ append(C0,C1,Comments),
          correct_commenting(after,Comments,FieldType,FieldTypeComments) },
	  externInterfaceDeclaration_fieldId(In1,Out,FieldTypeComments,
	                                               FieldAccess).

externInterfaceDeclaration_fieldTypeId(In,_Out,FieldAcc) -->
	{get_row_number(In,Row),
	 get_environment(In,Env),
	 get_environment_name(Env,ProtoName),
	 error_vrml(interfaceDeclaration_fieldTypeId(ProtoName,FieldAcc,Row))}.

%%%%%%%%%%%%%%%%
%Well logically it is a bit ugly, they are event[In,Out]Id.
externInterfaceDeclaration_fieldId(In,Out,FieldType,FieldAccess) -->
	fieldId(FieldId),
	fillout(In,In0,Comments),
	{ correct_commenting(after,Comments,FieldId,FieldIdComments),
	  create_field(FieldAccess,FieldType,FieldIdComments,Field),
	  insert_parsed([Field],In0,Out) }.

externInterfaceDeclaration_fieldId(In,_Out,FieldType, FieldAcc) -->
	{get_row_number(In,Row),
	 get_environment(In,Env),
	 get_environment_name(Env,ProtoName),
	 error_vrml(interfaceDeclaration_fieldId(ProtoName,FieldType,
	                                         FieldAcc,Row))}.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
routeDeclaration(In,Out) -->
	[id('ROUTE')],
	routeDeclaration_from(In,Out).

%%%%%%%%%%%%%%%%
routeDeclaration_from(In,Out) -->
	fillout(In,In0,Comment0),
	nodeNameId(NodeNameIdFrom),
	{ ( Comment0 == []
	  ->From = NodeNameIdFrom
	  ; append(Comment0,[NodeNameIdFrom],From)
	  ) },
	[symbol('.')],
	eventOutId(EventOutId),
	fillout(In0,In1,Comment1),
	{ ( Comment1 == []
	  ->Event = EventOutId
	  ; append([EventOutId],Comment1,Event)
	  ) },
	routeDeclaration_to(In1,Out,From,Event),
	!.

routeDeclaration_from(_In,_Out) -->
	look_ahead(Ahead),
	{ error_vrml(route_from(Ahead)) }.

%%%%%%%%%%%%%%%%
routeDeclaration_to(In,Out,From,EventOut) -->
	[id('TO')],
	fillout(In,In0,Comment0),
	nodeNameId(NodeNameIdTo),
	{ ( Comment0 == []
	  ->To = NodeNameIdTo
	  ; append(Comment0,[NodeNameIdTo],To)
	  ) },
	[symbol('.')],
	eventInId(EventInId),
	fillout(In0,In1,Comment1),
	{ ( Comment1 == []
	  ->EventIn = EventInId
	  ; append([EventInId],Comment1,EventIn)
	  ),
	insert_parsed(['ROUTE'(From,EventOut,'TO',To,EventIn)],In1, Out)},
	!.

routeDeclaration_to(_In,_Out,From,Event) -->
	look_ahead(Ahead),
	{ error_vrml(route_to(From,Event,Ahead)) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
node(In,Out) -->
	[id('Script')],
	!,
	node_Script(In,Out).


node(In,Out) -->
	fillout(In,In0,Com0),
	nodeTypeId(NodeTypeId),
	fillout(In0,In1,Com1),
	{append(Com0,Com1,Comment)},
	node_typeId(In1,Out,NodeTypeId,Comment).

%%%%%%%%%%%%%%%%
node_Script(In,Out) -->
	fillout(In,In0,Comment),
	[parenthesis_node_open],
	node_Script_parenthesis_open(In0,Out,Comment).

node_Script(_In,_Out) -->
	look_ahead(Ahead),
	{ error_vrml(parenthesis_node_open('Script',Ahead)) }.

%%%%%%%%%%%%%%%%
node_Script_parenthesis_open(In,Out,CommentIn) -->
	{create_parse_structure(In,P)},
	fillout(In,In0,Com0),
	scriptGuts(P,ScriptGuts),
	[parenthesis_node_close],
	{ append(CommentIn,Com0,Comment),
	  reverse_parsed(ScriptGuts,Guts0),
	  ( Comment == []
	  ->GutsOut = Guts0
	  ; insert_comments_in_beginning(Comment,Guts0,GutsOut)
	  ),
	 create_node('Script',GutsOut,Node),
	 insert_parsed([Node],In0,Out)}.

node_Script_parenthesis_open(_In,_Out,_CommentIn) -->
	{ error_vrml(scriptGuts) }.

%%%%%%%%%%%%%%%%

node_typeId(In,Out,NodeTypeId,Comment) -->
	[parenthesis_node_open],
	node_parenthesis_open(In,Out,NodeTypeId,Comment).

node_typeId(_In,_Out,NodeTypeId,_Comment) -->
	look_ahead(Ahead),
	{ error_vrml(parenthesis_node_open(NodeTypeId,Ahead)) }.

%%%%%%%%%%%%%%%%
node_parenthesis_open(In,Out,NodeTypeId,CommentIn) -->
	{create_parse_structure(In,P)},
	fillout(In,In0,Com0),
	nodeGuts(NodeTypeId,P,Guts),
	[parenthesis_node_close],
	{ append(CommentIn,Com0,Comment),
	  reverse_parsed(Guts,Guts0),
	  ( Comment == []
	  ->GutsOut = Guts0
	  ; insert_comments_in_beginning(Comment,Guts0,GutsOut)
	  ),
	  create_node(NodeTypeId,GutsOut,Node),
	  insert_parsed([Node],In0,Out)}.

node_parenthesis_open(_In,_Out,NodeTypeId,_CommentIn) -->
	{ error_vrml(nodeGuts(NodeTypeId)) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nodeGuts(_NodeTypeId,In,Out) -->
	[],
	 {stop_parse(In,Out)}.

nodeGuts(NodeTypeId,In,Out) -->
	continue(nodeGuts),
	nodeGut(NodeTypeId,In,Out0),
	nodeGuts(NodeTypeId,Out0,Out).

nodeGuts(NodeTypeId,In,Out) -->
	at_least_one(In,In0),
	nodeGuts(NodeTypeId,In0,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
scriptGuts(In, Out) -->
	[],
	{stop_parse(In,Out)}.

scriptGuts(In,Out) -->
	scriptGut(In,Out0),
	scriptGuts(Out0,Out).

scriptGuts(In,Out) -->
	at_least_one(In,In0),
	scriptGuts(In0,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%I am taking them as equal when reading the ID (field,eventIn,eventOut).
nodeGut(NodeTypeId,In,Out) -->
	{inside_proto(In)},
	fieldId(FieldId0),
	fillout(In,In0,Comment),
	[id('IS')],
	nodeGut_IS(In0,Out,NodeTypeId,FieldId0,Comment).

nodeGut(_NodeTypeId,In,Out) -->
	routeDeclaration(In,Out).

nodeGut(_NodeTypeId,In,Out) -->
	protoDeclaration(In,Out).

nodeGut(NodeTypeId,In,Out) -->
	fieldId(FieldId),
	nodeGut_fieldValue(In,Out,NodeTypeId,FieldId).

nodeGut(NodeTypeId,In,Out) -->
	at_least_one(In,In0),
	nodeGut(NodeTypeId,In0,Out).

%%%%%%%%%%%%%%%%
nodeGut_IS(In,Out,_NodeTypeId,FieldId0,Comment) -->
	fillout(In,In0,Comment0),
	fieldId(FieldId1),
	fillout(In0,In1,Comment1),
	{ correct_commenting(before,Comment,FieldId0,Id0),
	  correct_commenting(before,Comment0,FieldId1,FieldId11),
	  correct_commenting(after,Comment1,FieldId11,Id1),	
	  insert_parsed([Id0,'IS',Id1],In1,Out)},
	  !.

nodeGut_IS(_In,_Out,NodeTypeId,FieldId0,_Comment) -->
	{ error_vrml(nodeGut_IS(NodeTypeId,FieldId0)) }.

%%%%%%%%%%%%%%%%
nodeGut_fieldValue(In,Out,NodeTypeId,FieldId) -->
	{lookup_get_fieldType(In, NodeTypeId, FieldId, FieldType) },
	 ( { FieldType == undefined }
        -> { error_vrml(fieldType_undefined(NodeTypeId,FieldId)) }
	 ; fieldValue(In,In1,FieldType,FieldValue),
	   { create_field(FieldId,FieldValue,Field),
	     insert_parsed([Field],In1,Out)}
	 ),
	 !.

nodeGut_fieldValue(_In,_Out,NodeTypeId,FieldId) -->
	{ error_vrml(nodeGut_fieldValue(NodeTypeId,FieldId)) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% With the equality between the 'IS' cases and the restricted cases, you can
% not indicate an error before the 'IS'. Or should we read the restricted 
% before.

scriptGut(In,Out) -->
	restrictedInterfaceDeclaration(In,Out).

scriptGut(In,Out) -->
	[id('eventIn')],
	scriptGut_access(In,Out,eventIn).

scriptGut(In,Out) -->
	[id('eventOut')],
	 scriptGut_access(In,Out,eventOut).

scriptGut(In,Out) -->
	[id('field')],
	 scriptGut_access(In,Out,field).

scriptGut(In,Out) -->
	nodeGut('Script',In,Out).

scriptGut(In,Out) -->
	restrictedInterfaceDeclaration(In,Out).

%%%%%%%%%%%%%%%%
scriptGut_access(In,Out,AccessType) -->
	fillout(In,In0,Comment0),
	fieldTypeId(FieldType),
	fillout(In0,In1,Comment1),
	{ correct_commenting(before,Comment0,FieldType,FieldType0),
	  correct_commenting(after,Comment1,FieldType0,FieldType1)
	},
	scriptGut_fieldId(In1,Out,AccessType,FieldType1).

scriptGut_access(_In,_Out,AccessType) -->
	{ error_vrml(scriptGut_fieldType(AccessType)) }.

%%%%%%%%%%%%%%%%
scriptGut_fieldId(In,Out,AccessType,FieldType) -->
	fieldId(FieldId),
	fillout(In,In0,Comment),
	{ correct_commenting(after,Comment,FieldId,FieldId0) },
	scriptGut_IS(In0,Out,AccessType,FieldType,FieldId0).

scriptGut_fieldId(_In,_Out,AccessType,FieldType) -->
	{ error_vrml(scriptGut_fieldId(AccessType,FieldType)) }.

%%%%%%%%%%%%%%%%
scriptGut_IS(In,Out,Acc,FieldType,FieldId0) -->
	[id('IS')],
	fillout(In,In0,Comment0),
	fieldId(FieldId1),
	fillout(In0,In1,Comment1),
	{ correct_commenting(before,Comment0,FieldId1,FieldId10),
	  correct_commenting(after,Comment1,FieldId10,Target),
	  create_directed_field(Acc,FieldType,FieldId0,Target,Field),
	  insert_parsed([Field],In1,Out)},
	!.

scriptGut_IS(_In,_Out,Acc,FieldType,FieldId) -->
	{ error_vrml(scriptGut_IS(Acc,FieldType,FieldId)) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nodeNameId(Id) -->
	id(Id).

nodeTypeId(Id) -->
	id(Id).

fieldTypeId(Id) -->
	id(Id).

fieldId(Id) -->
	id(Id).

eventInId(Id) -->
	id(Id).

eventOutId(Id) -->
	id(Id).

id(Id) -->
	[id(Id)].


:- true prop field_Id/1.
field_Id(_).
