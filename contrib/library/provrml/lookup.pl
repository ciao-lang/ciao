:- module(lookup, [
	create_proto_element/3,
	get_prototype_interface/2,
	get_prototype_definition/2,
	%lookup_eventIn/3,
	%lookup_eventOut/3,
	%%%%%%%%%%%%%%%% DCG
	lookup_check_node/4,
	lookup_check_field/6,
	lookup_check_interface_fieldValue/8,
	%%%%%%%%%%%%%%%%
	lookup_field/4,
	lookup_route/5,
	lookup_fieldTypeId/1,
	lookup_get_fieldType/4,
	lookup_field_access/4,
	lookup_set_def/3,
	lookup_set_prototype/4,
	lookup_set_extern_prototype/4],[assertions, isomodes, dcg, iso]).

:- doc(author, "G@..{o}ran Smedb@..{a}ck").

%:- include(library(types)).

%:- use_module(library(basicprops)).
:- use_module(library(provrml/provrmlerror)).
:- use_module(library(provrml/internal_types), [bound/1]).
:- use_module(library(provrml/provrml_io)).
:- use_module(library(provrml/parser_util)).
:- use_module(library(provrml/dictionary)).
:- use_module(library(provrml/dictionary_tree)).
:- use_module(library(provrml/field_value_check), 
        [fieldValue_check/8]).
:- use_module(library(provrml/boundary), 
        [children_nodes/1]).
:- use_module(library(provrml/generator_util), 
        [decompose_field/3,
         indentation_list/2]).
:- use_module(library(provrml/field_type), 
        [fieldType/1]).
:- use_module(library(provrml/field_value), [parse/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%More checks could be done: the nodes has only a fixed set of 
%possible children nodes. In the dictionary module under each node there
%is the group name or the possible children nodes. The groupname 
%has its full list of possible children nodes in boundary.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred lookup_check_field(+ParseIn,-ParseOut,+NodeTypeId,+Field,L,T)
   :: parse * parse * atm * term * list * list
   # "The predicate will create some output through the DCG and the 
      output command out/3. There will be formatting and the most
      important part there will be a check of the field type and of its
      values so that they correspond to the type.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup_check_field(In,Out,NodeTypeId,Field) -->
	{
	%write(NodeTypeId),nl,write(Field),nl,
	decompose_field(Field,FieldId,Guts),
	lookup_get_fieldType(In,NodeTypeId,FieldId,FieldType)
	},
	( { FieldType == undefined }
	-> { error_vrml(fieldType_undefined(NodeTypeId,FieldId)) }
	;  { lookup_get_boundary(NodeTypeId, FieldId, InitValue, Boundaries),
	     indentation_list(In,Indent)
	   },
	   ( ( { FieldType == 'SFNode'
	       ; FieldType == 'MFNode' }
	     )
	   -> out(Indent),
	      out([FieldId,' '])
	   ;  out(Indent),
	      out([FieldId,' '])
	   ), 
	   (  { FieldId == children }
	   -> { inc_indentation(In,In0),
	        indentation_list(In,NodeIndent) },
		out(['\n']),
		out(NodeIndent),
		fieldValue_check(FieldType,Guts,In0,In1,InitValue,Boundaries),
	        { dec_indentation(In1,Out) }
	   ;  fieldValue_check(FieldType,Guts,In,Out,InitValue,Boundaries)
	   )
	).
	  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred lookup_check_node(+ParseIn,+NodeTypeId,L,T)
   :: parse * atm * list * list
   # "The predicate will check so that the node is of an acceptable type.
      If the node name is not found in the ordinary dictionary then the 
      secondary dictionary is consulted, the personal one. Then the node
      have to be a Prototype, Externproto or a Defined one.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup_check_node(In,NodeTypeId) -->
	{
	dictionary(NodeTypeId, _FieldAccess, _FieldType, 
	                   _FieldId, _InitValue, _Boundaries),
	children_nodes(CN)
	},
	( { member(NodeTypeId,CN) }
	->{ indentation_list(In,Indent_list) },
	  out(Indent_list),
	  out([NodeTypeId,' {\n'])
	; out([NodeTypeId,' {\n'])
	),
	!.

lookup_check_node(In,NodeTypeId) -->
	{
	get_dictionaries(In,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	dictionary_lookup(NodeTypeId,'PROTO',_Info,Proto_dic,Status),
	Status == defined,
	indentation_list(In,Indent_list) 
        },
	out(Indent_list),
	out([NodeTypeId,' {\n']).
	
lookup_check_node(In,NodeTypeId) -->
	{
	get_dictionaries(In,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	dictionary_lookup(NodeTypeId,'PROTO',_Info,Proto_dic,Status),
	Status == defined,
	indentation_list(In,Indent_list) },
	out(Indent_list),
	out([NodeTypeId,' {\n']).
	  
lookup_check_node(In,NodeTypeId) -->
	{
	get_dictionaries(In,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	dictionary_lookup(NodeTypeId,'DEF',_Info,Proto_dic,Status0),
	Status0 == defined,
        indentation_list(In,Indent_list) 
        },
	out(Indent_list),
	out([NodeTypeId,' {\n']).

lookup_check_node(_In,NodeTypeId) -->
	{ error_vrml(nodeTypeId(NodeTypeId)) }.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred lookup_check_interface_fieldValue(+ParseIn,-ParseOut,+AccessType,
                                          +FieldType,+Id,+FieldValue,DCGIn,DCGOut)
:: parse * parse * atm * term * atm * term * string * string
 # "The predicate formats the output for the interface part of the prototype.
    It also checks the values for the fields.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup_check_interface_fieldValue(In,Out,AccessType,FieldType,Id,FieldValue)-->
	{
	strip_clean(FieldType,FieldType_clean),
	indentation_list(In,Indent) 
	},
        out(Indent),
	out([AccessType,' ',FieldType,' ',Id,' ']),
        fieldValue_check(FieldType_clean,FieldValue,In,Out,_InitValue,[]),
	out(['\n']).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred lookup_fields(+ParseIn,+NodeTypeId,-AccessType,-FieldType,+Id,-Init,-Bound)
:: parse * atm * atm * atm * atm * term * bound
 # "The predicate get the field properties, from the dictionary which is in the parse
    structure we look for the FieldId for the given NodeTypeId. We get the boundaries
    and the accessType and the FieldType. ".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup_fields(In,NodeTypeId,FieldAccess,FieldType,FieldId,Init,Bound) :-
	lookup_get_fieldType(In,NodeTypeId,FieldId,FieldType),
	( FieldType == undefined
	-> true
	;  lookup_get_boundary(NodeTypeId,FieldId,Init,Bound),
	   lookup_access(In,NodeTypeId,FieldId,FieldAccess)
	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred lookup_get_boundary(+NodeTypeId,+FieldId,-InitValue,-Boundary)
:: atm * atm * term * term
# "The predicate will return the init values and the bound values for an ordinary
   NodeType. That is, if the Field is not included in the normal setup of fields
   we set the bound to an empty list.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup_get_boundary(NodeTypeId, FieldId, InitValue, Boundaries) :-
	dictionary(NodeTypeId, _FieldAccess, _FieldType, 
	                   FieldId, InitValue, Boundaries).

lookup_get_boundary(_NodeTypeId, _FieldId, _InitValue,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred lookup_get_fieldType(+Parse,+NodeTypeId,+FieldId,-FieldType)
:: parse * atm * atm * atm
# "The predicate will return the given field's type. It will start the search
   in the ordinar dictionary and then to the personal dictionary sarting off with 
   'PROTO'. After it will go for 'DEF' and 'EXTERNPROTO'.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup_get_fieldType(_Parse_struct, NodeTypeId, FieldId, FieldType) :-
	dictionary(NodeTypeId, _FieldAccess, FieldType, 
	                   FieldId, _InitValue, _Boundaries),
	!.

lookup_get_fieldType(Parse_struct, NodeTypeId, FieldId, FieldType) :-
	get_dictionaries(Parse_struct,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	dictionary_lookup(NodeTypeId,'PROTO',Info,Proto_dic,Status),
	Status == defined,
	lookup_fieldType_defined(NodeTypeId,FieldId,FieldType,Info).
	
	
lookup_get_fieldType(Parse_struct, NodeTypeId, FieldId, FieldType) :-
	get_dictionaries(Parse_struct,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	dictionary_lookup(NodeTypeId,'DEF',Info,Proto_dic,Status),
	Status == defined,
	get_node_name(Info,Name),
	( Name == 'Script'
	-> get_node_guts(Info,Interface),write(Interface),nl,
	   strip_from_list(Interface,[Inter_Str]),
	   lookup_interface(Inter_Str,_Acc,FieldType,FieldId,StatInter),
	   ( StatInter == defined
	   -> true
	   ;  error_vrml(fieldType_undefined(NodeTypeId,FieldId))
	   )
	; dictionary(Name, _FieldAccess, FieldType, 
	                   FieldId, _InitValue, _Boundaries)
	).

lookup_get_fieldType(Parse_struct, NodeTypeId, FieldId, FieldType) :-
	get_dictionaries(Parse_struct,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	dictionary_lookup(NodeTypeId,'EXTERNPROTO',Info,Proto_dic,Status),
	Status == defined,
	lookup_fieldType_defined(NodeTypeId,FieldId,FieldType,Info).

lookup_get_fieldType(_Parse, _NodeTypeId, _FieldId, undefined).

lookup_fieldType_defined(_NodeTypeId,FieldId,FieldType,Info) :-
	get_prototype_interface(Info,Interface),
	lookup_interface(Interface,_Access,FieldType,FieldId,Status_interface),
	Status_interface == defined.

lookup_fieldType_defined(NodeTypeId,FieldId,_FieldType,_Info) :-
	error_vrml(fieldType_undefined(NodeTypeId,FieldId)),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred lookup_field(+Parse,+FieldTypeId,+FieldId0,+FieldId1)
:: parse * atm * atm * atm
# "The predicate will control that the two connected Fields are of the 
   same type, e.g., SFColor - SFColor, MFVec3f - MFVec3f. ".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup_field(In,NodeTypeId0, FieldId0,FieldId1) :-
	dictionary(NodeTypeId0, _, FieldType0,
	            FieldId0, _InitValue, _Boundaries), 
	get_environment_name(In,NodeTypeId1),
	lookup_get_fieldType(In,NodeTypeId1,FieldId1,FieldType1),
	( FieldType1 == undefined
	->error_vrml(fieldType_undefined(NodeTypeId1,FieldId1))
	; FieldType0 == FieldType1
	).

lookup_field(In,NodeTypeId0, FieldId0, FieldId1) :-
	get_environment_name(In,NodeTypeId1),
	error_vrml(fieldType(NodeTypeId0,FieldId0,NodeTypeId1,FieldId1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred lookup_route(+Parse,+NodeTypeId0,+FieldId0,+NodeTypeId1,+FieldId1)
:: parse * atm * atm * atm * atm
# "The predicate will check the routing behaviour for two given fields.
   They will be checked according to the binding rules, like name changes
   access proporties. The node types for the field must of course be given 
   for the identification. ".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup_route(In,NodeTypeId0,FieldId0,NodeTypeId1,FieldId1) :-
	lookup_fields(In,NodeTypeId0,Acc0,Type0,FieldId0,_Init,_Bound),
	( Type0 == undefined
	-> change_name(changed,FieldId0,OutId),
	   lookup_fields(In,NodeTypeId0,Acc0,Type00,OutId,_Init,_Bound),
	   ( Type00 == undefined
	   ->error_vrml(fieldType_undefined(NodeTypeId1,FieldId1))
	   ; Type0_check = Type00
	   )
	;  Type0_check = Type0
	),
	lookup_fields(In,NodeTypeId1,Acc1,Type1,FieldId1,_Init,_Bound),
	( Type1 == undefined
	-> change_name(set,FieldId1,InId),
	   lookup_fields(In,NodeTypeId1,Acc1,Type11,InId,_Init,_Bound),
	   ( Type11 == undefined
	   ->error_vrml(fieldType_undefined(NodeTypeId1,FieldId1))
	   ; Type1_check = Type11
	   )
	; Type1_check = Type1
	),
	( ( Acc0 == eventOut
	  ; Acc0 == exposedField
	  )
	-> true
	;  error_vrml(route_eventOut(NodeTypeId0,FieldId0))
	),
	( ( Acc1 == eventIn		   
	  ; Acc1 == exposedField
	  )
	-> true
	;  error_vrml(route_eventIn(NodeTypeId1,FieldId1))
	),
	( Type0_check == Type1_check
	-> true
	;  error_vrml(route_type(NodeTypeId0,Type0_check,NodeTypeId1,Type1_check))
	).

lookup_route(_In,NodeTypeId0,FieldId0,NodeTypeId1,FieldId1) :-
	error_vrml(route(NodeTypeId0,FieldId0,NodeTypeId1,FieldId1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred change_name(+Type,+Name,-NewName)
:: atm * atm * atm
# "The predicate will acording to the naming rules for routing change the name 
   so that a match for a higher routine can be made.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change_name(set,Name,NewName) :-
	(  sub_atom(Name,0,4,_,'set_')
	-> sub_atom(Name,4,_,0,NewName)
	;  atom_concat('set_',Name,NewName)
	).

change_name(changed,Name,NewName) :-
	(  sub_atom(Name,_,8,0,'_changed')
	-> sub_atom(Name,0,8,_,NewName)
	;  atom_concat(Name,'_changed',NewName)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%lookup_eventIn(NodeTypeId, EventInId0,EventInId1) :-
%	dictionary(NodeTypeId, eventIn, FieldType0, 
%	                   EventInId0, _InitValue, _Boundaries),
%	dictionary(NodeTypeId, eventIn, FieldType1, 
%	                   EventInId1, _InitValue, _Boundaries),
%	(FieldType0 \== FieldType1
%	->
%	error_vrml(fieldType(NodeTypeId,EventInId0,FieldType0,
%	                                EventInId1,FieldType1))).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%lookup_eventOut(NodeTypeId, EventOutId0,EventOutId1) :-
%	dictionary(NodeTypeId, eventOut, FieldType0, 
%	                   EventOutId0, _InitValue, _Boundaries),
%	dictionary(NodeTypeId, eventOut, FieldType1, 
%	                   EventOutId1, _InitValue, _Boundaries),
%	(FieldType0 \== FieldType1
%	->
%	error_vrml(fieldType(NodeTypeId,EventOutId0,FieldType0,
%	                                EventOutId1,FieldType1))).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred lookup_fieldTypeId(+FieldTypeId)
:: atm
# "The predicate just make a check to see if the given FieldType id is
   among the allowed. You can not construct own ones and the check is mearly
   a spellcheck. ".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup_fieldTypeId(FieldTypeId) :-
	fieldType(FieldTypeId),
	!.

lookup_fieldTypeId(FieldTypeId) :-
	error_vrml(fieldTypeId_undefined(FieldTypeId)),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred lookup_set_prototype(+Parse,+Name,+Interface,+Definition)
:: parse * atm * term * term
# "The predicate will insert the prototype definition in the personal dictionary
   and will give a warning if there is a multiple name given. ".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

lookup_set_prototype(In,Name,Interface,Prototype) :-
        strip_interface(Interface,Interface_stripped),
	create_proto_element(Interface_stripped,Prototype,Element),
	get_dictionaries(In,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	dictionary_insert(Name,'PROTO',Element,Proto_dic,Info),
	(Info == new
	->
	true
	;
	Info == multiple,
	error_vrml(multiple_def(Name))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred lookup_set_extern_prototype(+Parse,+Name,+Interface,+Strings)
:: parse * atm * term * term
# "The predicate will insert the external prototype definition 
   in the personal dictionary
   and will give a warning if there is a multiple name given. ".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup_set_extern_prototype(In,Name,Interface,Strings) :-
	strip_interface(Interface,Interface_stripped),
	create_proto_element(Interface_stripped,Strings,Element),
	get_dictionaries(In,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	dictionary_insert(Name,'EXTERNPROTO',Element,Proto_dic,Info),
	(Info == new
	->
	true
	;
	Info == multiple,
	error_vrml(multiple_def(Name))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred lookup_set_def(+Parse,+Name,+Node)
:: parse * atm * term
# "The predicate will enter a new post in the personal dictionary for the 
   node definition. ".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup_set_def(In,NodeName,Node) :-
	get_dictionaries(In,Dic),
	get_definition_dictionary(Dic,Def_dic),
	dictionary_insert(NodeName,'DEF',Node,Def_dic,Info),
	(Info == new
	->
	true
	;
	Info == multiple,
	error_vrml(multiple_def(NodeName))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred lookup_field_access(+Parse, +NodenameId, +FieldId, +FieldId)
:: parse * atm * atm * atm
# "The predicate will control that the access proporties are correct according to 
   the certain rules that we have. It makes a check to see if the fields are of
   the same access type or if one of them is an exposedField. It is not doing
   a route check up to control that behaviour entirely.".
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup_field_access(Parse_struct, NodeName0, FieldId0, FieldId1) :-
	get_dictionaries(Parse_struct,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	get_environment(Parse_struct,Env),
	get_environment_name(Env,NodeName1),
	dictionary(NodeName0,Acc0,FieldType0,FieldId0,_Init,_Bound),
	dictionary_lookup(NodeName1,'PROTO',Info,Proto_dic,Status),
	(Status == defined
	->
	get_prototype_interface(Info,Interface),
	lookup_interface(Interface,Acc1,FieldType1,FieldId1,Status_interface),
	Status_interface == defined,
	( Acc0 == Acc1
	; Acc0 == exposedField
	),
	FieldType0 == FieldType1
	; error_vrml(accessType(NodeName0,FieldId0,Acc0,
	  NodeName1,FieldId1,Acc1))
	).

lookup_field_access(Parse_struct, NodeName0, FieldId0, FieldId1) :-
	get_dictionaries(Parse_struct,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	get_environment(Parse_struct,Env),
	get_environment_name(Env,NodeName1),
	dictionary(NodeName0,Acc0,FieldType0,FieldId0,_Init,_Bound),
	dictionary_lookup(NodeName1,'DEF',Info,Proto_dic,Status),
	  (Status == defined
	  ->
	  strip_interface(Info,[Interface]),
	  lookup_interface(Interface,Acc1,FieldType1,FieldId1,Status_interface),
	  Status_interface == defined,
	  ( Acc0 == Acc1
	  ; Acc0 == exposedField
	  ),
	  FieldType0 == FieldType1
	  ; error_vrml(accessType(NodeName0,FieldId0,Acc0,
	    NodeName1,FieldId1,Acc1))
	  ).


lookup_field_access(Parse_struct, NodeName0, FieldId0, FieldId1) :-
	get_dictionaries(Parse_struct,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	get_environment(Parse_struct,Env),
	get_environment_name(Env,NodeName1),
	dictionary(NodeName0,Acc0,FieldType0,FieldId0,_Init,_Bound),
	dictionary_lookup(NodeName1,'EXTERNPROTO',Info,Proto_dic,Status),
	  (  Status == defined
	  -> get_prototype_interface(Info,Interface),
	     lookup_interface(Interface,Acc1,FieldType1,FieldId1,Status_interface),
	     Status_interface == defined,
	     ( Acc0 == Acc1
	     ; Acc0 == exposedField
	     ),
	     FieldType0 == FieldType1
	  ;  error_vrml(accessType(NodeName0,FieldId0,Acc0,
	                      NodeName1,FieldId1,Acc1))
	).

lookup_field_access(In, NodeName0, FieldId0, FieldId1) :-
	get_environment(In,Env),
	get_environment_name(Env,NodeName1),
	error_vrml(fieldType(NodeName0,FieldId0,NodeName1,FieldId1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred lookup_access(+Parse, +NodeTypeId, +FieldId, -Acc)
:: parse * atm * atm * atm
# "The predicate will return the access type for the given nodce type with the 
   given field name. ". 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup_access(_In,NodeTypeId,FieldId,Acc) :-
	dictionary(NodeTypeId,Acc,_FieldType,FieldId,_Init,_Bound).

lookup_access(In,NodeTypeId,FieldId,Acc) :-
	get_dictionaries(In,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	dictionary_lookup(NodeTypeId,'PROTO',Info,Proto_dic,Status),
	(Status == defined
	->
	get_prototype_interface(Info,Interface),
	lookup_interface(Interface,Acc,_FieldType,FieldId,Status_interface),
	Status_interface == defined
	).

lookup_access(In, NodeTypeId, FieldId, Acc) :-
	get_dictionaries(In,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	dictionary_lookup(NodeTypeId,'DEF',Info,Proto_dic,Status),
	Status == defined,
	get_node_name(Info,Name),write(Info),nl,
	( Name == 'Script'
	-> get_node_guts(Info,Interface),write(Interface),nl,
	   strip_from_list(Interface,[Inter_Str]),
	   lookup_interface(Inter_Str,Acc,_FieldType,FieldId,StatInter),
	   ( StatInter == defined
	   -> true
	   ;  error_vrml(fieldType_undefined(NodeTypeId,FieldId))
	   )
	;
	dictionary(Name, Acc, _FieldType, 
	                   FieldId, _InitValue, _Boundaries)
	).

lookup_access(In,NodeTypeId,FieldId,Acc) :-
	get_dictionaries(In,Dic),
	get_prototype_dictionary(Dic,Proto_dic),
	dictionary_lookup(NodeTypeId,'EXTERNPROTO',Info,Proto_dic,Status),
	(Status == defined
	->
	get_prototype_interface(Info,Interface),
	lookup_interface(Interface,Acc,_FieldType,FieldId,Status_interface),
	Status_interface == defined
	).

lookup_access(_In,NodeTypeId,FieldId,_Acc) :-
	error_vrml(accessType(NodeTypeId,FieldId)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred lookup_interface(+Interface, +AccessType, +FieldType, +FieldId, -Status)
:: list(atm) * atm * atm * atm * atm
# "The predicate search the interface for the given arguments to see if the 
   declaration really is there. The status is defined or undefined.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup_interface([], _Acc,_Type,_Id, undefined).
lookup_interface([Node|_Interface], Acc, Type, Id, Stat) :-
	Node =.. [Acc,Type,Id|_More],
	Stat = defined.

lookup_interface([_Node|Interface], Acc,Type,Id, Stat) :-
	lookup_interface(Interface,Acc,Type,Id,Stat).

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup_event_list([], _Acc,_Type,_Id, undefined).
lookup_event_list([Node|_Interface], Acc, Type, Id, Stat) :-
	Node =.. [Acc,Type,Id],
	Stat = defined.

lookup_event_list([_Node|Interface], Acc,Type,Id, Stat) :-
	lookup_event_list(Interface,Acc,Type,Id,Stat).
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred create_proto_element(+Interface, +Definition,-Proto)
:: term * term * term
# "The predicate will construct a proto structure containing the 
   interface and the definition.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_proto_element(Interface,Definition,proto(Interface,Definition)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_prototype_interface(+Proto,-Interface)
:: term * term 
# "The predicate will return the interface from a proto structure.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_prototype_interface(proto(Interface,_Definition),Interface).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_prototype_definition(+Proto,-Definition)
:: term * term 
# "The predicate will return the definition from a proto structure.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_prototype_definition(proto(_Interface,Definition),Definition).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
get_node_name([Node],Name) :-
	Node =.. [Name|_Guts].

get_node_guts([Node],Guts) :-
	Node =.. [_Name|Guts].
