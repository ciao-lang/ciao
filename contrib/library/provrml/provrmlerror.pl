:- module(provrmlerror,[error_vrml/1,output_error/1], [assertions,isomodes]).

:- use_module(library(write)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- doc(module,"This file implements error predicates of different types.").
:- doc(author, "G@..{o}ran Smedb@..{a}ck").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred error_vrml(+Structure) 
   :: term
   # "Given a structure with the error type as its head with possible
      arguments, it will write the associated error-text. ".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
error_vrml(tokeniser(Ahead)) :-
	output(['The tokeniser found strange characters or in some way not allowed.\n',Ahead,'\n']).

error_vrml(declarations) :-
	output(['The declarations of the VRML could not be parsed correctly.\n']).
error_vrml(declarations(Ahead)) :-
	output(['The declarations of the terms could not be parsed correctly. ',Ahead,' \n']).

error_vrml(nodeDeclaration(Ahead)) :-
	output(['The node declarations of the VRML could not be parsed correctly. Found ',Ahead,'\n']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
error_vrml(accessType(NodeId,FieldId)) :-
	output(['The accesstype could not be found for ',NodeId,' ',FieldId,
	        '.\n']).

error_vrml(accessType(NodeId0,FieldId0,AccessType0,NodeId1,FieldId1,AccessType1)) :-
	output([NodeId0,'.',FieldId0,' have the  access type ', AccessType0,
	        '\n',' and is not possible to combine with ',AccessType1,
		' of ',	NodeId1,'.',FieldId1]).

error_vrml(fieldId(NodeTypeId,FieldId,FieldType)) :-
	output(['There is no field: ',FieldId,' in the node: ',NodeTypeId,
	        ' with the type ',FieldType,'\n']).

error_vrml(fieldType(NodeTypeId0,Id0,NodeTypeId1,Id1)) :-
	output(['There is no way you can connect ','\n',
	NodeTypeId0,' ',Id0,' with ',NodeTypeId1,' ',Id1,'\n' ]).

error_vrml(fieldType_missmatch(NodeTypeId,FieldId,FieldType,_Info)) :-
	output(['The field in ',NodeTypeId,' called ',FieldId,' is of type ',
	        FieldType,' and does not match input.\n']).

error_vrml(fieldType_undefined(NodeId,FieldId)) :-
	output(['The fieldType in ',NodeId,' for the field ',FieldId,
                ' could not be found.\n']).

error_vrml(fieldTypeId_undefined(FieldTypeId)) :-
	output(['The fieldTypeId ',FieldTypeId,' is undefined.\n']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
error_vrml(fieldValue(Type)) :-
	output(['The fieldValue could not be read with type ', Type,'.\n']).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
error_vrml(nodeTypeId(NodeTypeId)) :-
	output(['The NodeTypeId ',NodeTypeId,' was not defined anywhere.\n']).


error_vrml(nodeNameId_DEF(NodeNameId)) :-
	output(['An error was encountered in the DEFinition of ', 
	        NodeNameId,'.\n']).


error_vrml(nodeNameId_USE(NodeNameId)) :-
	output(['An error was encountered in the USE of ', 
	        NodeNameId,'.\n']).

error_vrml(nodeGuts(NodeTypeId)) :-
	output(['The nodeGuts could not be properly read in ',
	        NodeTypeId,'.\n']).

error_vrml(nodeGuts(NodeTypeId,Term)) :-
	output(['The nodeGuts could not be properly read in ',
	        NodeTypeId,' with the term ',Term,'.\n']).


error_vrml(nodeGut_IS(NodeTypeId,FieldId)) :-
	output(['The second fieldId could not be read in ',NodeTypeId,
	        ' ',FieldId,' IS ...\n']).

error_vrml(nodeGut_fieldValue(NodeTypeId,FieldId)) :-
	output(['The fieldValue could not be read properly in ',
	        NodeTypeId,' ',FieldId,'.\n']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
error_vrml(proto_NodeTypeId(Name)) :-
	output([Name,' could not be accepted as a PROTO-name\n']).

error_vrml(interfaceDeclaration(ProtoName)) :-
	output(['The interface to ',ProtoName,' was rejected.\n']).

error_vrml(interfaceDeclaration(ProtoName,AccessType)) :-
	output(['The accesstype ',AccessType,' in ',ProtoName,' is wrong.\n']).

error_vrml(interfaceDeclaration_fieldTypeId(ProtoName,Acc,Row)) :-
	output(['Rownumber: ',Row,' The fieldTypeId to ',ProtoName,
	        ' with fieldAccess ',Acc,' could not be determined.\n']).

error_vrml(interfaceDeclaration_fieldId(ProtoName,FieldType,FieldAcc,Row)) :-
	output(['At row ',Row,' The fieldId could not be found in ',ProtoName,
	         ' ',FieldType,' ',FieldAcc,'\n']).

error_vrml(interfaceDeclaration_fieldValue(ProtoName,FieldType, 
	                                            FieldId,FieldAcc,Row)) :-
        output(['At row ',Row,' The fieldValue could not be settled in ',
	         ProtoName,' ',FieldType,' ',FieldAcc,' ',FieldId,'\n']).

error_vrml(proto_vrmlScene(ProtoName)) :-
	output(['The definition of ',ProtoName,' could not be read correctly.\n']).

error_vrml(externInterfaceDeclaration) :-
	output(['The accesstype could not be settled.\n']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
error_vrml(route_from(Ahead)) :-
	output(['Route declaration malign after ',Ahead,
	        ' and before "TO"\n']).

error_vrml(route_to(From,Event,Ahead)) :-
	output(['Route declaration malign after ',From,' ',Event,
	        ' "TO" and ',Ahead,'.\n']).

error_vrml(route(NodeTypeOut,FieldOut,NodeTypeIn,FieldIn)) :-
	output(['ROUTE could not be established between ',
	     NodeTypeOut,'.',FieldOut,' and ',NodeTypeIn,'.',FieldIn,'.\n']).

error_vrml(route_eventOut(NodeTypeOut,FieldOut)) :-
	output(['There is no eventOut called ',FieldOut,' in ',NodeTypeOut,'\n']).

error_vrml(route_eventIn(NodeTypeIn,FieldIn)) :-
	output(['There is no eventIn called ',FieldIn,' in ',NodeTypeIn,'\n']).

error_vrml(route_type(NodeType0,Type0,NodeType1,Type1)) :-
	output(['eventIn and eventOut have to be of the same type.\n',
	        NodeType0,' is of type ',Type0,' and ',NodeType1,
		' is of type ',Type1,'\n']).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
error_vrml(scriptGuts) :-
	output(['The scriptGuts could not be read properly.\n']).

error_vrml(scriptGut_fieldType(AccessType)) :-
	output(['The fieldType could not be settled with the access type ', 
	        AccessType,'.\n']).

error_vrml(scriptGut_fieldId(AccessType,FieldType)) :-
	output(['The fieldId could not be settled with the access type ', 
	        AccessType,' and ',FieldType,'.\n']).

error_vrml(scriptGut_IS(Acc,FieldType,FieldId)) :-
	output(['The second fieldId do not match or could not be found. ',
	        Acc,' ',FieldType,' ',FieldId,'.\n']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
error_vrml(undefined('USE',NodeName)) :-
	output(['The definition ',NodeName,' could not be found whithin the context','\n']).

error_vrml(undefined('PROTO',ProtoName)) :-
	output(['The PROTO name ',ProtoName,' could not be found whithin the context','\n']).

%%%%%%%%%%%%%%%% This is only a warning, thats why output_error/1 and 
%%%%%%%%%%%%%%%%                               not output/1.
error_vrml(multiple_def(Name)) :-
	output_error(['There are more then one definition of ',Name,' beware\n']).

error_vrml(header(Header)) :-
	output(['The header ',Header,' could not be accepted\n']).


error_vrml(malformed_beginning(Beginning)) :-
	output([Beginning,' is a bad beginning for the code.\n']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
error_vrml(min_incl(Value,Min)) :-
	output_error(['The value ',Value,' was found less then its lower bound ',Min,'\n']).

error_vrml(max_incl(Value,Max)) :-
	output_error(['The value ',Value,' was found higher then its upper bound ',Max,'\n']).

error_vrml(min_excl(Value,Min)) :-
	output_error(['The value ',Value,' was found less or equal to its lower bound ',Min,'\n']).

error_vrml(max_excl(Value,Max)) :-
	output_error(['The value ',Value,' was found higher or equal to its upper bound ',Max,'\n']).

error_vrml(inf(Value)) :-
	output_error(['You cannot include the infinity in the bound ;-) ',Value,'\n']).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
error_vrml(parenthesis_node_open(NodeType,Next_token)) :-
	output(['The "{" is missing after ',NodeType,'\n','*HERE*\n',Next_token,'\n']).

error_vrml(parenthesis_list_open(NodeTypeId,Next_token)) :-
	output(['The "[" is missing after ',NodeTypeId,' found ',Next_token,'\n']).

error_vrml(parenthesis_node_close(NodeType,Next_token)) :-
	output(['The "}" is missing after ',NodeType,'\n','*HERE*\n',
	         Next_token,'\n']).

error_vrml(parenthesis_list_close(NodeTypeId,Next_token)) :-
	output(['The "]" is missing after ',NodeTypeId,' found ',
                 Next_token,'\n']).


%%%%%%%%%%%%%%%%
%A catch everything.
error_vrml(In) :-
	output(['An undefined error has occured, called with ',In]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred output(+Message) 
   :: list(atm)
   # "This predicate will throw an exception for the error message. The main predicate 
      will then print the message given as the argument. ".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
output(Msg) :-
	throw(Msg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred output_error(+Message) 
   :: list(atm)
   # "This predicate will print the error message given as the argument. 
      This predicate is used for warnings that only needs to be given as information 
      and not necessarily give an error by the VRML browser.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_error(Msg) :-
	write('\nERROR: '),
	output_rest(Msg).

output_rest([]) :-
	nl.

output_rest([Message|Rest]) :-
	write(Message),
	output_rest(Rest).
