:- module(field_value_check,
        [fieldValue_check/8,
         mfstringValue/7],
         [dcg,
          assertions,
          isomodes]).

:- doc(author, "G@..{o}ran Smedb@..{a}ck").

%:- use_package(assertions).
:- use_module(library(provrml/provrml_io), 
        [out/3]).
:- use_module(library(provrml/generator_util), 
        [indentation_list/2,
         remove_comments/4]).
:- use_module(library(provrml/boundary), 
        [boundary_check/3,
         boundary_rotation_first/2,
         boundary_rotation_last/2]).
:- use_module(library(provrml/tokeniser), 
        [token_read/3]).
:- use_module(library(provrml/generator), 
        [nodeDeclaration/4]).
:- use_module(library(provrml/parser_util), 
        [create_parse_structure/3,
         get_parsed/2,
         push_dictionaries/3
        ]).

:- set_prolog_flag(multi_arity_warnings, off).
%:- discontiguous([token_read/3]).

:- true prop parse(P) + regtype.
parse(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-pred fieldValue_check(+FieldTypeId,+Value,+ParseIn,-ParseOut,+InitValue,+Boundary,L,T)
 :: atm * list * parse * parse * atm * atm * list * list
 # "The predicate read the fieldValue from the input token stream
    from the ParseIn. Checks of the values will be done in other module
    but initiated here. The values will be, if correct, collected via the DCG
    with the out/3 predicate for later output. 

    All the predicates have the same meaning as the corresponding predicates 
    in the input module field_value.pl. For more information about the different
    please see that module. ".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fieldValue_check('MFColor',Value,In,Out,Init,Boundary) -->
	mfcolorValue(Value,In,Out,Init,Boundary).

fieldValue_check('MFFloat',Value,In,Out,Init,Boundary) -->
	mffloatValue(Value,In,Out,Init,Boundary).

fieldValue_check('MFInt32',Value,In,Out,Init,Boundary) -->
	mfint32Value(Value,In,Out,Init,Boundary).

fieldValue_check('MFNode',Value,In,Out,Init,Boundary) -->
	mfnodeValue(Value,In,Out,Init,Boundary).

fieldValue_check('MFRotation',Value,In,Out,Init,Boundary) -->
	mfrotationValue(Value,In,Out,Init,Boundary).

fieldValue_check('MFString',Value,In,Out,Init,Boundary) -->
	mfstringValue(Value,In,Out,Init,Boundary).

fieldValue_check('MFVec2f',Value,In,Out,Init,Boundary) -->
	mfvec2fValue(Value,In,Out,Init,Boundary).

fieldValue_check('MFVec3f',Value,In,Out,Init,Boundary) -->
	mfvec3fValue(Value,In,Out,Init,Boundary).

fieldValue_check('SFBool',Value,In,Out,Init,Boundary) -->
	sfboolValue(Value,In,Out,Init,Boundary).

fieldValue_check('SFColor',Value,In,Out,Init,Boundary) -->
	sfcolorValue(Value,In,Out,Init,Boundary).

fieldValue_check('SFFloat',Value,In,Out,Init,Boundary) -->
	sffloatValue(Value,In,Out,Init,Boundary).

fieldValue_check('SFImage',Value,In,Out,Init,Boundary) -->
	sfimageValue(Value,In,Out,Init,Boundary).

fieldValue_check('SFInt32',Value,In,Out,Init,Boundary) -->
	sfint32Value(Value,In,Out,Init,Boundary).

fieldValue_check('SFNode',Value,In,Out,Init,Boundary) -->
	sfnodeValue(Value,In,Out,Init,Boundary).

fieldValue_check('SFRotation',Value,In,Out,Init,Boundary) -->
	sfrotationValue(Value,In,Out,Init,Boundary).

fieldValue_check('SFString',Value,In,Out,Init,Boundary) -->
	sfstringValue(Value,In,Out,Init,Boundary).

fieldValue_check('SFTime',Value,In,Out,Init,Boundary) -->
	sftimeValue(Value,In,Out,Init,Boundary).

fieldValue_check('SFVec2f',Value,In,Out,Init,Boundary) -->
	sfvec2fValue(Value,In,Out,Init,Boundary).

fieldValue_check('SFVec3f',Value,In,Out,Init,Boundary) -->
	sfvec3fValue(Value,In,Out,Init,Boundary).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mfcolorValue([[]],In,In,_Init,_Boundary) -->
	{ indentation_list(In,Indent) 
	},
	out(Indent),
	out(['[ ]\n']).

mfcolorValue([SFColorValues],In,Out,Init,Boundary) -->
	{ list(SFColorValues),
	  indentation_list(In,Indent)
	},
	out(Indent),
	out(['[ ']),
	sfcolorValues(SFColorValues,In,Out,Init,Boundary),
	out([']\n']).

mfcolorValue(Value,In,Out,Init,Boundary) -->
	sfcolorValue(Value,In,Out,Init,Boundary).

sfcolorValues(Value,In,Out,Init,Boundary) -->
	sfcolorValue(Value,In,Out,Init,Boundary).

sfcolorValues([C0,C1,C2|Rest],In,Out,Init,Boundary) -->
	sfcolorValue([C0,C1,C2],In,Out,Init,Boundary),
	sfcolorValues(Rest,In,Out,Init,Boundary).

sfcolorValue([C0,C1,C2],In,In,Init,Boundary) -->
	read_float(C0,Init,Boundary),
	read_float(C1,Init,Boundary),
	read_float(C2,Init,Boundary).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mffloatValue([[]],In,In,_Init,_Boundary) -->
	{ indentation_list(In,Indent) 
	},
	out(Indent),
	out(['[ ]\n']).

mffloatValue([Values],In,Out,Init,Boundary) -->
	{ list(Values),
	  indentation_list(In,Indent) 
	},
	out(Indent),
	out(['[ ']),
	sffloatValues(Values,In,Out,Init,Boundary),
	out([']\n']).
		
mffloatValue(Value,In,Out,Init,Boundary) -->
	sffloatValue(Value,In,Out,Init,Boundary).

sffloatValues(Value,In,Out,Init,Boundary) -->
	sffloatValue(Value,In,Out,Init,Boundary).

sffloatValues([Value|Rest],In,Out,Init,Boundary) -->
	sffloatValue([Value],In,In0,Init,Boundary),
	sffloatValues(Rest,In0,Out,Init,Boundary).

sffloatValue([Value],In,In,Init,Boundary) -->
	read_float(Value,Init,Boundary).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mfint32Value([[]],In,In,_Init,_Boundary) -->
	{ indentation_list(In,Indent)
	},
	out(Indent),
	out(['[ ]\n']).

mfint32Value([Values],In,Out,Init,Boundary) -->
	{ list(Values),
	  indentation_list(In,Indent)
	},
	out(Indent),
	out(['[ ']),
	sfint32Values(Values,In,Out,Init,Boundary),
	out([']\n']).

mfint32Value(Value,In,Out,Init,Boundary) -->
	sfint32Value(Value,In,Out,Init,Boundary).

sfint32Values(Value,In,Out,Init,Boundary) -->
	sfint32Value(Value,In,Out,Init,Boundary).

sfint32Values([Value|Rest],In,Out,Init,Boundary) -->
	sfint32Value([Value],In,In0,Init,Boundary),
	sfint32Values(Rest,In0,Out,Init,Boundary).

sfint32Value([Value],In,In,Init,Boundary) -->
	read_integer(Value,Init,Boundary).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mfnodeValue([[]],In,In,_Init,_Boundary) -->
	{ indentation_list(In,Indent)
	},
	out(Indent),
	out(['[ ]\n']).

mfnodeValue([Values],In,Out,_Init,_Boundary) -->
	{ list(Values),
	  indentation_list(In,Indent),
	  create_parse_structure(Values,In,Parse)
	},
	out(Indent),
	out(['[ ']),
	nodeDeclarations(Parse,Out),
	out([']\n']).

mfnodeValue(Value,In,Out,_Init,_Boundary) -->
	{ create_parse_structure(Value,In,Parse)
	},
	nodeDeclaration(Parse,In0),
	{ push_dictionaries(In0,In,Out)
	}.

sfnodeValue(Value,In,In,_Init,_Boundary) -->
	( { Value = ['NULL']
	  ; Value = [['NULL'|_]]
	  }
	),
	{ indentation_list(In,Indent) },
	out(Indent),
	out(Value).
	
sfnodeValue(Value,In,Out,_Init,_Boundary) -->
	{ create_parse_structure(Value,In,Parse)
	},
	nodeDeclaration(Parse,In0),
	{ push_dictionaries(In0,In,Out)
	}.

nodeDeclarations(In,In) -->
	{ get_parsed(In,[]) }.

nodeDeclarations(In,Out) -->
	nodeDeclaration(In,In0),
	nodeDeclarations(In0,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mfrotationValue(Value,In,Out,Init,Boundary) -->
	sfrotationValue(Value,In,Out,Init,Boundary).

mfrotationValue([[]],In,In,_Init,_Boundary) -->
	{ indentation_list(In,Indent)
	},
	out(Indent),
	out(['[ ]\n']).

mfrotationValue([Values],In,Out,Init,Boundary) -->
	{ list(Values),
	  indentation_list(In,Indent)
	},
	out(Indent),
	out(['[ ']),
	sfrotationValues(Values,In,Out,Init,Boundary),
	out([']\n']).
	
sfrotationValues(Value,In,Out,Init,Boundary) -->
	sfrotationValue(Value,In,Out,Init,Boundary).

sfrotationValues([X,Y,Z,R|Rest],In,Out,Init,Boundary) -->
	sfrotationValue([X,Y,Z,R],In,In0,Init,Boundary),
	sfrotationValues(Rest,In0,Out,Init,Boundary).

sfrotationValue([X,Y,Z,R],In,In,Init,Bounds) -->
	{ boundary_rotation_first(Bounds,First),
	  boundary_rotation_last(Bounds,Last) 
	},
	read_float(X,Init,First),
	read_float(Y,Init,First),
	read_float(Z,Init,First),
	read_float(R,Init,Last).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mfstringValue([[]],In,In,_Init,_Boundary) -->
	{ indentation_list(In,Indent)
	},
	out(Indent),
	out(['[ ]\n']).

mfstringValue([Values],In,Out,Init,Boundary) -->
	{ list(Values),
	  indentation_list(In,Indent)
	},
	out(Indent),
	out(['[ ']),
	sfstringValues(Values,In,Out,Init,Boundary),
	out([']\n']).

mfstringValue(Value,In,Out,Init,Boundary) -->
	sfstringValue(Value,In,Out,Init,Boundary).
	
sfstringValues(Value,In,Out,Init,Boundary) -->
	sfstringValue(Value,In,Out,Init,Boundary).

sfstringValues([Value|Rest],In,Out,Init,Boundary) -->
	sfstringValue([Value],In,In0,Init,Boundary),
	sfstringValues(Rest,In0,Out,Init,Boundary).

sfstringValue([Value],In,In,_Init,_Boundary) -->
	out(['"',Value,'" ']).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mfvec2fValue([[]],In,In,_Init,_Boundary) -->
	{ indentation_list(In,Indent)
	},
	out(Indent),
	out(['[ ]\n']).

mfvec2fValue([Values],In,Out,Init,Boundary) -->
	{ list(Values),
	  indentation_list(In,Indent)
	},
	out(Indent),
	out(['[ ']),
	sfvec2fValues(Values,In,Out,Init,Boundary),
	out([']\n']).
	
mfvec2fValue(Value,In,Out,Init,Boundary) -->
	sfvec2fValue(Value,In,Out,Init,Boundary).

sfvec2fValues(Value,In,Out,Init,Boundary) -->
	sfvec2fValue(Value,In,Out,Init,Boundary).

sfvec2fValues([V0,V1|Rest],In,Out,Init,Boundary) -->
	sfvec2fValue([V0,V1],In,In0,Init,Boundary),
	sfvec2fValues(Rest,In0,Out,Init,Boundary).

sfvec2fValue([V0,V1],In,In,Init,Boundary) -->
	read_float(V0,Init,Boundary),
	read_float(V1,Init,Boundary).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mfvec3fValue([[]],In,In,_Init,_Boundary) -->
	{ indentation_list(In,Indent) 
	},
	out(Indent),
	out(['[ ]\n']).

mfvec3fValue([Values],In,Out,Init,Boundary) -->
	{ list(Values),
	  indentation_list(In,Indent)
	},
	out(Indent),
	out(['[ ']),
	sfvec3fValues(Values,In,Out,Init,Boundary),
	out([']']).

mfvec3fValue(Value,In,Out,Init,Boundary) -->
	sfvec3fValue(Value,In,Out,Init,Boundary).

sfvec3fValues(Value,In,Out,Init,Boundary) -->
	sfvec3fValue(Value,In,Out,Init,Boundary).

sfvec3fValues([V0,V1,V2|Rest],In,Out,Init,Boundary) -->
	{ indentation_list(In,Indent) 
	},
	out(Indent),
	sfvec3fValue([V0,V1,V2],In,In0,Init,Boundary),
	out(['\n']),
	sfvec3fValues(Rest,In0,Out,Init,Boundary).

sfvec3fValue([V0,V1,V2],In,In,Init,Boundary) -->
	read_float(V0,Init,Boundary),
	read_float(V1,Init,Boundary),
	read_float(V2,Init,Boundary).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sfboolValue([Value],In,In,Init,Boundary) -->
	read_bool(Value,Init,Boundary).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sfimageValue([I0,I1,I2|Rest],In,In,Init,Boundary) -->
	read_image(I0,Init,Boundary),
	read_image(I1,Init,Boundary),
	read_image(I2,Init,Boundary),
	{ get_number(I0,N0),
	  get_number(I1,N1),
	  Number is N0 * N1
	},
	sfimageValues(Number, Rest, In,Init,Boundary).

sfimageValues(0,[],_In,_Init,_Boundary) -->
	[].

sfimageValues(Number,[Value|Values],In,Init,Boundary) -->
	read_integer(Value,Init,Boundary),
	{ Next is Number - 1
	},
	sfimageValues(Next,Values,In,Init,Boundary).

get_number(N,N) :-
	number(N).

get_number([N|_Rest],N) :-
	number(N).

get_number([List|Rest],Number) :-
	(atomic(List)
	->
	get_number(Rest,Number)
	;
	get_number(List,Number)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sftimeValue([Value],In,In,Init,Boundary) -->
	read_float(Value,Init,Boundary).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_bool('TRUE',_Init,_Boundary) -->
	out(['TRUE ']).

read_bool('FALSE',_Init,_Boundary) -->
	out(['FALSE ']).

read_bool(List,_Init,_Boundary) -->
	{ list(List),
	  remove_comments(List,Before,Value,After)
	},
	out(Before),
	read_bool(Value,_,_),
	out(After).

%%%%%%%%%%%%%%%%
read_float(List,Init,Boundary) -->
	{ list(List),
	  remove_comments(List,Before,Value,After)
	},
	out(Before),
	read_float(Value,Init,Boundary),
	out(After).

read_float(Float,Init,Boundary) -->
	{ float(Float),
	  boundary_check(Float,Init,Boundary)
	},
	out([Float,' ']).
	

read_float(Int,Init,Boundary) -->
	{ integer(Int),
	  boundary_check(Int,Init,Boundary)
	},
	out([Int,' ']).

read_float(Exp,Init,Boundary) -->
	{ name(Exp,List),
	  token_read(exp(Exp),List,[]),
	  boundary_check(Exp,Init,Boundary)
	},
	out([Exp,' ']).

%%%%%%%%%%%%%%%%
read_integer(List,Init,Boundary) -->
	{ list(List),
	  remove_comments(List,Before,Value,After)
	},
	out(Before),
	read_integer(Value,Init,Boundary),
	out(After).

read_integer(Int,Init,Boundary) -->
	{ integer(Int),
	  boundary_check(Int,Init,Boundary)
	},
	out([Int,' ']).

read_integer(Hex,Init,Boundary) -->
	{ name(Hex,List),
	  token_read(hex(Hex),List,[]),
	  boundary_check(Hex,Init,Boundary)
	},
	out([Hex,' ']).

%%%%%%%%%%%%%%%%
read_image(List,Init,Boundary) -->
	{ list(List),
	  remove_comments(List,Before,Value,After)
	},
	out(Before),
	read_image(Value,Init,Boundary),
	out(After).

read_image(Im,Init,Boundary) -->
	{ integer(Im),
	  boundary_check(Im,Init,Boundary)
	},
	out([Im,' ']).
