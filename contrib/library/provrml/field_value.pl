:- module(field_value,
        [fieldValue/6,
         mfstringValue/5,
         parse/1], 
         [assertions,isomodes,dcg]).

:- doc(author, "G@..{o}ran Smedb@..{a}ck").


%:- use_package(assertions).
%:- use_package(isomodes).
%:- use_package(types).

%:- include(library(iso)).
%:- use_module(library(iso_char)).
%%:- use_module(library(basicprops)).
:- use_module(library(lists)).
:- use_module(library(provrml/provrml_parser), [nodeDeclaration/4]).
:- use_module(library(provrml/parser_util)).
:- use_module(library(provrml/provrmlerror)).

:- set_prolog_flag(multi_arity_warnings, off).
%:- discontiguous([token_read/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- true prop parse(P).
parse(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-pred fieldValue(+ParseIn,-ParseOut,+FieldTypeId,-FieldValue, L, T)
 :: parse * parse * atm * list(term) * list * list
 # "The predicate read the fieldValue from the input token stream
     and return the value of the parsing. The resulting list might
     be of numbers, strings or VRML code dependnig on the FieldTypeId.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fieldValue(In,Out,'MFColor',Value) -->
	mfcolorValue(In,Out,Value).

fieldValue(In,Out,'MFFloat',Value) -->
	mffloatValue(In,Out,Value).

fieldValue(In,Out,'MFInt32',Value) -->
	mfint32Value(In,Out,Value).

fieldValue(In,Out,'MFNode',Value) -->
	mfnodeValue(In,Out,Value).

fieldValue(In,Out,'MFRotation',Value) -->
	mfrotationValue(In,Out,Value).

fieldValue(In,Out,'MFString',Value) -->
	mfstringValue(In,Out,Value).

fieldValue(In,Out,'MFVec2f',Value) -->
	mfvec2fValue(In,Out,Value).

fieldValue(In,Out,'MFVec3f',Value) -->
	mfvec3fValue(In,Out,Value).

fieldValue(In,Out,'SFBool',Value) -->
	sfboolValue(In,Out,Value).

fieldValue(In,Out,'SFColor',Value) -->
	sfcolorValue(In,Out,Value).

fieldValue(In,Out,'SFFloat',Value) -->
	sffloatValue(In,Out,Value).

fieldValue(In,Out,'SFImage',Value) -->
	sfimageValue(In,Out,Value).

fieldValue(In,Out,'SFInt32',Value) -->
	sfint32Value(In,Out,Value).

fieldValue(In,Out,'SFNode',Value) -->
	sfnodeValue(In,Out,Value).

fieldValue(In,Out,'SFRotation',Value) -->
	sfrotationValue(In,Out,Value).

fieldValue(In,Out,'SFString',Value) -->
	sfstringValue(In,Out,Value).

fieldValue(In,Out,'SFTime',Value) -->
	sftimeValue(In,Out,Value).

fieldValue(In,Out,'SFVec2f',Value) -->
	sfvec2fValue(In,Out,Value).

fieldValue(In,Out,'SFVec3f',Value) -->
	sfvec3fValue(In,Out,Value).

fieldValue(_,_,Type,_) -->
	{ error_vrml(fieldValue(Type)) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred mfcolorValue(+ParseIn,-ParseOut,-Value, L, T)
   :: parse * parse * list(term) * list * list
   # "The predicate read a colorvalue or a list of color values.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mfcolorValue(In,Out,Value) -->
	sfcolorValue(In,Out,Value).

mfcolorValue(In,Out,[SFColorValues]) -->
	[parenthesis_list_open],
	sfcolorValues(In,Out,SFColorValues),
	[parenthesis_list_close].


mfcolorValue(In,Out,[Comment]) -->
	[parenthesis_list_open],
	fillout(In,Out,Comment),
	[parenthesis_list_close].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred sfcolorValues(+ParseIn,-ParseOut,-Value, L, T)
   :: parse * parse * list(term) * list * list
   # "The predicate reads one or multiple colorvalues.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sfcolorValues(In,Out,Value) -->
	sfcolorValue(In,Out,Value).

sfcolorValues(In,Out,Values) -->
	sfcolorValue(In,In0,Value),
	sfcolorValues(In0,Out,Rest),
	{append(Value,Rest,Values)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred sfcolorValue(+ParseIn,-ParseOut,-Value, L, T)
   :: parse * parse * list(term) * list * list
   # "The predicate read a colorvalue.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sfcolorValue(In,Out,Value) -->
	read_float(In,In0,C0),
	read_float(In0,In1,C1),
	read_float(In1,Out,C2),
	{append(C0,C1,C01),
	 append(C01,C2,Value)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred mffloatValue(+ParseIn,-ParseOut,-Value,L,T)
   :: parse * parse * list(term) * list * list
   # "The predicate read a float value or a list of float values.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mffloatValue(In,Out,Value) -->
	sffloatValue(In,Out,Value).

mffloatValue(In,Out,[Value]) -->
	[parenthesis_list_open],
	 fillout(In,Out,Value),
	[parenthesis_list_close].

mffloatValue(In,Out,[Values]) -->
	[parenthesis_list_open],
	sffloatValues(In,Out,Values),
	[parenthesis_list_close].
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred sffloatValues(+ParseIn,-ParseOut,-Value,L,T)
   :: parse * parse * list(term) * list * list
   # "Reads one or multiple float values.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sffloatValues(In,Out,Value) -->
	sffloatValue(In,Out,Value).

sffloatValues(In,Out,Values) -->
	sffloatValue(In,In0,Value),
	sffloatValues(In0,Out,Rest),
	{append(Value,Rest,Values)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred sffloatValue(+ParseIn,-ParseOut,-Value,L,T)
   :: parse * parse * list(term) * list * list
   # "Reads a float value.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sffloatValue(In,Out,Value) -->
	read_float(In,Out,Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred mfint32Value(+ParseIn,-ParseOut,-Value,L,T)
   :: parse * parse * list(term) * list * list
   # "The predicate read one integer or multiple integer values in a list.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mfint32Value(In,Out,Value) -->
	sfint32Value(In,Out,Value).

mfint32Value(In,Out,[Val]) -->
	[parenthesis_list_open],
	 fillout(In,Out,Val),
	[parenthesis_list_close].

mfint32Value(In,Out,[Values]) -->
	[parenthesis_list_open],
	sfint32Values(In,Out,Values),
	[parenthesis_list_close].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred sfint32Values(+ParseIn,-ParseOut,-Value,L,T)
   :: parse * parse * list(term) * list * list
   # "Reads one or more integers.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sfint32Values(In,Out,Value) -->
	sfint32Value(In,Out,Value).

sfint32Values(In,Out,Values) -->
	sfint32Value(In,In0,Value),
	sfint32Values(In0,Out,Rest),
	{append(Value,Rest,Values)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred sfint32Value(+ParseIn,-ParseOut,-Value,L,T)
   :: parse * parse * list(term) * list * list
    # "Reads an integer.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sfint32Value(In,Out,Value) -->
	read_integer(In,Out,Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred mfnodeValue(+ParseIn,-ParseOut,-Value,L,T)
   :: parse * parse * list(term) * list * list
   # "Reads a node declaration or multiple node declarations from a list.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
mfnodeValue(In,Out,[Values]) -->
	[parenthesis_list_open],
	{create_parse_structure(In,P)},
	nodeDeclarations(P,Values_parsed0),
	{push_whitespace(Values_parsed0,In,Out),
	 reverse_parsed(Values_parsed0,Values_parsed),
	 get_parsed(Values_parsed,Values)},
	[parenthesis_list_close].

mfnodeValue(In,Out,Value) -->
	{create_parse_structure(In,P)},
	nodeDeclaration(P,Value_parsed0),
	{push_whitespace(Value_parsed0,In,Out),
	 reverse_parsed(Value_parsed0,Value_parsed),
	 get_parsed(Value_parsed,Value)}.

mfnodeValue(In,Out,[Value]) -->
	[parenthesis_list_open],
	fillout(In,Out,Value),
	[parenthesis_list_close].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred sfnodeValue(+ParseIn,-ParseOut,-Value,L,T)
   :: parse * parse * list(term) * list * list
   # "Reads a single node declaration.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sfnodeValue(In,Out,Value) -->
	[id('NULL')],
	fillout(In,Out,Comment),
	{ ( Comment == []
	  -> Value = ['NULL']
	  ;  Value = [['NULL'|Comment]]
	  )
	}.

sfnodeValue(In,Out,Value) -->
	{create_parse_structure(In,P)},
	nodeDeclaration(P,Value_parsed),
	{push_whitespace(Value_parsed,In,Out),
	 get_parsed(Value_parsed,Value)}.

sfnodeValue(In,Out,Node) -->
	at_least_one(In,In0),
	sfnodeValue(In0,Out,Node).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred nodeDeclarations(+ParseIn,-ParseOut,L,T)
   :: parse * parse * list * list
   # "Reads cero or more node declarations.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nodeDeclarations(In,Out) -->
	[],
	{stop_parse(In,Out) }.


nodeDeclarations(In,Values) -->
	nodeDeclaration(In,Value),
	nodeDeclarations(Value,Values).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred mfrotationValue(+ParseIn,-ParseOut,-Value,L,T)
   :: parse * parse * list(term) * list * list
   # "Reads a rotation value or a list of multiple rotation values.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mfrotationValue(In,Out,Value) -->
	sfrotationValue(In,Out,Value).

mfrotationValue(In,Out,[Comment]) -->
	[parenthesis_list_open],
	fillout(In,Out,Comment),
	[parenthesis_list_close].

mfrotationValue(In,Out,[Values]) -->
	[parenthesis_list_open],
	sfrotationValues(In,Out,Values),
	[parenthesis_list_close].
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred sfrotationValues(+ParseIn,-ParseOut,-Value,L,T)
   :: parse * parse * list(term) * list * list
   # "Reads one or more rotation values.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sfrotationValues(In,Out,Value) -->
	sfrotationValue(In,Out,Value).

sfrotationValues(In,Out,Values) -->
	sfrotationValue(In,In0,Value),
	sfrotationValues(In0,Out,Rest),
	{append(Value,Rest,Values)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred sfrotationValue(+ParseIn,-ParseOut,-Value,L,T)
   :: parse * parse * list(term) * list * list
   # "Reads one rotation value which consist of four floating values.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sfrotationValue(In,Out,Values) -->
	read_float(In,In0,X),
	read_float(In0,In1,Y),
	read_float(In1,In2,Z),
	read_float(In2,Out,R),
	{append(X,Y,XY),
	 append(XY,Z,XYZ),
	 append(XYZ,R,Values)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred mfstringValue(+ParseIn,-ParseOut,-Value,L,T)
 :: parse * parse * list(string) * list * list
 # "The predicate is exported for 'EXTERNPROTO' use, where names for 
     locations are given. Reads one string value or multiple stringvalues
     from a list.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mfstringValue(In,Out,Value) -->
	sfstringValue(In,Out,Value).

mfstringValue(In,Out,[Values]) -->
	[parenthesis_list_open],
	sfstringValues(In,Out,Values),
	[parenthesis_list_close].
	
mfstringValue(In,Out,[Value]) -->
	[parenthesis_list_open],
	fillout(In,Out,Value),
	[parenthesis_list_close].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred sfstringValues(+ParseIn,-ParseOut,-Value,L,T)
 :: parse * parse * list(string) * list * list
 # "Reads one string value or multiple stringvalues.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sfstringValues(In,Out,Value) -->
	sfstringValue(In,Out,Value).

sfstringValues(In,Out,Values) -->
	sfstringValue(In,In0,Value),
	sfstringValues(In0,Out,Rest),
	{append(Value,Rest,Values)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred sfstringValue(+ParseIn,-ParseOut,-Value,L,T)
 :: parse * parse * list(string) * list * list
 # "Reads one string value.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sfstringValue(In,In,[Value]) -->
	[string(Value)].

sfstringValue(In,Out,Value) -->
	at_least_one(In,In0,Fill),
	sfstringValue(In0,Out,String),
	{append(Fill,String,Value)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred mfvec2fValue(+ParseIn,-ParseOut,-Value,L,T)
   :: parse * parse * list(term) * list * list
   # "Reads 2 float values or multiple 2 floats from a list.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mfvec2fValue(In,Out,Value) -->
	sfvec2fValue(In,Out,Value).

mfvec2fValue(In,Out,[Value]) -->
	[parenthesis_list_open],
	fillout(In,Out,Value),
	[parenthesis_list_close].

mfvec2fValue(In,Out,[Values]) -->
	[parenthesis_list_open],
	sfvec2fValues(In,Out,Values),
	[parenthesis_list_close].
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred sfvec2fValues(+ParseIn,-ParseOut,-Value,L,T)
   :: parse * parse * list(term) * list * list
   # "Reads one or more 2 float values.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sfvec2fValues(In,Out,Value) -->
	sfvec2fValue(In,Out,Value).

sfvec2fValues(In,Out,Values) -->
	sfvec2fValue(In,In0,Value),
	sfvec2fValues(In0,Out,Rest),
	{append(Value,Rest,Values)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred sfvec2fValue(+ParseIn,-ParseOut,-Value,L,T)
   :: parse * parse * list(term) * list * list
   # "Reads one 2 float value.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sfvec2fValue(In,Out,Value) -->
	read_float(In,In0,V0),
	read_float(In0,Out,V1),
	{append(V0,V1,Value)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred mfvec3fValue(+ParseIn,-ParseOut,-Value,L,T)
   :: parse * parse * list(term) * list * list
   # "Read one 3 float value or multiple 3 float values from a list.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mfvec3fValue(In,Out,Value) -->
	sfvec3fValue(In,Out,Value).

mfvec3fValue(In,Out,[Value]) -->
	[parenthesis_list_open],
	fillout(In,Out,Value),
	[parenthesis_list_close].

mfvec3fValue(In,Out,[Values]) -->
	[parenthesis_list_open],
	sfvec3fValues(In,Out,Values),
	[parenthesis_list_close].
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred sfvec3fValues(+ParseIn,-ParseOut,-Value,L,T)
   :: parse * parse * list(term) * list * list
   # "Read one 3 float value or more 3 float values.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sfvec3fValues(In,Out,Value) -->
	sfvec3fValue(In,Out,Value).

sfvec3fValues(In,Out,Values) -->
	sfvec3fValue(In,In0,Value),
	sfvec3fValues(In0,Out,Rest),
	{append(Value,Rest,Values)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred sfvec3fValue(+ParseIn,-ParseOut,-Value,L,T)
   :: parse * parse * list(term) * list * list
   # "Read one 3 float value.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sfvec3fValue(In,Out,Value) -->
	read_float(In,In0,V0),
	read_float(In0,In1,V1),
	read_float(In1,Out,V2),
	{append(V0,V1,V01),
	 append(V01,V2,Value)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred sfboolValue(+ParseIn,-ParseOut,-Value,L,T)
   :: parse * parse * list(term) * list * list
   # "Read a bool, true or false.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sfboolValue(In,Out,Value) -->
	read_bool(In,Out,Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred sfimageValue(+ParseIn,-ParseOut,-Value,L,T)
   :: parse * parse * list(term) * list * list
   # "The predicate reads cero or more image values. 
      At least 3 numbers and then the pixelvalues. 
      The number of pixels is the first times the second.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sfimageValue(In,Out,Value) -->
	read_image(In,In0,I0),
	read_image(In0,In1,I1),
	read_image(In1,In2,I2),
	{get_number(I0,N0),
	get_number(I1,N1),
	Number is N0 * N1},
	sfimageValues(In2,Out,Number, Values),
	{append(I0,I1,I01),
	 append(I01,I2,I012),
	 append(I012,Values,Value)}.

sfimageValues(In,In,0,[]) -->
	[].

sfimageValues(In,Out,Number,Values) -->
	read_integer(In,In0,Value),
	{Next is Number - 1},
	sfimageValues(In0,Out,Next, Rest),
	{append(Value,Rest,Values)}.

get_number([N|_Rest],N) :-
	number(N).

get_number([List|Rest],Number) :-
	(atomic(List)
	->
	get_number(Rest,Number)
	;
	get_number(List,Number)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred sftimeValue(+ParseIn,-ParseOut,-Value,L,T)
   :: parse * parse * list(term) * list * list
   # "The predicate reads a time value, a float.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sftimeValue(In,Out,Value) -->
	read_float(In,Out,Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred read_bool(+ParseIn, -ParseOut,-Value,L,T)
   :: parse * parse * list(term) * list * list
    # "Reads a bool value and possibly comments. The answer is put in 
       a list.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_bool(In,Out,Value) -->
	read_bool0(Bool),
	fillout(In,Out,Comment),
	{ (Comment == []
	->
	Value = App
	;
	Value = [App])},
	{append(Bool,Comment,App)}.

read_bool(In,Out,[Value]) -->
	at_least_one(In,In0,Com),
	read_bool(In0,Out,Bool),
	{append(Com,Bool,Value)}.

read_bool0(['TRUE']) -->
	[id('TRUE')].

read_bool0(['FALSE']) -->
	[id('FALSE')].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred read_float(+ParseIn, -ParseOut,-Value,L,T)
   :: parse * parse * list(term) * list * list
    # "Reads a float value and possibly comments. The answer is put in 
       a list. The floating point value is a normal float an integer or
       an exponential.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_float(In,Out,Value) -->
	read_float0(Float),
	fillout(In,Out,Comment),
	{ (Comment == []
	->
	Value = App
	;
	Value = [App])},
	{append(Float,Comment,App)}.

read_float(In,Out,[Value]) -->
	at_least_one(In,In0,Comment),
	read_float(In0,Out,Float),
	{append(Comment,Float,Value)}.

read_float0([Float]) -->
	[float(Float)].

read_float0([Int]) -->
	[integer(Int)].

read_float0([Exp]) -->
	[exp(Exp)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred read_integer(+ParseIn, -ParseOut,-Value,L,T)
   :: parse * parse * list(term) * list * list
    # "Reads an integer value and possibly comments. The answer is put in 
       a list. An integer is a normal integer or a hexdecimal number.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_integer(In,Out,Value) -->
	read_integer0(Int),
	fillout(In,Out,Comment),
	{(Comment == []
	->
	Value = App
	;
	Value = [App])},
	{append(Int,Comment,App)}.

read_integer(In,Out,[Value]) -->
	at_least_one(In,In0,Com),
	read_integer(In0,Out,Num_or_com), 
	{append(Com,Num_or_com,Value)}.

read_integer0([Int]) -->
	[integer(Int)].

read_integer0([Hex]) -->
	[hex(Hex)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred read_image(+ParseIn, -ParseOut,-Value,L,T)
   :: parse * parse * list(term) * list * list
    # "Reads an image value and possibly comments. The answer is put in 
       a list. The image value is an integer.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_image(In,Out,Value) -->
	[integer(Im)],
	fillout(In,Out,Comment),
	{(Comment == []
	->
	Value = App
	;
	Value = [App])},
	{append([Im],Comment,App)}.

read_image(In,Out,[Im]) -->
	at_least_one(In,In0,Com),
	read_image(In0,Out,Rest),
	{append(Com,Rest,Im)}.
