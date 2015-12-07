:- module(generator, [generator/2,nodeDeclaration/4], 
	             [assertions,regtypes,isomodes,dcg,iso]).

:- doc(author, "G@..{o}ran Smedb@..{a}ck").

:- use_module(library(provrml/provrml_io), 
        [convert_atoms_to_string/2]).
:- use_module(library(provrml/generator_util), 
        [reading/4,
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
         start_vrmlScene/4]).
:- use_module(library(provrml/parser_util), [create_parse_structure/2]).
:- use_module(library(provrml/provrmlerror)).
:- use_module(library(provrml/internal_types)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred generator(+Terms,-VRML)
	:: list(term) * string
        # "This predicate is the generator of VRML code. It accepts a
           list of terms that is correct VRML code, other kind of terms 
           will be rejected will errormessage accordingly. The output
           is a string of correct VRML code, acceptable for VRML browsers.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generator(Terms,VRML) :-
	create_parse_structure(Terms,Structure),
	catch( vrmlScene_first(Structure,_Out,VRML_atoms,[]), 
	Msg, output_error(Msg)),
	convert_atoms_to_string(VRML_atoms,VRML),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred vrmlScene_first(+ParseIn,-ParseOut,L,T)
   :: parse * parse * list * list
   # "The predicate is awaiting the header before continue. The header can be 
      followed by comments on the same row. The header is '#VRML V2.0 utf8'.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vrmlScene_first(In,Out) -->
	header(In,Out0),
	vrmlScene(Out0,Out).
	 
header(In,Out) -->
	reading(header,In,Out).

header(In,Out) -->
	reading(error_header,In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred vrmlScene(+ParseIn, -ParseOut, L, T)
:: parse * parse * list * list
# "We can read declarations or we have an error, undefined probably.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vrmlScene(In,Out) -->
	declarations(In,Out).

vrmlScene(In,Out) -->
	reading(error_declaration,In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred declarations(+ParseIn, -ParseOut, L, T)
:: parse * parse * list * list
# "We can have finnished the parsing and are done or we can be reading
   more declaration.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

declarations(In,Out) -->
	reading(empty,In,Out).

declarations(In,Out) -->
	declaration(In,Out0),
	declarations(Out0,Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred declaration(+ParseIn, -ParseOut, L, T)
:: parse * parse * list * list
# "We can have the key word NULL, a comment or more. We can read a 
   prototype or a route declaration or a node declaration. We see
   that we can end the 'program with a comment' as well thats why 
   we have two commentreding predicates.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

declaration(In,Out) -->
	reading('NULL',In,Out).

declaration(In,Out) -->
	reading(comment,In,In0),
	declaration(In0,Out).

declaration(In,Out) -->
	protoDeclaration(In,Out).

declaration(In,Out) -->
	routeDeclaration(In,Out).

declaration(In,Out) -->
	reading(comment,In,Out).

declaration(In, Out) -->
	nodeDeclaration(In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred nodeDeclaration(+ParseIn,-ParseOut,L,T)
:: parse * parse * list * list
# "The node declaration canbe constructed by a DEFinition, we then make 
   a call to generator_util to make proper settings before continue.
   There can be a USE of a prior defined node or we can have a normal 
   node declaration, one of the built ins.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nodeDeclaration(In,Out) -->
	reading('DEF',In),
	open_DEF(In,In0,NodeIn),
	node(NodeIn,NodeOut),
	close_DEF(NodeOut,In0,Out).

nodeDeclaration(In,Out) -->
	reading('USE',In,Out).

nodeDeclaration(In,Out) -->
	node(In,Out).

nodeDeclaration(In,Out) -->
	reading(error_nodeDeclaration,In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred protoDeclaration(+ParseIn,-ParseOut,L,T)
:: parse * parse * list * list
# "The prototype declaration can be normal or external.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

protoDeclaration(In,Out) -->
	proto(In,Out).

protoDeclaration(In,Out) -->
	externproto(In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred proto(+ParseIn,-ParseOut,L,T)
:: parse * parse * list * list
# "The proto read the structure name and then a setting of environment
   is beeing done in the generator_util module. Afterwards we read the 
   interface and then we continue with the scene, which is like a VRML
   program of its own.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

proto(In,Out) -->
	reading('PROTO',In),
	open_PROTO(In,DeclIn),
	interfaceDeclarations(DeclIn,DeclOut),
	start_vrmlScene(In,SceneIn),
	vrmlScene(SceneIn,SceneOut),
	close_PROTO(DeclOut,SceneOut,In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred interfaceDeclarations(+ParseIn,-ParseOut,L,T)
:: parse * parse * list * list
# "There can be cero or more interface declarations.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interfaceDeclarations(In,Out) -->
	reading(empty,In,Out).

interfaceDeclarations(In,Out) -->
	interfaceDeclaration(In,In0),
	interfaceDeclarations(In0,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred interfaceDeclaration(+ParseIn,-ParseOut,L,T)
:: parse * parse * list * list
# "We can have comments, restricted interface declarations 
   or a exposed field.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interfaceDeclaration(In, Out) -->
	reading(comment,In,In0),
	interfaceDeclaration(In0,Out).

interfaceDeclaration(In,Out) -->
	restrictedInterfaceDeclaration(In,Out).

interfaceDeclaration(In,Out) -->
	reading(exposedField,In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred restrictedInterfaceDeclaration(+ParseIn,-ParseOut,L,T)
:: parse * parse * list * list
# "The restricted key words do we find in the generator_util module
   where we check if we have them.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

restrictedInterfaceDeclaration(In,Out) -->
	reading(restrictedInterfaceDeclaration,In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred externproto(+ParseIn,-ParseOut,L,T)
:: parse * parse * list * list
# "We find a structurename for the externproto
   and we make the proper arangements for the continue. We then read the
   interface and then the names for the other VRML modules.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

externproto(In,Out) -->
	reading('EXTERNPROTO',In),
	open_EXTERNPROTO(In,DeclIn,StringIn),
	externInterfaceDeclarations(DeclIn,DeclOut),
	reading(mfstringValue,StringIn,StringOut),
	close_EXTERNPROTO(DeclOut,StringOut,In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred externInterfaceDeclarations(+ParseIn,-ParseOut,L,T)
:: parse * parse * list * list
# "There can be cero or more external interface declarations.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

externInterfaceDeclarations(In,Out) -->
	reading(empty,In,Out).

externInterfaceDeclarations(In,Out)-->
	externInterfaceDeclaration(In,In0),
	externInterfaceDeclarations(In0,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred externInterfaceDeclaration(+ParseIn,-ParseOut,L,T)
:: parse * parse * list * list
# "There can be a comment or a declaration of type we can find in the 
   generator_util module. ".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

externInterfaceDeclaration(In,Out) -->
	reading(comment,In,In0),
	externInterfaceDeclaration(In0,Out).

externInterfaceDeclaration(In,Out) -->
	reading(externInterfaceDeclaration,In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred routeDeclaration(+ParseIn,-ParseOut,L,T)
:: parse * parse * list * list
# "We find the structure name ROUTE and act accordingly.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

routeDeclaration(In,Out) -->
	reading('ROUTE',In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred node(+ParseIn,-ParseOut,L,T)
:: parse * parse * list * list
# "The node can be a script or a normal node, we do the arrangments
   in the generator_util module. We then continue with the reading of 
   the guts. ".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

node(In,Out) -->
	reading('Script',In),
	open_Script(In,In0,ScriptGutsIn),
	scriptGuts(ScriptGutsIn,ScriptGutsOut),
	close_Script(ScriptGutsOut,In0,Out).

node(In,Out) -->
	reading(node,In),
	open_node(In,In0,NodeGutsIn,NodeNameId),
	nodeGuts(NodeNameId,NodeGutsIn,NodeGutsOut),
	close_node(NodeGutsOut,In0,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred nodeGuts(+Nodename,+ParseIn,-ParseOut,L,T)
::  atm * parse * parse * list * list
# "We can have cero or more node guts.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nodeGuts(_Name,In,Out) -->
	close_nodeGut(In,Out).

nodeGuts(Name,In,Out) -->
	nodeGut(Name,In,Out0),
	nodeGuts(Name,Out0,Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred scriptGuts(+ParseIn,-ParseOut,L,T)
:: parse * parse * list * list
# "We can have cero or more script guts.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scriptGuts(In,Out) -->
	reading(empty,In,Out).

scriptGuts(In,Out) -->
	scriptGut(In,Out0),
	scriptGuts(Out0,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred nodeGut(+Nodename,ParseIn,-ParseOut,L,T)
:: atm * parse * atm * list * list
# "We can have a comment, a redirection using the IS key word. A nodeGut
   a route declaration or a prototype.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nodeGut(Name,In,Out) -->
	reading(comment,In,In0),
	nodeGut(Name,In0,Out).

nodeGut(Name,In,Out) -->
	reading('IS',Name,In,Out).

nodeGut(Name,In,Out) -->
	reading(nodeGut,Name,In,Out).

nodeGut(_Name,In,Out) -->
	routeDeclaration(In,Out).

nodeGut(_Name,In,Out) -->
	protoDeclaration(In,Out).

nodeGut(Name,In,Out) -->
	reading(error_nodeGut(Name),In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred scriptGut(+ParseIn,-ParseOut,L,T)
:: parse * parse  * list * list
# "We can have a comment a nodeGut or 
   a restricted interface declaration.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scriptGut(In,Out) -->
	reading(comment,In,In0),
	scriptGut(In0,Out).

scriptGut(In,Out) -->
	restrictedInterfaceDeclaration(In,Out).

scriptGut(In,Out) -->
	nodeGut('Script',In,Out).
