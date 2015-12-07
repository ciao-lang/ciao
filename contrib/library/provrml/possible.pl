:- module(possible,[continue/3],[dcg,assertions,isomodes]).

:- doc(author, "G@..{o}ran Smedb@..{a}ck").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred continue(+RuleName,L,T)
:: atm * list * list
# "The predicate will check the rule name's possible followers, 
   that is the entrance in the grammar, to see if we have the 
   possibility to contiue the generation in the asked direction. 
   We will by using the DCG look one item ahead to see if we have 
   the proper key name ahead. Then we check the possible alternatives 
   for the rule name from the list and type we receive from the call to
   possible. As we can see there are different look ahead predicates depending
   if we have a token or a word, looking on the structure name(token name)
   or if we should read the first word beyond the structure name.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

continue(Clause) -->
	
	{possible(Clause,Possibilities,Type)},
	 look_ahead(Type,Ahead),
	 {memberchk(Ahead,Possibilities)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

look_ahead(token,Token,[Val|Rest],[Val|Rest]) :-
	Val =.. [Token|_Value].

look_ahead(word,Word,[Val|Rest],[Val|Rest]) :-
	Val =.. [_Token,Word|_Value].

memberchk(_Element,[]) :-
	!,
	fail.

memberchk(Element,[Element|_More]) :-
	!.

memberchk(Element,[_Something|More]) :-
	memberchk(Element,More).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred possible(+RuleName,-ListOfPossible,-TypeOfStructure)
:: atm * list(atm) * atm
# "The rule name makes us enter in the right predicate to 
   get the right list of possible followers and the type we
   are dealing with.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

possible(vrmlScene,[comment,whitespace,id],token).

possible(declarations,[comment,whitespace,id],token).

possible(declaration,[comment,whitespace,id],token).

possible(nodeDeclaration,[comment,whitespace,id],token).

possible(protoDeclaration,['PROTO','EXTERNPROTO'],word).

possible(proto,['PROTO','EXTERNPROTO'],word).

possible(interfaceDeclarations, [eventIn,eventOut,field,exposedField],word).

possible(interfaceDeclaration, [eventIn,eventOut,field,exposedField],word).

possible(restrictedInterfaceDeclaration,[eventIn,eventOut,field],word).

possible(externproto,['EXTERNPROTO'],word).

possible(externInterfaceDeclarations,[eventIn,eventOut,field,exposedField],word).

possible(externInterfaceDeclaration,[eventIn,eventOut,field,exposedField],word).

possible(routeDeclaration,['ROUTE'],word).

possible(node,[comment,whitespace,id],token).

possible(nodeGuts,[comment,whitespace,id],token).

possible(nodeGut,[comment,whitespace,id],token).

possible(scriptGuts,[comment,whitespace,id],token).

possible(scriptGut,[comment,whitespace,id],token).

possible(fieldType,['MFColor','MFFloat','MFInt32','MFNode','MFRotation',
	            'MFString','MFVec2f','MFVec3f','SFBool','SFColor',
		    'SFFloat','SFImage','SFInt32','SFNode','SFRotation',
		    'SFString','SFTime','SFVec2f','SFVec3f'],word).

possible(fieldValue,[comment,whitespace,id,float,integer,hex,exp,id,string,
	             parenthesis_list_open],token).
