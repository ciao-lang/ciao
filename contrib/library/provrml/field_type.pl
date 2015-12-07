:- module(field_type, [fieldType/1],[assertions,isomodes]).

:- doc(author, "G@..{o}ran Smedb@..{a}ck").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred fieldType(+FieldTypeId)
   :: atm
   # "Boolean predicate used to check the fieldTypeId with the defiened.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fieldType('MFColor').
fieldType('MFFloat').
fieldType('MFInt32').
fieldType('MFNode').
fieldType('MFRotation').
fieldType('MFString').
fieldType('MFVec2f').
fieldType('MFVec3f').

fieldType('SFBool').
fieldType('SFColor').
fieldType('SFFloat').
fieldType('SFImage').
fieldType('SFInt32').
fieldType('SFNode').
fieldType('SFRotation').
fieldType('SFString').
fieldType('SFTime').
fieldType('SFVec2f').
fieldType('SFVec3f').
