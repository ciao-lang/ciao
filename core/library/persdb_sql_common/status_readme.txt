DIRECTORY CONTENTS
==================
- "pl2sqlinsert.pl" contains the source Prolog code of the Prolog to SQL
insert translator.
- "predicates.pl" contains two SQL tables definitions.

WHAT IS pl2sqlInsert
====================
- pl2sqlInsert/2 translates an external Prolog predicate with constant
arguments, into a SQL insertion sentence. 
- Tables will be defined using relation/3, attribute/4. 
- pl2sqlInsert is used by persdb_sql library, which makes an expansion adding
relation/3 and attribute/4 facts to define the table.

OUTSTANDINGS
============
Errors and type checking

EXAMPLE OF USE
==============

%% FIRST: LOADING pl2sqlinsert MODULE
?- use_module('pl2sqlinsert').

%% SECOND: LOADING EXTERNAL PREDICATES DEFINITIONS
?- ensure_loaded('predicates').

%% THIRD: IMPORTING STRINGS LIBRARY TO SEE TRANSLATIONS
?- use_module(library(strings)).

%% FOURTH: TWO EXAMPLES OF pl2sqlInsert USE

?- pl2sqlInsert(product(300,'Tee Shirt',28,'White',9.00),T),write_string(T).
INSERT INTO PRODUCT (ID,NAME,QUANTITY,COLOR,UNIT_PRICE) VALUES (300,'Tee Shirt',28,'White',9.0).
T = [73,78,83,69,82,84,32,73,78,84,79,32,80,82,79,68,85,67,84,32,40,73,68,44,78,65,77,69,44,81,85,65,78,84,73,84,89,44,67,79,76,79,82,44,85,78,73,84,95,80,82,73,67,69,41,32,86,65,76,85,69,83,32,40,51,48,48,44,39,84,101,101,32,83,104,105,114,116,39,44,50,56,44,39,87,104,105,116,101,39,44,57,46,48,41,46] ? ;

no
{debug}
?- pl2sqlInsert(employee('Barletta','1998-7-18',45450),T),write_string(T).
INSERT INTO EMPLOYEE (EMP_LNAME,START_DATE,SALARY) VALUES ('Barletta','1998-7-18',45450).
T = [73,78,83,69,82,84,32,73,78,84,79,32,69,77,80,76,79,89,69,69,32,40,69,77,80,95,76,78,65,77,69,44,83,84,65,82,84,95,68,65,84,69,44,83,65,76,65,82,89,41,32,86,65,76,85,69,83,32,40,39,66,97,114,108,101,116,116,97,39,44,39,49,57,57,56,45,55,45,49,56,39,44,52,53,52,53,48,41,46] ? ;

no
{debug}






