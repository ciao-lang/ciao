% Where to put descriptions
% <description>Basic XDR schema definition</description>

% :- module(xdrtree, [main/0], [assertions]).
:- module(_, [xdrdoc/4], []).
% :- module(xdrtree, [schema_node/1]).

% :- use_module(library(lists), [member/2]).

%:- include(schema).
:- include(constraints).
% :- use_module(engine(basic_props), [member/2, string/1, nnegint/1]).
% :- use_module(engine(basiccontrol)).

%%%%%%%%%%%%%%%%%%%

% main:- test.

% test:- 
%       schema(B, C, D, E, F),
%       schema_node(schema(B, C, D, E, F)).

%%%%%%%%%%%%%%%%%%

xdrdoc(A, B, C, D):-
       xmldecl_list(A),
       schema_constraint_list(B),
       attribute_type_list(C),
       element_type_list(D).

xmldecl_list([]).
xmldecl_list([D|Ds]) :-
	xmldecl(D),
	xmldecl_list(Ds).

xmldecl(=(version,_V)).
xmldecl(=(encoding,_E)).

% name(A):- string(A).

schema_constraint_list([]).
schema_constraint_list([A|T]):- 
    schema_constraint(A),
    schema_constraint_list(T).

schema_constraint(=(model, Value)):- 
    model_value(Value).
schema_constraint(=(xmlns, _Value)). % :-
%     xmlns_value(Value).  
schema_constraint(=('xmlns:dt', _Value)). % :- 
%     xmlns_dt_value(Value).  


element_type_list([]).
element_type_list([A|T]):-
    element_type(A),
    element_type_list(T).

element_type(element_type(A, B, C, D, E)):-
    element_type_constraint_list(A),
    attribute_type_list(B),
    element_type_list(C),
    attribute_list(D),
    element_list(E).  

element_type_constraint_list([]).
element_type_constraint_list([A|T]):-
   element_type_constraint(A),
   only_one_occurence(A, T),
   element_type_constraint_list(T).



element_list([]).
element_list([A|T]):-
    element_item(A),
    element_list(T).

element_item(element(A, B)):-
    element_type_name(A),
    element_constraint_list(B).

% The name constraint is required but appears as the first argument of
% element_type/4.

element_constraint_list([]).  
element_constraint_list([A|T]):-
   element_constraint(A),
   only_one_occurence(A, T),
   element_constraint_list(T).

attribute_list([]).
attribute_list([A|T]):-
   attribute(A),
   attribute_list(T).

attribute(attribute(A, B)):-
   attribute_type_name(A), 
   attribute_constraint_list(B).

attribute_constraint_list([]).
attribute_constraint_list([A|T]):-
  attribute_constraint(A),
  only_one_occurence(A, T),
  attribute_constraint_list(T).

attribute_constraint(=(default, Val)):-
  cdata_type_value(Val). % Is implied if there is no constraint. 
attribute_constraint(=(required, Val)):-
  yes_no_value(Val). % Implied.
