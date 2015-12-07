element_type_constraint(=(name, Val)):-
  cdata_type(Val). 
element_type_constraint(=(content, Val)):-
   element_type_content_value(Val).
element_type_constraint(=(model, Val)):-
   model_value(Val). 
% Implied. If no model value, inherits from Schema, i.e. default is "open".
element_type_constraint(=('dt:type', Val)):-
  cdata_type(Val). 
% Implied. dt:type allowed only if content="textOnly".
element_type_constraint(=('dt:values', Val)):-
  dt_value_enumeration(Val). 
element_type_constraint(=(order, Val)):-
   element_type_order_value(Val). 
element_type_constraint(Constraint):-
   element_constraint(Constraint). 	
% Implied. If content is "mixed", default order is "many"; if "eltOnly", "seq".
% datatype daughter allowed only if no dt:type and content="textOnly".
element_type_constraint(=(description, Val)):-
   string(Val). 

 %% This is not actually a constraint: it corresponds to the description
 %% elements in the XDR document, e.g.
 %% 
 %%   <description>Basic XDR schema definition</description>

%% Define:

element_occurs("REQUIRED").
element_occurs("OPTIONAL").
element_occurs("ONEORMORE").
element_occurs("ZEROORMORE").

dt_value_enumeration(_). 

element_type_content_value("empty").
element_type_content_value("textOnly").
element_type_content_value("eltOnly").
element_type_content_value("mixed").
% Default is  "mixed".

model_value("open"). 
model_value("closed").

% Need to be defined at smaller grain level.

cdata_type(A):- string(A).

cdata_type_value(_Val).

 %% Default is "open" for Schemas. For ElemenType's, if there is no model
 %% value, inherits from Schema, i.e. default is 'open'.

element_type_order_value("seq").
element_type_order_value("one").
element_type_order_value("all").
element_type_order_value("many"). 


%%%%%

element_constraint(=(occurs, Val)):-
   element_occurs(Val).
element_constraint(=(type, Val)):-
  cdata_type(Val). 
element_constraint(=(minOccurs, [Val])):- 
   nnegint(Val).
element_constraint(=(minOccurs, ['*'])). 
element_constraint(=(maxOccurs, [Val])):- 
   nnegint(Val).
element_constraint(=(maxOccurs, ['*'])).

 %% Version in ``XML-Data Reduced''. Draft 3 July 1998, version
 %% 0.21. C. Frankston (Microsoft) and H. S. Thompson (University of
 %% Edinburg) Editors.  http://www.ltg.ed.ac.uk/~ht/XMLData-Reduced.htm

 %% element_constraint(=(occurs, Val)):- % Default is "1:1"
 %%    occurs_value(Val).
 %% 
 %% occurs_value(':'(MinOccurs, MaxOccurs)):-
 %%    occurs_limit(MinOccurs), 
 %%    occurs_limit(MaxOccurs).
 %% occurs_value(required).     % 1:1
 %% occurs_value(optional).     % 0:1
 %% occurs_value(oneOrMore).    % 1:
 %% occurs_value(zeroOrMore).   % 0: 
 %% 
 %% occurs_limit(A):- var(A).
 %% occurs_limit(A):- nnegint(A).

%%%

:- push_prolog_flag(unused_pred_warnings, no).
attribute_type_list([]).
attribute_type_list([A|T]):-
   attribute_type(A),
   attribute_type_list(T).
 
attribute_type(attribute_type(A, B)):- 
       attribute_type_name(A), 
       attribute_type_constraint_list(B).

attribute_type_name(Type):- string(Type).
% Type has to be the name of a AttributeType declaration.
:- pop_prolog_flag(unused_pred_warnings).

attribute_type_constraint_list([]).
attribute_type_constraint_list([A|T]) :-
  attribute_type_constraint(A),
  only_one_occurence(A, T),
  attribute_type_constraint_list(T).

attribute_type_constraint(=(type, Val)):-
  cdata_type(Val). 
attribute_type_constraint(=('dt:type', Val)):-
  cdata_type(Val). % Default is "string"
attribute_type_constraint(=('dt:values', Val)):-
  dt_value_enumeration(Val). 
attribute_type_constraint(=(default, Val)):-
  cdata_type_value(Val). % Is implied if there is no constraint. 
attribute_type_constraint(=(required, Val)):-
  yes_no_value(Val). % Default is "no"

yes_no_value("yes").
yes_no_value("no").

element_type_name(Type):- 
   string(Type).
   % cdata_type_value(Type). ?
% Type has to be the name of an ElementType.

% The name constraint is required but appears as the first argument of
% element_type/4.



only_one_occurence(_, []).
only_one_occurence(A, [B|T]):-
     no_repeated(A, B),
     only_one_occurence(A, T).

no_repeated(=(A, _), =(B, _)):- \+ A = B. 
