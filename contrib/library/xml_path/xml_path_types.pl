
:- module(xml_path_types,
	[
	    canonic_xml_term/1, 
	    canonic_xml_item/1,
	    tag_attrib/1,
	    canonic_xml_query/1,
	    canonic_xml_subquery/1
	],
	[assertions,regtypes]).

:- true prop canonic_xml_term(XMLTerm) + regtype
        # "@var{XMLTerm} is a term representing XML code in canonical form.".

canonic_xml_term(T) :- list(T,canonic_xml_item).

:- true prop canonic_xml_item(XMLItem) + regtype # "@var{XMLItem} is
either a XML attribute, a XML element or a line break.".

% a little strange:
%canonic_xml_item(Term) :- canonic_html_item(Term).
canonic_xml_item(xmldecl(Atts)) :-
        list(Atts,tag_attrib).
canonic_xml_item(env(Tag,Atts,Terms)) :-
        atm(Tag),
        list(Atts,tag_attrib),
        canonic_xml_term(Terms).
canonic_xml_item(Space) :-
% line breaks and so on
	list(Space).
canonic_xml_item(elem(Tag,Atts)) :-
%can be reduced to canonic_xml_item(env(Tag,Atts,[])).
       atm(Tag),
       list(Atts,tag_attrib).

:- true prop tag_attrib(Att) + regtype # "@var{Att} is a XML attribute.".

tag_attrib(Att) :- var(Att).
tag_attrib(Att) :- atm(Att).
tag_attrib((Att = _Val)) :- var(Att).
tag_attrib((_Att = Val)) :- var(Val).
tag_attrib((Att = Val)) :- atm(Att), string(Val).
tag_attrib((Att = Val)) :- atm(Att), number(Val).


:- true prop canonic_xml_query(Query) + regtype # "@var{Query} is a
primitive XML query.".

% The query is on the children elements/atts of the current node:
canonic_xml_query(env(Tag,Atts,Terms)) :-
	canonic_xml_context_node(Tag,Atts,Terms).
% The query is on the descendants and the current node:
canonic_xml_query(//(Tag,Atts,Terms)) :-
	canonic_xml_context_node(Tag,Atts,Terms).

canonic_xml_context_node(Tag,Atts,Terms) :-
       atm(Tag),
	list(Atts,canonic_xml_subquery),
	list(Terms,canonic_xml_subquery).


:- true prop canonic_xml_subquery(SQuery) + regtype # "@var{SQuery}
defines a XML subquery.".

% ^ denotes X is a predicate to execute on the element/attribute or a property
% that must hold on it
% canonic_xml_subquery(^X) :-
%         callable(X).
% A query on anything. It matches any child of the current node
canonic_xml_subquery(X) :-
        var(X).
% A query can be composed by a set of queries:
canonic_xml_subquery(X) :-
        canonic_xml_query(X).
% A query on a string:
canonic_xml_subquery(X) :-
        string(X).
% A query on a number
canonic_xml_subquery(X) :-
        number(X).
% A query on the N-th element/attribute
canonic_xml_subquery([N]) :-
	number(N).	
% It is possible to query on attributes: (e.g.: 'name = Joe')
canonic_xml_subquery(X) :-
	tag_attrib(X).
