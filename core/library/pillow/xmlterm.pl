:- module(xmlterm,[xmlterm/2,xmlterms/2,canonic_xml_terms_filter/2],[]).

xmlterm(Elem,Term):-
	nonvar(Elem), !,
	xmlterm_1(Elem,Term).
xmlterm(Elem,Term):-
	nonvar(Term), !,
	xmlterm_2(Term,Elem).

xmlterms([Elem|Elems],[Term|Terms]):-
	xmlterm(Elem,Term),
	xmlterms(Elems,Terms).
xmlterms([],[]).

xmlterm_1(env(Name,Attr,Els),Term):- !,
	xmlterms_1(Els,Args),
	Term=..[Name,Attr|Args].
xmlterm_1(Term,'$'(Term)).

xmlterms_1([Elem|Elems],[Term|Terms]):-
	xmlterm_1(Elem,Term),
	xmlterms_1(Elems,Terms).
xmlterms_1([],[]).

xmlterm_2('$'(Contents),Contents):- !.
xmlterm_2(Term,env(Name,Attr,Els)):-
	Term=..[Name,Attr|Args],
	xmlterms_2(Args,Els).

xmlterms_2([Term|Terms],[Elem|Elems]):-
	xmlterm_2(Term,Elem),
	xmlterms_2(Terms,Elems).
xmlterms_2([],[]).

canonic_xml_terms_filter([],[]).
canonic_xml_terms_filter([T|Ts],Cs):-
	canonic_xml_item_filter(T,Cs,Cs0),
	canonic_xml_terms_filter(Ts,Cs0).

canonic_xml_item_filter(env(Tag,Atts,Terms),[env(Tag,Atts,Terms1)|Ts],Ts):- !,
        canonic_xml_terms_filter(Terms,Terms1).
canonic_xml_item_filter(elem(Tag,Atts),[elem(Tag,Atts)|Ts],Ts):- !.
canonic_xml_item_filter(Term,Ts,Ts):-
	irrelevant(Term), !.
canonic_xml_item_filter(Term,[Term|Ts],Ts).

irrelevant(declare(_)).
irrelevant([]).
irrelevant([X|Xs]):-
	formatting_code([X]),
	irrelevant(Xs).

formatting_code(" ").          % white-space
formatting_code("\n").         % line-break
formatting_code("\t").         % tab-character
