:- module(xdr_handle, 
	[
	    xdr_tree/3,
	    xdr_tree/1, 
	    xdr_node/1,
	    xdr2html/4, 
	    xdr2html/2, 
	    unfold_tree/2, 
	    unfold_tree_dic/3,
	    xdr_xpath/2
	],
	[dcg, assertions, isomodes, hiord, regtypes]).

:- use_package(pillow).
:- use_module(library(pillow/pillow_types)).
:- use_module(library(xdr_handle/xdr_types)).
:- use_module(library(aggregates)).
:- use_module(library(lists)).
:- use_module(library(terms), [atom_concat/2]).
:- include(html_format).
:- include(compatible).
:- include(constraints).

:- doc(nodoc,assertions).
:- doc(nodoc,regtypes).
:- doc(nodoc,isomodes).
:- doc(nodoc,hiord).
:- doc(nodoc,dcg).


:- doc(title,"XDR handle library").
:- doc(author, "Jos@'{e} Manuel G@'{o}mez P@'{e}rez").
:- doc(copyright,"@include{DocCopyright.lpdoc}").

:- doc(summary, "XDR documents handling library").

:- doc(module, "This library offers facilities to enable users to
setup preferences on the values an eventual XML document may take. XML
documents are specified by XDR documents (eXternal Data Representation
standard), in a way conceptually similar to that of objects and
classes in object oriented programming. These facilities allow to take
as input an XDR Schema defining the class of documents of interest,
and establish a dialogue with the user via an HTML form that allows
the user to setup preferences to select sub-classes of documents
(those which satisfy the preferences). The preferences are the output
of the process and may be in the form of XPath expressions, for
example, as can be seen in the example attached in the \"examples\"
directory.  ").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XDR PARSING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- data xdr_id/1.
:- doc(hide, data/1).

:- pred xdr_tree(+XDR_url, -XDR_tree, -XDR_id) : url_term * xdr * int 
       # "Parses an XDR (External Data Representation 
       Standard) located at an url @var{XDR_url} into a tree structured Prolog 
       term @var{XDR_tree}. It also returns an identifier  of the XDR_tree 
       @var{XDR_id} corresponding to the sequence of nodes in the tree (this 
       is intended to be a hook to use in CGI applications).".

xdr_tree(XDRUrl, XDRTree, IdNum) :- 
	(url_info(XDRUrl, URLInfo) -> 
	 fetch_url(URLInfo, [], Response),
        member(status(Status,_,_),Response),
	(Status \== success -> 
	 XDRTree = 'not_available',
	 asserta_fact(xdr_id(0))
	;
	 member(content(Content), Response),
	 xml2terms(Content, Terms),
	 member(env(_Name, _Atts,XDR), Terms),
	 findall(elem('AttributeType', AttrConsts), 
	         member(elem('AttributeType', AttrConsts), XDR), 
		 AttTypesListPrev),
	 findall(env('ElementType', ElemConsts, SubElems), 
                 member(env('ElementType', ElemConsts, SubElems), XDR), 
		 ElemTypesListPrev),
	 get_element_type_list(ElemTypesListPrev, ElemTypesList), 
	 get_att_type_list(AttTypesListPrev, AttTypesList), 
	 asserta_fact(xdr_id(1)),
	 build_tree_prev(ElemTypesList, AttTypesList, XDRTree),
	 xdr_tree(XDRTree))
	;
	 XDRTree='not_available',
	 asserta_fact(xdr_id(0))),
        xdr_id(IdNum).

get_element_type_list([], []).
get_element_type_list([ElemPrev|ElemPrevs], 
	              [element_type(A,B,C,D,E)|Elems]) :-
        ElemPrev=env('ElementType', ElemConsts, SubElems), 
	A=ElemConsts,
	findall(elem('AttributeType', AttrConsts), 
                member(elem('AttributeType', AttrConsts), SubElems), B),
	get_element_type_list(SubElems, C),
	findall(elem(attribute, Aatts), 
                member(elem(attribute, Aatts), SubElems), D),
	findall(elem(element, Eatts), 
	member(elem(element, Eatts), SubElems), E),
	get_element_type_list(ElemPrevs, Elems).
get_element_type_list([_|ElemPrevs], Elems) :-
	get_element_type_list(ElemPrevs, Elems).

get_att_type_list([], []).
get_att_type_list([elem('AttributeType', AttrConsts)|AttPrevs], 
	          [AttrConsts|Atts]) :-
	get_att_type_list(AttPrevs, Atts).

build_tree_prev(ElemTypeList, GlobalAttTypeList, 
	        xdr_tree(root, AllChildrenList)) :-
	build_tree(ElemTypeList, ElemTypeList,
	           GlobalAttTypeList, AllChildrenList).

build_tree(Elt, ETL, GAtList, xdr_tree(Node, ChildrenList)) :-
       build_node(Elt, GAtList, Node),
	get_children(Elt, ETL, GAtList, ChildrenList).	
build_tree([],_,_,[]).
build_tree([Elt|Elts], ETL, GlobalAttsList,
           [xdr_tree(Node, ChildrenList)|XDRTrees]) :-
       build_node(Elt, GlobalAttsList, Node),
	get_children(Elt, ETL, GlobalAttsList, ChildrenList),
	build_tree(Elts, ETL, GlobalAttsList, XDRTrees).

build_node(element_type(ConstsList,
	                 AttsTypeList,
			   _ElemsTypeList,
			   AttsList,
			  _ElemsList),
	   GlobalAttsTypesList, 
	   xdr_node(Name, ConstsList, FinalAttsList, [], Id)) :-
       xdr_id(IdNum),
	number_codes(IdNum,IdList), 
	atom_codes(Id, IdList),
	(member(=(name, Name), ConstsList) ->
	 true
	;
	 member(=(type, Name), ConstsList)),
	 obtain_atts(AttsList, AttsTypeList, 
                    GlobalAttsTypesList, FinalAttsList),
	 Id1 is IdNum + 1,
	 retract_fact(xdr_id(IdNum)),
	 asserta_fact(xdr_id(Id1)).

obtain_atts([],_,_,[]).
obtain_atts([A|As], ATs, GATs, [attr(FA, [])|FAs]) :-
	A=elem(attribute, AConsts),
	member(=(type, AType), AConsts),
	obtain_AT_cons(AType, ATs, ATConstsPrev),
	(ATConstsPrev = [] ->
	 obtain_AT_cons(AType, GATs, ATConsts)
	;
	 ATConsts = ATConstsPrev
	),
	get_sub_AT_consts(AConsts, ATConsts, SubATConsts),
	append(AConsts, SubATConsts, FA),
	obtain_atts(As, ATs, GATs, FAs).

obtain_AT_cons(_, [], []).
obtain_AT_cons(Name, [AT|_ATs], ATRes) :-
	member(=(name, Name), AT),
	delete(AT, =(name,Name), ATRes).
obtain_AT_cons(Name, [_AT|ATs], ConstsList) :-
	obtain_AT_cons(Name, ATs, ConstsList).

get_sub_AT_consts(_, [], []).
get_sub_AT_consts(As, [AT|ATs], Ss) :-
	member(AT, As),
	get_sub_AT_consts(As, ATs, Ss).
get_sub_AT_consts(As, [AT|ATs], [AT|Ss]) :-
	get_sub_AT_consts(As, ATs, Ss).

get_children(Elt, LTotal, GlobalAttsList, ChildrenList) :-
	Elt=element_type(_, _, ElementTypeList, _, ElemsList), 
	process_elements(ElemsList, ElementTypeList, LTotal, GlobalAttsList, ChildrenList).

process_elements([], [], _, _, []).
process_elements([], [E|Es], LTotal, GlobalAttsList, [Child|Children]) :-
	E=element_type(_,B,_,_,_),
	append(B,GlobalAttsList,NewGlobalAttsList),
	build_tree(E, LTotal, NewGlobalAttsList, Child),
	process_elements([], Es, [E|LTotal], GlobalAttsList, Children).	
process_elements([El|Els], ElementTypeList, LTotal, GlobalAttsList, [Child|Children]) :- 
	El=elem(element, Eatts),
	append(ElementTypeList, LTotal, NewLTotal),
	member(=(type, ElementTypeName), Eatts),
	member(element_type([=(name, ElementTypeName)|RCs],B,C,D,E),
	       NewLTotal), 
	append(B,GlobalAttsList,NewGlobalAttsList),
	append(Eatts, RCs, NewRCs),
	build_tree(element_type(NewRCs,B,C,D,E),
	           NewLTotal, 
	           NewGlobalAttsList,
	           Child),
	process_elements(Els, ElementTypeList, LTotal, GlobalAttsList, Children).
	

:- pred xdr_tree(XDR_tree) : xdr # "Checks the correctness of an XDR tree 
   @var{XDR_tree}.".

xdr_tree(xdr_tree(Node, Branches)) :-
	xdr_node(Node),
	xdr_tree_list(Branches).   

xdr_tree_list([]).   
xdr_tree_list([A|T]):-
	xdr_tree(A),
	xdr_tree_list(T).

:- true prop xdr_node(XDR_node) + regtype
        # "@var{XDR_node} is a XDR tree node.".

xdr_node(root).
xdr_node(xdr_node(A, B, C, _, _)):-
	element_type_name(A),
	xdr_node_constraint_list(B),
	attribute_type_constraint_list_list(C).


attribute_type_constraint_list_list([]).
attribute_type_constraint_list_list([C1|Cs]) :- 
	C1=attr(ATL, _APrefs),
	attribute_type_constraint_list(ATL),
	attribute_type_constraint_list_list(Cs).

xdr_node_constraint_list([]).  
xdr_node_constraint_list([A|T]):-
	xdr_node_constraint(A),
	only_one_occurence(A, T),
	xdr_node_constraint_list(T).

xdr_node_constraint(A):- element_constraint(A).
xdr_node_constraint(A):- element_type_constraint(A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XDR <-> HTML
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred  xdr2html(+XDRTree, -HTMLOutput) : xdr * html_term 
   # "Receives an XDR tree @var{XDRTree} and produces the corresponding HTML 
   code @var{HTMLOutput}. This html code is  intended to be part of a form 
   used as a means by which an eventual user can give value to an instance of 
   the XDR, i.e. an XML element.".

xdr2html(XDRTree, HTMLOutput) :-	
	xdr2html(XDRTree, HTMLOutput,_,_).

:- pred xdr2html(+XDRTree, -HTMLOutput, -UnfoldedTree, -Dic) : xdr *
   html_term * xdr * form_dict # "Receives an XDR tree @var{XDRTree}
   and produces the corresponding HTML code @var{HTMLOutput}, an
   equivalente unfolded plain tree @var{UnfoldedTree} and a control
   dictionary @var{Dic} to hold a reference the evenutal fom
   objects. ".

xdr2html(XDRTree, HTMLOutput, UnfoldedTree, Dic) :-
	XDRTree=xdr_tree(root, Branches),
	traverse_brothers(Branches, Branches, HTMLOutput, TO, Dic),
	UnfoldedTree=xdr_tree(root, TO).

:- pred unfold_tree(+XDRTree, -UFT) : xdr * xdr 
   # "Obtains an unfolded XDR tree @var{UFT} from a standard XDR tree
   @var{XDRTree}, i.e.  an XDR tree where all references to XDR
   elements have been substituted with the elements
   themselves. Especially useful for eventual generation of equivalent
   XPATH expressions, (see example).".

unfold_tree(XDRTree, UFT) :-	
	xdr2html(XDRTree,_, UFT,_).

:- pred unfold_tree_dic(+XDRTree, -UFT, -Dic) : xdr * xdr * form_dict
   # "Obtains an unfolded XDR tree @var{UFT} and a form dictionary
   @var{Dic} from a standard XDR tree @var{XDRTree}. Especially useful
   for HTML form data exchange (see example).".

unfold_tree_dic(XDRTree, UFT, Dic) :-	
	xdr2html(XDRTree,_, UFT, Dic).

traverse_brothers([],_,[],[],[]).
traverse_brothers([XDRTree|Xs], Ambito, HTMLResult, TResult, Dic) :-
	XDRTree=xdr_tree(Node, Branches),
	Node=xdr_node(Name, ConstsList,_,_,_),
	member(=(name, Name), ConstsList), !,
	(is_referenced(Name, Ambito) ->
	 true,
	 HTMLResult=Hs,
	 TResult=Ts,
	 Dic=Ds,
	 NewAmbito=Ambito
	;
	 append(Branches, Ambito, NewAmbito),
	 process_node(XDRTree, Ambito, HTMLOutput, T, D),
	 TResult=[T|Ts],
	 HTMLResult=[HTMLOutput|Hs],
	 append(D,Ds,Dic)
	),
	traverse_brothers(Xs, NewAmbito, Hs, Ts, Ds).
traverse_brothers([XDRTree|Xs],Ambito,[HTMLOutput|Hs],[TO|Ts], Dic) :-
	XDRTree=xdr_tree(_, Branches),
	append(Branches, Ambito, NewAmbito),
	process_node(XDRTree, Ambito, HTMLOutput, TO, D),
	traverse_brothers(Xs, NewAmbito, Hs, Ts, Ds),
	append(D,Ds,Dic).

process_node(XDRTree, Ambito, [HTMLOutput|itemize(Hs)],
	xdr_tree(Node, TOs), Dic) :- 
	XDRTree=xdr_tree(Node, Branches),
	Node=xdr_node(Name, ConstsList, AttsList, P, Id),
	get_HTML(Name, ConstsList, AttsList, P, Id, HTMLOutput, D),
	append(Branches, Ambito, NewAmbito),
	traverse_brothers(Branches, NewAmbito, Hs, TOs, Ds),
	append(D,Ds,Dic).
	
is_referenced(_, []) :- fail.
is_referenced(Name, [XDRTree|_]) :-
	XDRTree=xdr_tree(xdr_node(_,CL,_,_,_),Children),
	(member(=(type, Name), CL) ->
	 true
	;
	 is_referenced(Name, Children)
	).
is_referenced(Name, [_|Xs]) :-
	is_referenced(Name, Xs).

get_HTML(Name, ConstsList, AttsList, P, Id, HTML, Dic) :-
	name(NameA, Name),
	get_occurs(ConstsList, Occurs),
	((member(=('dt:type', DTType), ConstsList);
	  (member(=(content, DTType), ConstsList), DTType = "textOnly"))->
	 compatible(DTType, DTTypeC),
	 get_area_handlers(DTTypeC, Name, Id, ConstsList,Handlers,DVList,Vals),
	 html_format(DTTypeC, Occurs, Handlers, Vals, P, HTMLelem),
	 D=form_elem(Id, NameA, DTTypeC, DVList),
	 Dic=[D|Ds]
	;
	    (Occurs=no ->
	     html_format("element", _, NameA, _, _, HTMLelem),
	     Dic=Ds
	    ;
	     get_area_handlers("element", Name, Id, ConstsList, 
	                       Handlers, DVList, Vals),
	     html_format("element", Occurs, Handlers, Vals, P, HTMLelem),
	     D=form_elem(Id, NameA, "element", DVList),
	     Dic=[D|Ds]
	    )
	),
	get_attr_HTML(Id, AttsList, HTMLattr, Ds),
	append(HTMLelem, [\\, HTMLattr], HTML).

get_area_handlers("enumeration", Name, Id, ConstsList, H,
                  [NameId, On_NameId, More, Occ], Values) :-
	member(=('dt:values', LValues), ConstsList),
	enum_vals(LValues, Values),
	process_names(Name, Id, NameA, NameId, On_NameId,_, Occ, More),
 	H = [NameA, NameId, On_NameId, Occ, More].
get_area_handlers(DTType, Name, Id, _, H, [Exp, On_NameId, More, Occ], []) :-
	(DTType = "date"; DTType = "time"), !,
	process_names(Name, Id, NameA, NameId, On_NameId, Exp, Occ, More),  
	atom_concat([NameId, '_1'], NameId1),
	atom_concat([NameId, '_2'], NameId2),
	atom_concat([NameId, '_3'], NameId3),
	H = [NameA, NameId, On_NameId, Occ, More, Exp, NameId1, NameId2, NameId3]. 
get_area_handlers(_DTType, Name, Id, _, H, [Exp, On_NameId, More, Occ], []) :-
	process_names(Name, Id, NameA, NameId, On_NameId, Exp, Occ, More),  
	H = [NameA, NameId, On_NameId, Occ, More, Exp].

process_names(Name, Id, NameA, NameId, On_NameId, Exp_NameId, Occurs, More) :-
	append("on_", Name, On_Name),
	name(NameA, Name),	 
	name(On_NameA, On_Name),
	atom_concat(NameA, Id, NameId),
	atom_concat(occurs_, NameId, Occurs),
	atom_concat(more_, NameId, More),
	atom_concat(exp_, NameId, Exp_NameId),
	atom_concat(On_NameA, Id, On_NameId).


get_occurs(ConstsList, Occurs) :-
	(member(=(minOccurs, Min), ConstsList) ->
	  member(=(maxOccurs, Max), ConstsList),
	  occurs_syntax(Min, Max, Occurs)
	 ;
	  (member(=(occurs, Occurs), ConstsList) ->
	   true
	  ;
	   member(=(required, Occ), ConstsList),
	   attr_occurs(Occ, Occurs))).
get_occurs(_, no).

occurs_syntax("1", "1", "REQUIRED").
occurs_syntax("0", "1", "OPTIONAL").
occurs_syntax("1", "*", "ONEORMORE").
occurs_syntax("0", "*", "ZEROORMORE").

attr_occurs("yes", "REQUIRED").
attr_occurs("no", "OPTIONAL").
	
get_attr_HTML(_,[],[],[]).
get_attr_HTML(Id, [attr(AttList, P)|As], [HTML|Hs], [D|Ds]) :-
	member(=(type, Name), AttList),
	get_HTML(Name, AttList, [], P, Id, HTML, LD),
%	LD=D,
	LD=[D],
	get_attr_HTML(Id, As, Hs, Ds).
	
enum_vals([],[]).
enum_vals([L|Ls], Vs) :-
	white(L),
	enum_vals(Ls, Vs).
enum_vals(Ls, [Value|Vs]) :-
	enum_value(Ls, Value, R),
	enum_vals(R, Vs).

enum_value([], [], []).
enum_value([L|Ls], [], Ls) :-
	white(L).
enum_value([L|Ls], [L|Vss], R) :-	
	enum_value(Ls, Vss, R).
	
white(10).
white(13).
white(32).
white(9).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XDR -> HTML FORM -> XPATH EXPRESSION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred xdr_xpath(+XDRTree, -XPath) : xdr * atm # "Produces an XPATH
   expression @var{XPath} from an XDR tree @var{XDRTree}. If the given
   XDR tree has no definite value the xpath expression produced will
   be empty".

xdr_xpath(XDRTree, XPath) :-
	XDRTree=xdr_tree(Node, Children),
	xpath_process_children(Node, Children, ExpList), 
	xpath_process_node(Node, [AttList, PrefList]),
	(AttList = [], PrefList = [], ExpList \== []->
	 (Node=root ->
	  NodeExp='/'
	 ;
          Node=xdr_node(Name,_,_,_,_),
	  name(NodeExp, Name))
	;
	 (AttList = [] ->
	  PrefExpList=PrefList
	 ;
	 encapsulate(PrefList, PrefExpList)
	 ),
	 append(AttList, PrefExpList, NodeExpList),
	 atom_concat(NodeExpList, NodeExp)),
	(Node = root, length(ExpList, 1) ->
	 EncExpList=ExpList
	;
	encapsulate(ExpList, EncExpList)),
	atom_concat([NodeExp|EncExpList], XPath).

encapsulate([], []).
encapsulate([N|Ns], [NE|NEs]) :-
	atom_concat(['[', N, ']'], NE),
	encapsulate(Ns, NEs).

xpath_process_children(_, [],[]).
xpath_process_children(N, [Child|Cs], LEs) :-
	xdr_xpath(Child, E),
	xpath_process_children(N, Cs, Es),
	(E ='' -> 
	 LEs=Es
	;
	 LEs=[E|Es]).

xpath_process_node(root, [[],[]]).
xpath_process_node(xdr_node(Name, _, AList, ProtoPrefs,_), XPathList) :-
	name(NameA, Name),
	transform(el, _, ProtoPrefs, Prefs,_),
	xpath_process_attributes(AList, Aexps),
	(Aexps = [] ->
	 NodeAttList=[]
	;
	 atom_concat([NameA|Aexps], NodeAtt),
	 NodeAttList=[NodeAtt]),
	(Prefs = '' ->
	 NodePrefList=[]
	;
	 NodePrefList=[Prefs]),
	 XPathList=[NodeAttList, NodePrefList].

xpath_process_attributes([], []).
xpath_process_attributes([attr(_,[])|As], Es) :-
	xpath_process_attributes(As, Es). 
xpath_process_attributes([attr(Consts, A)|As], [E|Es]) :-
	member(=(type, Name), Consts),
	name(NameA, Name),	
	transform(at, NameA, A, NewA,Type),
	(Type = enum ->
	 E=NewA
	;
	 xpath_process_attribute(NewA, E)), 
	xpath_process_attributes(As, Es). 

xpath_process_attribute(A, E) :-
	name(A, AList),
	get_att_by_grammar(AList, EList),
	name(EAux, EList),
	atom_concat(['[', EAux, ']'], E).
	
transform(ElType, Name, [Op, L], Prefs, enum) :-
	(Op=in; Op='not in'),
	trans_enum(ElType, Op, Name, L, Prefs).
transform(_, _, [_, Prefs], Prefs, normal).
transform(_, _, [], '', normal).


trans_enum(ElT, Op, Name, Vs, '') :-
	trans_enum_aux(ElT, Op, Name, Vs, '').
trans_enum(ElT, Op, Name, Vs, P) :-
	trans_enum_aux(ElT, Op, Name, Vs, Paux),
	atom_concat(['[', Paux, ']'], P).

trans_enum_aux(_,_,_,[], '').
trans_enum_aux(ElT, Op, Name, [V|Vs], P) :-
	(Op = 'in' -> 
	 Connector=' or ',
	 Eq=' = ' 
	;
	 Connector=' and ',
	 Eq=' != '),
	(ElT = at ->
	 Cat = '@'
	;
	 Cat = ''
	),
	trans_enum_aux(ElT, Op, Name, Vs, Paux),
	(Paux = '' ->
	 PP=''
	;
	 atom_concat([Connector, Paux], PP)
	),
	atom_concat([Cat, Name, Eq, V, PP], P).


get_att_by_grammar([], []).
get_att_by_grammar(S, SRes) :-
	expr(SRes, S, []).

expr(E) --> exp(E1),"or ",expr(E2),{append("or ", E2, E21),append(E1, E21, E)}.
expr(E) --> exp(E1),"and ",expr(E2),{append("and ",E2, E21),append(E1, E21,E)}.
expr(E) --> exp(E) .

exp([0'(|L]) --> "(", exp(L).
exp([0'@, R|Resto]) --> [R], body(Resto), {R\==0'(}.

body([C]) --> [C], {C=10}, !.
body([C]) --> [C], {C=13}, !.
body([C]) --> [C], {C=32}, !.
body([C]) --> [C], {C=9}, !.
body([C|Cs]) --> [C], {C=0')}, !, body(Cs).
body([C|Cs]) --> [C], !, body(Cs).
body("") --> "".
