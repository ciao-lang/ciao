:- module(example2, [main/0], [dcg, persdb]).

:- use_module(library(xdr_handle)).
:- use_package(pillow).
:- use_module(library(lists)).
:- use_module(library(aggregates)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(system), [current_executable/1]).

:- include(js_location).

:- data id/1.
:- data not_show_xpath/0.

persistent_dir(xdr_dir, './pers') .

:- persistent(xdr_tree_c/1, xdr_dir).

main :- 
	get_form_input(Info),
	get_form_value(Info, xdr_url, XDRUrl1),
	(XDRUrl1 = '' ->
	 XDRUrl='$empty'
	;
	 XDRUrl=XDRUrl1),
	(form_empty_value(XDRUrl) -> 
	 OutPut=[ \\, em('URL of the XDR to be processed: '),\\,
		  nl, input(text,[name=xdr_url, size='75']),
	     	  --,	
		  begin(table, [cols='2']), begin(tbody),
		  nl, begin(tr), begin(td),
		  nl, input(submit,[value='Process this XDR']),
		  end(td),
		  begin(td),
		  input(reset,[value='Reset URL']),
		  end(td),
		  end(tr),
		  end(tbody),
		  end(table)]
	;	
	 (get_form_value(Info, mtimes, yes)->
	  get_form_value(Info, id, Id),
	  data_facts:asserta_fact(id(Id)),
	  xdr_tree_c(XDRTree),
	  unfold_tree_dic(XDRTree, UFT, Dic),
	  process_elems(Info, UFT, Dic, NewXDRTree),
	  xdr_xpath(NewXDRTree, XPath), 
	  retract_all_fact(xdr_tree_c(_)),
	  persdb_rt:asserta_fact(xdr_tree_c(NewXDRTree)),
	  xdr2html(NewXDRTree, HTMLOutput),
	  compute_reply(Info, Reply, XPath),
	  get_output(Reply, XDRUrl, HTMLOutput, OutPut)
	 ;
	 xdr_tree(XDRUrl, XDRTree, Id),
	 data_facts:asserta_fact(id(Id)),
	 (XDRTree=not_available ->
	  OutPut=[ 
	       \\, em('Sorry, that XDR was not available or not correct'), \\,
               \\, em('Please, enter the URL of the XDR to be processed: '),\\,
	       input(text,[name=xdr_url, size='75']),
	       --,		
	       begin(table, [cols='2']), begin(tbody),
	       nl, begin(tr), begin(td),
	       nl, input(submit,[value='Process this XDR']),
	       end(td),
	       begin(td),
	       input(reset,[value='Reset URL']),
	       end(td),
	       end(tr),
	       end(tbody),
	       end(table)]	       
	  ;
           retract_all_fact(xdr_tree_c(_)),
	   persdb_rt:asserta_fact(xdr_tree_c(XDRTree)),
	   xdr2html(XDRTree, HTMLOutput),
	   get_output([], XDRUrl, HTMLOutput, OutPut)
	 ))),
	output_html([cgi_reply,
                     begin(html),
		     title('XPath Interface'),
		     begin(body,[bgcolor="fff8dc",
		                 background='pictures/back2.jpg']),
	             start_form('html_manager.cgi',[name=data]),  
		     OutPut,
		     end_form,
		     end(body),
		     end]).

:- meta_predicate retract_all_fact(fact).
retract_all_fact(T) :-
	T,
	persdb_rt:retract_fact(T),
	fail.
retract_all_fact(_).

get_output(Reply, XDRUrl, HTMLOutput, OutPut) :-
	  id(IdNum),
	  number_codes(IdNum,IdList), 
	  atom_codes(IdX, IdList),
	  get_js(JSL),
	  OutPut=[   JSL,
		     h1('Use this form to generate your Xpath expression'),
     		     em('Processing XDR:   '),verbatim(XDRUrl),
		     --,
     		     Reply,		 
		     begin(table, [cols='2']), begin(tbody),
		     nl, begin(tr), begin(td),
	     	     nl, end(td), end(tr),
		     nl, begin(tr), begin(td),
		     em('You can store your Xpath expression in a new file: '),
		     \\,
     		     input(text,[name=xpath_file, size='50']),
	     	     nl, end(td), end(tr),
		     nl, end(tbody),
		     nl, end(table),
		     --,
		     nl, input(hidden,[name=mtimes, value=yes]),
		     nl, input(hidden,[name=xdr_url, value=XDRUrl]),
		     nl, input(hidden,[name=id, value=IdX]),
		     nl, HTMLOutput,
		     --,		     
		     begin(table, [cols='2']), begin(tbody),
		     nl, begin(tr), begin(td),
		     em('Click here to validate your preferences  '),
		     input(submit,[name=submit1, value='Go!']),
	     	     nl, end(td),
		     nl, begin(td),
		     em('or here, to reset the form  '),
		     input(reset,[value='Reset']),
	     	     nl, end(td), end(tr),
		     nl, end(tbody),
		     nl, end(table)] .

compute_reply(_, [], _):- not_show_xpath, !.
compute_reply(Info, Reply, XPath) :-
	display_html(XPath, HTML),
	get_form_value(Info, xpath_file, File),
	((form_empty_value(File) ; HTML = []) ->
	 SubReply = []
	;
	 current_executable(Exe),
	 absolute_file_name(Exe,'','','.',_,_,Dir),
	 atom_concat([Dir, '/', File], TFile),
	 open(TFile, write, S),	
	 display_all(S, XPath),
	 close(S),
	 SubReply = [em('stored in:   '), \\, \\,
	            verbatim(TFile), --] 
	),
	(HTML = [] ->
	 Message= [em('Please, select your preferences'), \\, \\, --]
	;
	 Message= [em('Xpath expression:   '), \\, \\, HTML, --]
	),
	Reply=[Message,
	       SubReply].

display_all(_, []).
display_all(S, [XPath|Xs]) :-
	display(S, XPath), nl(S),
	display_all(S, Xs).

display_html('', []).
display_html(XPath, H) :-
	H=[verbatim(XPath), \\].

get_js(JSL) :- 
	js_location(JS_file),
	current_input(OldInput),
        open(JS_file, read, S),
        set_input(S),
	get_code(X),
	get_js_aux(X, JSL),
        set_input(OldInput),
        close(S).

get_js_aux(-1, []).
get_js_aux(Char, [Char|R]) :-
	get_code(NewChar),
	get_js_aux(NewChar, R).

% Aqui empieza la generacion antigua (Hay que mantener la parte del arbol y los
% predicados auxiliares. 
process_elems(Info, Tree, Dic, xdr_tree(root, NewBranches)) :-
	Tree=xdr_tree(root, Branches),
	produce_xpath_tree(Info, Dic, Branches, NewBranches).

produce_xpath_tree(_Info, _, [], []).
produce_xpath_tree(Info, Dic, [xdr_tree(Node, Children)|Bs], TreeList) :-
	Node=xdr_node(_, _CList, _AList, _, _Id),
	get_new_node(Info, Dic, Node, NewNode, AnotherNode, Flag),
	produce_xpath_tree(Info, Dic, Children, NewChildren),
	(AnotherNode = [] ->
	 TreeList=[xdr_tree(NewNode, NewChildren)|NBs]
	;
	 update_id(NewChildren, NewChildren2),
	 (Flag=off ->
	  TreeList=[xdr_tree(NewNode, NewChildren),
	            xdr_tree(AnotherNode, NewChildren2)|NBs]
	 ;
	  TreeList=[xdr_tree(AnotherNode, NewChildren2)|NBs])
	),	
	produce_xpath_tree(Info, Dic, Bs, NBs).

get_new_node(Info, Dic, Node, NewNode, AnotherNode, Flag) :-
	Node=xdr_node(Name, CList, AList, _, Id),
	name(NameA, Name),
	NewNode=xdr_node(Name, CList, NewAList, Value, Id),	
	get_new_atts(Info, AList, Dic, Id, NewAList),
	(member(form_elem(Id, NameA, DType, Handlers), Dic) ->
	 get_preferences(Info, DType, Handlers, Value),
	 Handlers=[_,_,MoreField|_],
	 get_form_value(Info, MoreField, Val),
	 (Val='' ->
	  AnotherNode=[],
	  Flag=off
	 ;
	  data_facts:asserta_fact(not_show_xpath),
	  get_alt_node(Node, AnotherNode, Flag)
	 )
	;
	 AnotherNode=[],
	 Flag=off,
	 Value=[]
	).

update_id([], []).
update_id([XDRTree|Xs], [XDRTree2|X2s]) :-
	XDRTree=xdr_tree(xdr_node(Name, CList, AList, Value, _), Children),
	id(IdNum),
	number_codes(IdNum,IdList), 
	atom_codes(Id, IdList),
	XDRTree2=xdr_tree(xdr_node(Name, CList, AList, Value,Id),Children2),
	Id1 is IdNum + 1,
	data_facts:retract_fact(id(IdNum)),
	data_facts:asserta_fact(id(Id1)),
	update_id(Children, Children2),
	update_id(Xs, X2s).
	

get_alt_node(Node, AnotherNode, Flag) :-
	Node=xdr_node(Name, CList, AList, _, _),
	(member(=(minOccurs, Min), CList) ->
	 member(=(maxOccurs, Max), CList),
	 delete(CList, =(minOccurs, Min), CL1),
	 delete(CL1, =(maxOccurs, Max), CL2),
	(Min="0", Max="1" ->
	 Flag=on
	;
         Flag=off
	)
	;
 	(member(=(occurs, "OPTIONAL"), CList) ->
	 Flag=on
	;
         Flag=off
	),
	 member(=(occurs, O), CList),
	 delete(CList, =(occurs, O), CL2)),
	 NewCList=[=(occurs, "REQUIRED")|CL2],	 
	 id(IdNum),
	 number_codes(IdNum,IdList), 
	 atom_codes(Id, IdList),
	 AnotherNode=xdr_node(Name, NewCList, AList, [], Id),
	 Id1 is IdNum + 1,
	 data_facts:retract_fact(id(IdNum)),
	 data_facts:asserta_fact(id(Id1)).

get_new_atts(_,[],_,_,[]).
get_new_atts(Info,[attr(Consts,_)|As],Dic, Id,[attr(Consts,Value)|NAs]) :- 
	member(=(type, Name), Consts),
	name(NameA, Name),	
	member(form_elem(Id, NameA, DType, Handlers), Dic), 
	get_preferences(Info, DType, Handlers, Value),
	get_new_atts(Info, As, Dic, Id, NAs).

% get_preferences(_,_,_,_, [_,_,_,Occurs], '', []):- 	
% 	(Occurs="OPTIONAL" ; Occurs="ZEROORMORE"),!.
get_preferences(_,"element", _, []).
get_preferences(Info, "enumeration", Handlers, P):- 
	!, Handlers=[NameId, On_NameId, _More|_], 
	get_form_value(Info, On_NameId, Op),
	(Op='ignore' ->
	 P=[]
	;
	 findall(Value, member(NameId=Value,Info), ValList),
	 (ValList=[] -> 
	  P=[]
	 ;
	  P=[Op,ValList])
	).
get_preferences(Info,_, [Exp, On_NameId,_More|_], P) :- 
	get_form_value(Info, On_NameId, Op),
	get_form_value(Info, Exp, Val),
	(form_empty_value(Val) -> 
	 P=[]
	;
	 P=[Op,Val]).
