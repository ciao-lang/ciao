:- module(xml_path_rt, 
	[
	    '$xml_query'/3, 
	    xml_search/3, 
	    '$xml_search_match'/3,
	    xml_parse/3,
	    xml_parse_match/3,
	    xml_index/1,
	    xml_index_to_file/2,
	    xml_index_query/3,
	    get_query_and_constraint/3,
	    continue_index_querying/4
%	    clpfd_call/1
	], 
	[assertions, isomodes, hiord, regtypes]).

:- use_package(fd).

:- use_module(library(goal_trans), [add_goal_trans/3]).

:- use_module(library(fd/fd_tr), [fd_tr/2]).

:- use_module(library(xml_path/xml_path_types)).
:- use_module(library(pillow/xmlterm), [canonic_xml_terms_filter/2]).

:- use_module(library(lists)).
:- use_module(library(dynamic)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(streams)).
:- use_module(library(pretty_print)).

:- doc(title,"XML Querying Library").
:- doc(author, "Jos@'{e} Manuel G@'{o}mez P@'{e}rez").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XML SEARCH
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred xml_search(+Query, +Source, -Doc) : canonic_xml_query *
   canonic_xml_item * canonic_xml_item # "Checks a high level query
   @var{Query} against an XML document @var{Source}. If the query is
   successful it retuns in @var{Doc} the whole xml element(s) of the
   document that matched it.".

xml_search(UserQuery, SourceDoc, Doc) :-
	canonic_xml_terms_filter(SourceDoc, Doc), 
 	get_query_and_constraint(UserQuery, BasicQuery, Clp),
%	(Clp = [] -> true; clpfd_call(Clp)),
	(Clp = [] -> true; call(Clp)),
	'$xml_query'(BasicQuery, Doc,_).

:- pred xml_parse(+Query, +Source, -Doc) : canonic_xml_query *
     canonic_xml_item * canonic_xml_item # "Checks a high level query
     @var{Query} against an XML document @var{Source}. If the query is
     successful it retuns in @var{Doc} the whole xml element(s) of the
     document that matched it. On the contrary as @pred{xml_search/3},
     the query can start at any level of the XML document, not
     necessarily at the root node.".

xml_parse(Query, SourceDoc, SubDoc) :-
	canonic_xml_terms_filter(SourceDoc, Doc), 
	do_xml_parse(whole, Query, Doc, SubDoc).

:- pred xml_parse_match(+Query, +Source, -Match) : canonic_xml_query *
       canonic_xml_item * canonic_xml_item # "Checks a high level
       query @var{Query} against an XML document @var{Source}. If the
       query is successful it retuns in @var{Doc} the exact subtree of
       the xml document that matched it. On the contrary as
       @pred{'$xml_search_match/3}, the query can start at any level
       of the XML document, not necessarily at the root node.".

xml_parse_match(Query, SourceDoc, Match) :-
	canonic_xml_terms_filter(SourceDoc, Doc), 
	do_xml_parse(match, Query, Doc, Match).

do_xml_parse(Flag, UserQuery, Doc, Match) :-
 	get_query_and_constraint(UserQuery, BasicQuery, Clp),
%	(Clp = [] -> true; clpfd_call(Clp)),
	(Clp = [] -> true; call(Clp)),
	 (Flag = whole ->
	  '$xml_query'(BasicQuery, Doc, _),
	  Match = Doc
	 ;
	  '$xml_query'(BasicQuery, Doc, Match)
	 ).
do_xml_parse(Flag, Query, Doc, SubMatch) :-
	member(env(_,_,Elems), Doc),
	xml_parse_children(Elems, Flag, Query, SubMatch).

xml_parse_children([E|_], Flag, Query, Doc) :-
	do_xml_parse(Flag, Query, [E], Doc), !.
xml_parse_children([_|Es], Flag, Query, Doc) :-
	xml_parse_children(Es, Flag, Query, Doc).


:- pred '$xml_search_match'(+BasicQuery, +SourceDoc, -Match) :
      canonic_xml_query * canonic_xml_item * canonic_xml_item #
      "Checks query @var{Query} against an XML document
      @var{Source}. If the query is successful it retuns in @var{Doc}
      the exact subtree of the xml document that matched it.".

'$xml_search_match'(BasicQuery, SourceDoc, Match) :-
	canonic_xml_terms_filter(SourceDoc, Doc), 
	'$xml_query'(BasicQuery, Doc, Match).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XML INDEXING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile xml_attribute/4.
:- dynamic xml_attribute/4.

:- multifile xml_element/4.
:- dynamic xml_element/4.

:- data id/1.

% Retrieving data:

:- pred xml_index_query(+Query, -Id, -Match) : canonic_xml_query * atm
	* canonic_xml_item # " Matches a high level query @var{Query}
	against an XML document previously transformed into a Prolog
	program. @var{Id} identifies the resulting document
	@var{Match}, which is the exact match of the query against the
	XML document. ".

xml_index_query(Query, Id, Doc) :- 
  	get_query_and_constraint(Query, BasicQuery, Clp),
%	(Clp = [] -> true; clpfd_call(Clp)),
	(Clp = [] -> true; call(Clp)),
	do_xml_index_query('', BasicQuery, Id ,Doc).

do_xml_index_query(Key, env(Tag, SubQueryAtts, SubQueryEls),
                   Id,  env(Tag, Atts, Elems)) :-
	atom_concat([Key, '$', Tag], Key_Tag), 
	atom_concat(Key_Tag, '_att', Key_Tag_Att),
	call_attribute_queries(SubQueryAtts, Key_Tag_Att, Id, Atts),
	((SubQueryEls = [Elems], ground(Elems), string(Elems))   ->

	 Elems=[C|_],
	 xml_element(Key_Tag, [C], Id, [Elems])
	;
% Watch out: if var(Elems), in case of succes, SubQueryEls will be instantiated
% Problem in retrieving N solutions. Needs to refresh query. It can be fixed 
% here but would need copy_term/2 -> Left to the programmer.
	 xml_element(Key_Tag, SubQueryEls, Id, Elems)
        ).

call_attribute_queries([QA|Qs], Key_Tag_Att, Id, [Att|As]) :-
	call_attribute_queries(Qs, Key_Tag_Att, Id, As),
	QA = =(Name, Val),
	((ground(Val), string(Val)) ->
	 Val = [C|_],
	 Att = QA,
	 xml_attribute(Key_Tag_Att, [Name=C], Id, [Att])
	;
	 xml_attribute(Key_Tag_Att, [QA], Id, [Att])
	).
call_attribute_queries([],_,_,[]) .

continue_index_querying([Query|Qs], Key, Id, [El|Elems]) :-
	do_xml_index_query(Key, Query, Id, El),
	xml_element(Key, Qs, Id, Elems).
continue_index_querying([], _Key, _Id, []) .

% Generating xml index: 

:- pred xml_index(SourceDoc) : canonic_xml_item # "Transforms the XML
   document @var{SourceDoc} in a Prolog program which is output to
   file @var{File}.".

xml_index_to_file(SourceDoc, File) :-
	canonic_xml_terms_filter(SourceDoc, Doc),
	open_output(File, S),
	pretty_print([
		directive(module(_,[],[])),
%		directive(use_package(fd)),
		directive(use_module(library(terms),[atom_concat/2])),
		directive(set_prolog_flag(discontiguous_warnings, off)),
		directive(set_prolog_flag(single_var_warnings, off)),
		directive(use_module(library(xml/xml_rt), 
		          [continue_index_querying/4])), 
		directive(reexport(library(xml/xml_rt),[xml_index_query/3])),
 		directive(multifile(xml_attribute/4)),
		directive(dynamic(xml_attribute/4)),
 		directive(multifile(xml_element/4)),
		directive(dynamic(xml_element/4))
		     ], []),
	nl,
	do_xml_index(Doc, file),
	close_output(S).


:- pred xml_index(SourceDoc) : canonic_xml_item # "Transforms the XML
   document @var{SourceDoc} in a Prolog program, generating the
   associated clauses, which are stored dynamically into the current
   process memory space.".

xml_index(SourceDoc) :-
	canonic_xml_terms_filter(SourceDoc, Doc),
	do_xml_index(Doc, db).

do_xml_index([],_) :- !.
do_xml_index(Doc, Output) :-
	select(env(Tag, Atts, Elems), Doc, DocRest), !,
	retract_fact(id(N)),
	N1 is N + 1,
	asserta_fact(id(N1)),
	process_root_element(env(Tag, Atts, Elems), '', N1, Output),
	do_xml_index(DocRest, Output).
do_xml_index(_,_).

process_root_element(env(Tag, Atts, Elems), Preffix, N, Output) :-
	atom_concat([Preffix, '$', Tag], NewTag), 
	process_elems(Elems, NewTag, N, Output),
	process_atts(Atts, NewTag, N, Output).

% Optimization 1: Use element index.
% Optimization 2: Asserta for "ground" elems/atts during indexing phase and
% assertz for "nonground" ones. Proved to be useles.

process_elems([], Tag, N, Output)  :- !,
 	my_asserta(Output, xml_element(Tag, [], N, []), true).
process_elems(Elems, Tag, N, Output) :-
	(member(env(_,_,_), Elems) ->
	 my_asserta(Output, xml_element(Tag, SubQueries, Id, Elements), 
	      (!, continue_index_querying(SubQueries, Tag, Id, Elements)))
	;
	 Elems = [ElemsIn],
	 simple_check(NEls, ElemsIn),
	 NEl = [NEls],
	 (NEls = [C|_] ->
	  my_asserta(Output, xml_element(Tag, [C], N, NEl), true)
	 ;
	  my_asserta(Output, xml_element(Tag, NEl, N, NEl), true))
	),
	process_children(Elems, Tag, N, Output).

process_children([],_,_,_) .
process_children([Elem|Es], Preffix, N, Output) :-
	(Elem = env(_,_,_) ->
	 process_root_element(Elem, Preffix, N, Output)
	;
	 true),
	process_children(Es, Preffix, N, Output).

process_atts([], Tag,_,Output)  :-
 	atom_concat(Tag, '_att', TagAtt),
 	my_asserta(Output, xml_attribute(TagAtt,[],_,[]), true).
process_atts(Atts, Tag, N, Output) :-
	atom_concat(Tag, '_att', TagAtt),
	do_process_atts(Atts, TagAtt, N, Output) .
	
do_process_atts([], Tag, N, Output)  :-
	my_asserta(Output, xml_attribute(Tag, [], N, []), true).
do_process_atts(Atts, Tag, N, Output) :-
	select(Att=AttVal, Atts, RestAtts),
	simple_check(NVal, AttVal),
 	(NVal = [C|_] ->
 	 my_asserta(Output, 
	            xml_attribute(Tag, [Att=C], N, ClAtts), ClAtts = [Att=NVal])
 	;	 
	 my_asserta(Output,
	            xml_attribute(Tag, [Att=NVal], N, ClAtts), ClAtts = [Att=NVal])
	),
	do_process_atts(RestAtts, Tag, N, Output).

:- meta_predicate my_asserta(?, fact, ?).
my_asserta(db, H, B) :- !,
 	(clause(H, B) ->
 	 true
 	;
 	 asserta((H :- B))).

my_asserta(file, H, B) :-	
	pretty_print([(H :- B)], []), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XML QUERY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred '$xml_query'(+Query, +Doc, -Match) : canonic_xml_query *
  canonic_xml_item * canonic_xml_item # "Checks that XML document
  @var{Doc} is compliant with respect to the query @var{Query}
  expressed in the low level query language. The exact mapping of the
  query over the document is returned in @var{Match}".

'$xml_query'(Query, Doc, Match) :-
	canonic_xml_item(Doc),
	canonic_xml_query(Query),!,
	unfold_query(Query, Doc, Match).

unfold_query(Query, Doc, env(QueryTag, Atts, Elems)) :- 
	nonvar(Query),
	Query = env(QueryTag, QueryAtts, QueryTerms), !,
	context_node_by_tag(QueryTag, Doc, DocAtts, DocTerms),
	context_node_by_atts(QueryAtts, DocAtts, [], Atts),
	context_node_by_els(QueryTerms, DocTerms, [], Elems).
unfold_query(Tag, [Doc], Tag) :-
	string(Doc), !,
	simple_check(Tag, Doc).
unfold_query(Tag, Doc, Doc) :-
	canonic_xml_item(Doc),
	Tag = Doc.

context_node_by_tag(Tag, Doc, Atts, Terms) :-
	nonvar(Tag),
	member(env(Tag, Atts, Terms), Doc).
context_node_by_tag(Tag, Doc, Atts, []) :-
	nonvar(Tag),
	member(elem(Tag, Atts), Doc).

context_node_by_atts([],_, Atts, Atts) .
context_node_by_atts([Att|As], DocAtts, Atts, Total) :-
	Att = =(Name, Val),
	revert_check(Val, RVal),
	member(=(Name, RVal), DocAtts),
	context_node_by_atts(As, DocAtts, [Att|Atts], Total).

context_node_by_els([],_,Els, Els) .
context_node_by_els([X], DocTerms, Els, [El|Els]) :-
	var(X), !,
	unfold_query(X, DocTerms, El).
context_node_by_els([Term|Ts], DocTerms, Els, Total) :-
	unfold_query(Term, DocTerms, El),
	context_node_by_els(Ts, DocTerms, [El|Els], Total).

simple_check(Tag, V) :-
	var(V), !,
	Tag = V.
simple_check(Tag, N) :-
	number(N), !,
	Tag = N.
simple_check(Tag, C) :-
	number_codes(N, C), !,
	Tag  = N.
simple_check(Tag, Tag).

revert_check(N, C) :-
	number(N),!,
	number_codes(N, C).
revert_check(X, X) .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TRANSFORMATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_query_and_constraint(UserQuery, BasicQuery, Clp) :-
	get_constraints(UserQuery, Clp, Kernel),
	get_query(Kernel, BasicQuery).

get_query(Kernel, BasicQuery) :-
	get_query_on_els(Kernel, OnAtts, EQuery),
	get_query_on_atts(OnAtts, Tag, AQuery),
	BasicQuery = env(Tag, AQuery, EQuery).

get_constraints(UserQuery, Clp, Kernel) :-
	functor(UserQuery, with, 2), !,
	divide(UserQuery, Kernel, Clp).
get_constraints(Kernel, [], Kernel) .

get_query_on_atts(OnAtts, Tag, AQuery) :-
	functor(OnAtts, @, 2), !,
	divide(OnAtts, Tag, AQueryT),
	nonvar(Tag),
	parse_atts(AQueryT, [], AQuery).
get_query_on_atts(Tag, Tag, []):-
	nonvar(Tag).

get_query_on_els(Kernel, OnAtts, OnEls) :-
	functor(Kernel, ::, 2),!,
	(divide(Kernel, OnAtts, EQuery) ->
	 parse_els(EQuery, OnEls)
	;
	 OnAtts = Kernel,
	 OnEls = []).
get_query_on_els(Term, F, [A]) :-
	functor(Term, F, 1),!,
	arg(1, Term, A).
get_query_on_els(Kernel, Kernel, []) .

divide(E, A1, A2) :- arg(1, E, A1), arg(2, E, A2).

parse_atts(AQueryT, [AQueryT], [AQueryT]) :-
	var(AQueryT), !.
parse_atts(AQueryT, Acc, AQuery) :-
	functor(AQueryT,',',2), !,
	arg(1, AQueryT, A),
	process(A, PA),
	arg(2, AQueryT, QRest),
	parse_atts(QRest, [PA|Acc], AQuery).
parse_atts(AQ, Acc, [PA|Acc]) :-
	process(AQ, PA).

process(val(X,Y), X=NY)  :- !,
 	simple_check(NY, Y).
process(A,A).
	
parse_els(EQuery, [EQuery]) :-
	var(EQuery), !.
parse_els(EQuery, [Q]) :-
	functor(EQuery,@,2), !,
	get_query(EQuery, Q).
parse_els(EQuery, Q) :-
	functor(EQuery,F,2), !,
	(F = :: ->
	 get_query(EQuery, OnEls),
	 Q = [OnEls]
	;
	 arg(1, EQuery, E),
	 get_query(E, OnEls),
	 arg(2, EQuery, Es),
	 parse_els(Es, Os),
	 Q = [OnEls|Os]) .
parse_els(EQuery, [Q]) :-
	get_query(EQuery, Q), !.
parse_els(EQuery, OnEls) :-
	simple_check(NEQuery, EQuery),
	OnEls = [NEQuery]. 

:- initialization(reset_id).

reset_id :-
	asserta_fact(id(0)).

:- doc(bug, "Since meta-predicate information for the Clp
objective (see get_query_and_constraint) is missing, this package can
break the module system (JFMC)").
