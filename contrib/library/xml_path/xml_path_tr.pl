:- module(xml_path_tr, [xml_path_tr/2], []).
:- include(library(xml_path/xml_path_syntax)).

xml_path_tr((xml_query(UserQuery, Doc)), 
 	(get_query_and_constraint(UserQuery, BasicQuery, Clp),
%	 (Clp = [] -> true; clpfd_call(Clp)),
	 (Clp = [] -> true; call(Clp)),
	  '$xml_query'(BasicQuery, Doc,_))).

xml_path_tr((xml_search_match(UserQuery, Doc, Match)), 
 	(get_query_and_constraint(UserQuery, BasicQuery, Clp),
%	 (Clp = [] -> true; clpfd_call(Clp)),
	 (Clp = [] -> true; call(Clp)),
	  '$xml_search_match'(BasicQuery, Doc, Match))).
