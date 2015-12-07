:- module(examples, [main/0], []).

:- use_package(xml_path).
:- use_package(pillow).

:- include(library(xml_path/xml_path_syntax)).
:- use_package(fd).

:- use_module(library(prolog_sys), [statistics/2]).
:- include(query_store_new). 

:- use_module(library(format)).

main :-
	tquery,
	tsearch,
	tsearch_match,
	tparsing,
	tindexing.
tquery:-
       line,
       format("Retrieving XML document from URL:~n~+ http://clip.dia.fi.upm.es/~~jgomez/xmldocs/ej1.xml~n~n", []),
	format("Performing the following query on XML doc:~n", []),
	format("~+product@val(_,prueba)::(quantity(X), time-left(Y)~n", []),
	format("~+~+negotiation::preference::price(Z)) with X * Z .>. Y~n~n", []),
	query(1).
tquery .

tsearch :-
	line,
	format("Retrieving the XML element to match query.~n~n", []),
	search(1).
tsearch.

tsearch_match :-
	line,
	format("Retrieving the exact subelement path of the XML doc to match query.~n~n", []),
	search_match(1).
tsearch_match.

tparsing :-
	line,
	format("Retrieving the exact subelement of the XML doc to match query.~n~n", []),
	parsing(1).
tparsing. 

tindexing :-
%	index(1),
	line,
	format("Indexing http://clip.dia.fi.upm.es/~~jgomez/xmldocs/ej1.xml to file.~n~n", []),
	index_to_file(1),
	format("Performing search in indexed .pl document.~n~n", []),
	search_index(1).
tindexing.

query(Id) :- %Id from 1 to 6.
	query_store(Id,_, Vs, Query), 
	statistics(walltime,_),
	fetch_xml(Id, Terms),
	statistics(walltime,[_, _Time]),
	xml_query(Query, Terms),
	statistics(walltime,[_, Time2]),
	message(['Document querying took: ', Time2, ' ms.']),
	message(['Extracted variables: [X,Y,Z] = ', Vs, '.']).


search(Id) :- %Id from 1 to 6.
	query_store(Id,_,Vs, Query), 
	statistics(walltime,_),
	fetch_xml(Id, Terms),
	statistics(walltime,[_, _Time]),
	do_search(Query, Terms, Vs),
	statistics(walltime,[_, Time2]),
	message(['Search performed. Spent time: ', Time2, ' ms.']).

do_search(Query, Terms, _Vs) :-
	xml_search(Query, Terms, Doc),
	display(Doc), nl, nl,
	fail.
do_search(_,_,_).

search_match(Id) :- %Id from 1 to 6.
	query_store(Id,_,Vs, Query), 
	statistics(walltime,_),
	fetch_xml(Id, Terms),
	statistics(walltime,[_, _Time]),
	do_search_match(Query, Terms, Vs),
	statistics(walltime,[_, Time2]),
	message(['Search match performed. Spent time: ', Time2, ' ms.']), nl.

do_search_match(Query, Terms, _Vs) :-
	xml_search_match(Query, Terms, Doc),
	display(Doc), nl, nl,
	fail.
do_search_match(_,_,_).

parsing(Id) :- %Id from 1 to 8.
	query_store(Id,_,Vs, Query), 
	statistics(walltime,_),
	fetch_xml(Id, Terms),
	statistics(walltime,[_, _Time]),
	do_parsing(Query, Terms, Vs),
	statistics(walltime,[_, Time2]),
	message(['Parse performed. Spent time: ', Time2, ' ms.']), nl.

do_parsing(Query, Terms, _Vs) :-
	xml_parse_match(Query, Terms, Doc),
	display(Doc), nl, nl,
	fail.
do_parsing(_,_,_).

index(Id) :-
	statistics(walltime,_),
	fetch_xml(Id, Terms),
	statistics(walltime,[_, _Time]),
	xml_index(Terms),
	statistics(walltime,[_, Time2]),
	message(['Document indexing took', Time2, '.']).

xml_output('./xml_indexed.pl').

index_to_file(Id) :-
	statistics(walltime,_),
	fetch_xml(Id, Terms),
	statistics(walltime,[_, _Time]),
	xml_output(Storage),
	xml_index_to_file(Terms, Storage),
	format("XML document transformed into Prolog program xml_indexed.pl.~n~n", []),
	statistics(walltime,[_, Time2]),
	message(['Document indexing took', Time2, '.']).

search_index_one(Id) :-
	query_store(Id, _, _Vs, Query),
	statistics(walltime,_),
	bk_search_index_one(Query),
	statistics(walltime,[_, Time]),
	message(['Document searching took', Time, '.']).

search_index(Id) :-
	query_store(Id, _, _Vs, Query),
	statistics(walltime,_),
	do_search_index(Id, 1, Query),
	statistics(walltime,[_, Time]),
	message(['Index search performed. Spent time: ', Time,' ms.']).

bk_search_index_one(Query) :-
	xml_index_query(Query, _N, _Doc).

bk_search_index(Query) :-
	xml_index_query(Query, _N, _Doc),
	fail.
bk_search_index(_).

do_search_index_one(_, N, Query) :-
	xml_index_query(Query, N, _Doc).

do_search_index(Id, N, Query) :-
	xml_index_query(Query, N, _Doc),
	N1 is N + 1,
	query_store(Id, _, _Vs, NQuery),
	do_search_index(Id, N1, NQuery).
do_search_index(_,_,_).

index_and_search :-
	index(6), !,
	search_index(6).

fetch_xml(Id, Terms) :-
	query_store(Id, URL,_,_), 
	url_info(URL, I),
	fetch_url(I, [], Response),
	member(content(Content), Response),
	xml2terms(Content, Terms).


line:- 
	nl,
	display(
'***************************************************************************'
), 
	nl.
