:- include(xml_dir).

% This fact stores an index, the URL of the queried xml document and a given 
% query over it, including the constraints

query_store(1, XMLFile, [X,Y,Z], Query) :-
	xml_test_dir(XMLDir),
	atom_concat(XMLDir, 'ej1.xml', XMLFile),
	Query = (product@val(_,"prueba")::(quantity(X), 'time-left'(Y), 
	negotiation::preference::price(Z)) with X * Z .>. Y).

query_store(2, XMLFile, [X,Y,L], Q) :-
	xml_test_dir(XMLDir),
	atom_concat(XMLDir, 'ej1.xml', XMLFile),
	Q = product@val(product_name,_)::(quantity(X), location(L), 'time-left'(Y)).

query_store(3, XMLFile, [X,Y,Z], Q):-
	xml_test_dir(XMLDir),
	atom_concat(XMLDir, 'ej2.xml', XMLFile),
	Q = (product@val(_,"prueba")::(quantity(X), 'time-left'(Y), 
	negotiation::preference::price(Z)) with X * Z .>. Y).

query_store(4, XMLFile, [X,Y], Q) :-
	xml_test_dir(XMLDir),
	atom_concat(XMLDir, 'article1098.xml', XMLFile),
	Q = nitf::(head::docdata::'doc-id'@val('id-string',"020918050")::(Y),
	body::'body.head'::abstract::p(X)).

query_store(5, XMLFile, [X,Y], Q) :-
	xml_test_dir(XMLDir),
	atom_concat(XMLDir, 'article1000.xml', XMLFile),
	Q = nitf::(head::docdata::'doc-id'@val('id-string',Y),
	body::'body.head'::abstract::p(X)).

query_store(6, XMLFile, [X,Y], Q) :-
	xml_test_dir(XMLDir),
	atom_concat(XMLDir, 'nitf.xml', XMLFile), 
	Q = nitf@(val('change.date',"7 July 2000"), val('change.time',"1900"))::
                                                 (head::docdata::'doc-id'@val('id-string',Y),body::'body.head'::abstract::p(X)).
%        Q = nitf@(val('change.date',"4 July 2000"), val('change.time',"1900"))::(head::(title("Amphibien vor dem Aussterben"), docdata::'doc-id'@val('id-string',Y)),body::'body.head'::abstract::p(X)).

query_store(7, XMLFile, [X], Q) :-
	xml_test_dir(XMLDir),
	atom_concat(XMLDir, 'article1000.xml', XMLFile),
	Q = body::'body.head'::abstract::p(X).

query_store(8, XMLFile, [X], Q) :-
	xml_test_dir(XMLDir),
	atom_concat(XMLDir, 'nitf2.xml', XMLFile), 
	Q = abstract::p(X).
