:- module(example1, [main/0], []).
:- use_module(library(xdr_handle)).
:- use_package(pillow).

:- use_module(library(format)).

main :-
       line,
       format("Retrieving XDR document from URL:~n", []),
       format("http://clip.dia.fi.upm.es/~~jgomez/fish2.xdr~n~n", []),
	xdr_tree('http://clip.dia.fi.upm.es/~jgomez/fish2.xdr', XDRTree, _),
	format("XDR document transformed into a Prolog term.~n~n", []),
	format("Transforming XDR tree into equivalent HTML code: ~n", []), 
	xdr2html(XDRTree, HTML),
	output_html(HTML).

line:- 
	nl,
	display(
'***************************************************************************'
), 
	nl.
