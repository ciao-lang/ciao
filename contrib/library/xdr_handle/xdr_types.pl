:- module(xdr_types, 
	[
	    xdr/1
	],
	[assertions, isomodes]).

:- use_module(library(xdr_handle/xdr_handle), [xdr_node/1]).

:- true prop xdr(T) + regtype # "@var{T} specifies an XDR document.".

xdr(xdr_tree(Node, Branches)) :-
	xdr_node(Node),
	list(Branches, xdr).
