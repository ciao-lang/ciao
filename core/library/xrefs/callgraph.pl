:- module(callgraph,
	[ call_graph/2,
	  reachability/4
	],
	[ assertions, regtypes
	]).

:- use_module(library(assertions/c_itf_props), [filename/1]).
:- use_module(library(sets), [ord_subtract/3]).  
:- use_module(library(terms), [atom_concat/2]).  
:- use_module(library(graphs/ugraphs), 
        [ rooted_subgraph/3,
	  ugraph/1,
	  vertices/2,
	  vertices_edges_to_ugraph/3
        ]).
:- use_module(library(xrefs/xrefsread), 
	[ meta_call/3,
	  set_files/1,
	  xrefs_files/1
	]).

%-----------------------------------------------------------------------------
% entry points

:- true pred call_graph(File,Graph) : filename * var => ugraph(Graph)
	# "@var{Graph} is the call-graph of the code in @var{File}.".

call_graph(File,Graph):-
	set_files([File]),
	xrefs_files([(F,Cls)]),
	clause_calls(Cls,F,Edges),
	vertices_edges_to_ugraph([],Edges,Graph).

:- true pred reachability(Graph,Sources,Reached,UnReached)
	: ugraph * list * var * var
        # "@var{Reached} are the vertices in @var{Graph} reachable from
           @var{Sources}, @var{UnReached} are the rest.".

reachability(Graph,Sources,Reached,UnReached):-
	rooted_subgraph(Graph,Sources,SubGraph),
	vertices(SubGraph,Reached),
	vertices(Graph,All),
	ord_subtract(All,Reached,UnReached).

%-----------------------------------------------------------------------------

clause_calls([],_F,[]).
clause_calls([Cl|Cls],F,L):-
	used(Cl,F,L,L0),
	clause_calls(Cls,F,L0).

used((H:-B),F,Subgoals,Tail):- !,
	( number(H)
	-> Subgoals = Tail
	 ; getname(H,Pred),
	   body_calls(B,Pred,F,Subgoals,Tail)
	).

body_calls(X,P,_F,Out,Tail):-
	var(X), !,
	Out = [P-metacall|Tail].
body_calls(_:X,P,F,Xones,Tail):- !,
	body_calls(X,P,F,Xones,Tail).
body_calls(!,_,_F,Out,Tail):- !,
	Out = Tail.
body_calls((X,Y),P,F,Xones,Tail):- !,
	body_calls(X,P,F,Xones,Yones),
	body_calls(Y,P,F,Yones,Tail).
body_calls((X;Y),P,F,Xones,Tail):- !,
	body_calls(X,P,F,Xones,Yones),
	body_calls(Y,P,F,Yones,Tail).
body_calls((X->Y),P,F,Xones,Tail):- !,
	body_calls(X,P,F,Xones,Yones),
	body_calls(Y,P,F,Yones,Tail).
body_calls(X,P,F,Xones,Tail):-
	meta_call(X,F,Y), !,
	( Y == 0
	-> Yones=[P-metacall|Tail]
	 ; body_calls(Y,P,F,Yones,Tail)
	),
	getname(X,Name),
	get_rid_of(P,Name,Xones,Yones).
body_calls(X,P,_F,Xname,Tail):-
	getname(X,Name),
	get_rid_of(P,Name,Xname,Tail).

get_rid_of(P,P,Out,Tail):- !,         % Do not consider recursive calls
	Out = Tail.
get_rid_of(P,X,[P-X|Tail],Tail).

getname(H,Name):-
	functor(H,F,A),
        name(A,N),
        atom_codes(At,N),
	atom_concat([F,'/',At],Name).
