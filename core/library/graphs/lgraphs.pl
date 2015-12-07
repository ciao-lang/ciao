:- module(lgraphs,
        [ lgraph/2,
	  vertices_edges_to_lgraph/3
        ],
        [assertions,isomodes,regtypes] ).

:- doc(title,"Labeled graph-processing utilities").
:- doc(author,"Francisco Bueno").
:- doc(module,"See the comments for the @lib{ugraphs} library.").

:- use_module(library(sort)).
:- use_module(library(sets), [ord_union/3]).

:- regtype lgraph(Graph,Type)
	# "@var{Graph} is a labeled graph of @var{Type} terms.".

lgraph(_,_).

:- doc(vertices_edges_to_lgraph(Vertices0, Edges, Graph), "This
   one is a copy of the same procedure in library(wgraphs) except for
   the definition of min/3 (ah! - the polimorphism!).

   It would only be needed if there are multi-edges, i.e., several
   edges between the same two vertices.").

vertices_edges_to_lgraph(Vertices0, Edges, Graph) :-
	sort(Vertices0, Vertices1),
	keysort(Edges, EdgeSet),
	edges_vertices(EdgeSet, Bag),
	sort(Bag, Vertices2),
	ord_union(Vertices1, Vertices2, VertexSet),
	group_edges(VertexSet, EdgeSet, Graph).

edges_vertices([], []).
edges_vertices([From-(To-_)|Edges], [From,To|Vertices]) :-
	edges_vertices(Edges, Vertices).

group_edges([], _, []).
group_edges([Vertex|Vertices], Edges, [Vertex-Neibs|G]) :-
	group_edges_(Edges, Vertex, Neibs0, RestEdges),
	keysort(Neibs0, Neibs1),
	group_min_edges(Neibs1, Neibs),	
	group_edges(Vertices, RestEdges, G).

group_edges_([V0-X|Edges], V, [X|Neibs], RestEdges) :- V0==V, !,
	group_edges_(Edges, V, Neibs, RestEdges).
group_edges_(Edges, _, [], Edges).

group_min_edges([], []).
group_min_edges([Key-W|L1], L2) :- group_min_edges_(L1, Key, W, L2).

group_min_edges_([Key1-W1|L1], Key, W, L2) :- Key1==Key, !,
	min(W1,W,W2),
	group_min_edges_(L1, Key, W2, L2).
group_min_edges_(L1, Key, W, [Key-W|L2]) :-
	group_min_edges(L1, L2).

min(W1,W,W2):- W1@=<W, !, W2=W1.
min(_W1,W,W).
