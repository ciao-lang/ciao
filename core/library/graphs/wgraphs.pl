:- module(wgraphs, [vertices_edges_to_wgraph/3], [assertions]).

:- use_module(library(sets), [ord_union/3]).
:- use_module(library(sort), [sort/2, keysort/2]).

vertices_edges_to_wgraph(Vertices0, Edges, Graph) :-
	sort(Vertices0, Vertices1),
	keysort(Edges, EdgeSet),
	edges_vertices(EdgeSet, Bag),
	sort(Bag, Vertices2),
	ord_union(Vertices1, Vertices2, VertexSet),
	group_edges(VertexSet, EdgeSet, Graph).

edges_vertices([],                  []).
edges_vertices([From-(To-_)|Edges], [From, To|Vertices]) :-
	edges_vertices(Edges, Vertices).

group_edges([],                _,     []).
group_edges([Vertex|Vertices], Edges, [Vertex-Neibs|G]) :-
	group_edges_(Edges, Vertex, Neibs0, RestEdges),
	keysort(Neibs0, Neibs1),
	group_min_edges(Neibs1, Neibs),
	group_edges(Vertices, RestEdges, G).

group_edges_([V0-X|Edges], V, [X|Neibs], RestEdges) :- V0==V, !,
	group_edges_(Edges, V, Neibs, RestEdges).
group_edges_(Edges, _, [], Edges).

group_min_edges([],         []).
group_min_edges([Key-W|L1], L2) :- group_min_edges_(L1, Key, W, L2).

group_min_edges_([Key1-W1|L1], Key, W, L2) :- Key1==Key, !,
	W2 is min(W1, W),
	group_min_edges_(L1, Key, W2, L2).
group_min_edges_(L1, Key, W, [Key-W|L2]) :-
	group_min_edges(L1, L2).
