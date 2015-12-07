:- module(ugraphs, 
        [
            vertices_edges_to_ugraph/3,
            neighbors/3,
            edges/2,
            del_edges/3,
            add_edges/3,
            vertices/2,
            del_vertices/3,
            add_vertices/3,
            transpose/2,
	    rooted_subgraph/3,
            point_to/3,
	    ugraph/1
        ],
        [assertions,isomodes,regtypes] ).


:- doc(title,"Unweighted graph-processing utilities").

:- doc(author,"Richard A. O'Keefe (original shared code)").
:- doc(author,"Mats Carlsson (adapted from original code)").
:- doc(author,"Francisco Bueno (modifications)").
:- doc(author,"Manuel Carro (modifications)").

:- doc(module,"An unweighted directed graph (ugraph) is
   represented as a list of (vertex-neighbors) pairs, where the pairs
   are in standard order (as produced by keysort with unique keys) and
   the neighbors of each vertex are also in standard order (as
   produced by sort), and every neighbor appears as a vertex even if
   it has no neighbors itself.

   An undirected graph is represented as a directed graph where for
   each edge @tt{(U,V)} there is a symmetric edge @tt{(V,U)}.

   An edge @tt{(U,V)} is represented as the term @tt{U-V}.

   A vertex can be any term.  Two vertices are distinct iff they are
   not identical (@pred{==/2}).

   A path is represented as a list of vertices.  No vertex can appear
   twice in a path.

").

:- use_module(library(sets), [
        ord_union/3, 
        ord_subtract/3,
        ord_member/2] ).
:- use_module(library(sort), [sort/2]).

:- regtype ugraph(Graph)
	# "@var{Graph} is an ugraph.".

ugraph(_).

:- true pred point_to(+Vertex, +Graph, -Point_to) 

# "Is true if @var{Point_to} is the list of nodes which go directly
   to @var{Vertex} in @var{Graph}.".

point_to(Vertex, Graph, Point_to):-
        point_to_(Graph, Vertex, Point_to).

point_to_([], _Vertex, []).
point_to_([Node-Neibs|Graph], Vertex, PointedTo):-
        ord_member(Vertex, Neibs), !,
        PointedTo = [Node|RestPointed],
        point_to_(Graph, Vertex, RestPointed).
point_to_([_|Graph], Vertex, PointedTo):-
        point_to_(Graph, Vertex, PointedTo).

:- true pred transpose(+Graph, -Transpose) 

# "Is true if @var{Transpose} is the graph computed by replacing each
   edge @tt{(u,v)} in @var{Graph} by its symmetric edge @tt{(v,u)}.
   It can only be used one way around.  The cost is O(N^2).".

transpose(Graph, Transpose) :-
	transpose_(Graph, Base, Base, Transpose).

transpose_([], [], Base, Base).
transpose_([Vertex-Neibs|Graph], [Vertex-[]|RestBase], Base, Transpose) :-
	transpose_(Graph, RestBase, Base, SoFar),
	transpose1(SoFar, Neibs, Vertex, Transpose).

transpose1([], [], _, []).
transpose1([Neib0-Trans|SoFar], [Neib|Neibs], Vertex,
		[Neib-[Vertex|Trans]|Transpose]) :- Neib0==Neib, !,
	transpose1(SoFar, Neibs, Vertex, Transpose).
transpose1([Head|SoFar], Neibs, Vertex, [Head|Transpose]) :-
	transpose1(SoFar, Neibs, Vertex, Transpose).

vertices_edges_to_ugraph(Vertices0, Edges, Graph) :-
	sort(Vertices0, Vertices1),
	sort(Edges, EdgeSet),
	edges_vertices(EdgeSet, Bag),
	sort(Bag, Vertices2),
	ord_union(Vertices1, Vertices2, VertexSet),
	group_edges(VertexSet, EdgeSet, Graph).

edges_vertices([], []).
edges_vertices([From-To|Edges], [From,To|Vertices]) :-
	edges_vertices(Edges, Vertices).

group_edges([], _, []).
group_edges([Vertex|Vertices], Edges, [Vertex-Neibs|G]) :-
	group_edges_(Edges, Vertex, Neibs, RestEdges),
	group_edges(Vertices, RestEdges, G).

group_edges_([V0-X|Edges], V, [X|Neibs], RestEdges) :- V0==V, !,
	group_edges_(Edges, V, Neibs, RestEdges).
group_edges_(Edges, _, [], Edges).


:- true pred neighbors(+Vertex, +Graph, -Neighbors)

# "Is true if @var{Vertex} is a vertex in @var{Graph} and
  @var{Neighbors} are its neighbors.".

neighbors(V, [V0-Neighbors|_], Neighbors) :- V0==V, !.
neighbors(V, [_|Graph], Neighbors) :- neighbors(V, Graph, Neighbors).


:- true pred add_vertices(+Graph1, +Vertices, -Graph2)

#  "Is true if @var{Graph2} is @var{Graph1} with @var{Vertices} added to it.".

add_vertices(Graph0, Vs0, Graph) :-
	sort(Vs0, Vs),
	vertex_units(Vs, Graph1),
	graph_union(Graph0, Graph1, Graph).



:- true pred del_vertices(+Graph1, +Vertices, -Graph2)

# "Is true if @var{Graph2} is @var{Graph1} with @var{Vertices} and all
   edges to and from @var{Vertices} removed from it.".

del_vertices(Graph0, Vs0, Graph) :-
	sort(Vs0, Vs),
	graph_del_vertices(Graph0, Vs, Vs, Graph).



:- true pred add_edges(+Graph1, +Edges, -Graph2) 

# "Is true if @var{Graph2} is @var{Graph1} with @var{Edges} and their
  'to' and 'from' vertices added to it.".

add_edges(Graph0, Edges0, Graph) :-
	sort(Edges0, EdgeSet),
	edges_vertices(EdgeSet, Vs0),
	sort(Vs0, Vs),
	group_edges(Vs, EdgeSet, Graph1),
	graph_union(Graph0, Graph1, Graph).


:- true pred del_edges(+Graph1, +Edges, -Graph2)

# "Is true if @var{Graph2} is @var{Graph1} with @var{Edges} removed from it.".

del_edges(Graph0, Edges0, Graph) :-
	sort(Edges0, EdgeSet),
	edges_vertices(EdgeSet, Vs0),
	sort(Vs0, Vs),
	group_edges(Vs, EdgeSet, Graph1),
	graph_difference(Graph0, Graph1, Graph).

vertex_units([], []).
vertex_units([V|Vs], [V-[]|Us]) :- vertex_units(Vs, Us).


:- push_prolog_flag(multi_arity_warnings, off).

graph_union(G0, [], G) :- !, G = G0.
graph_union([], G, G).
graph_union([V1-N1|G1], [V2-N2|G2], G) :-
	compare(C, V1, V2),
	graph_union(C, V1, N1, G1, V2, N2, G2, G).

graph_union(<, V1, N1, G1, V2, N2, G2, [V1-N1|G]) :-
	graph_union(G1, [V2-N2|G2], G).
graph_union(=, V, N1, G1, _, N2, G2, [V-N|G]) :-
	ord_union(N1, N2, N),
	graph_union(G1, G2, G).
graph_union(>, V1, N1, G1, V2, N2, G2, [V2-N2|G]) :-
	graph_union([V1-N1|G1], G2, G).


graph_difference(G0, [], G) :- !, G = G0.
graph_difference([], _, []).
graph_difference([V1-N1|G1], [V2-N2|G2], G) :-
	compare(C, V1, V2),
	graph_difference(C, V1, N1, G1, V2, N2, G2, G).

graph_difference(<, V1, N1, G1, V2, N2, G2, [V1-N1|G]) :-
	graph_difference(G1, [V2-N2|G2], G).
graph_difference(=, V, N1, G1, _, N2, G2, [V-N|G]) :-
	ord_subtract(N1, N2, N),
	graph_difference(G1, G2, G).
graph_difference(>, V1, N1, G1, _, _, G2, G) :-
	graph_difference([V1-N1|G1], G2, G).

graph_del_vertices(G1, [], Set, G) :- !,
	graph_del_vertices(G1, Set, G).
graph_del_vertices([], _, _, []).
graph_del_vertices([V1-N1|G1], [V2|Vs], Set, G) :-
	compare(C, V1, V2),
	graph_del_vertices(C, V1, N1, G1, V2, Vs, Set, G).

graph_del_vertices(<, V1, N1, G1, V2, Vs, Set, [V1-N|G]) :-
	ord_subtract(N1, Set, N),
	graph_del_vertices(G1, [V2|Vs], Set, G).
graph_del_vertices(=, _, _, G1, _, Vs, Set, G) :-
	graph_del_vertices(G1, Vs, Set, G).
graph_del_vertices(>, V1, N1, G1, _, Vs, Set, G) :-
	graph_del_vertices([V1-N1|G1], Vs, Set, G).

graph_del_vertices([], _, []).
graph_del_vertices([V1-N1|G1], Set, [V1-N|G]) :-
	ord_subtract(N1, Set, N),
	graph_del_vertices(G1, Set, G).

:- pop_prolog_flag(multi_arity_warnings).


:- true pred rooted_subgraph(Graph, Sources, SubGraph)
	: ugraph * list * var => ugraph(SubGraph)
        # "@var{SubGraph} is the subgraph of @var{Graph} which is reachable 
           from @var{Sources}.".

rooted_subgraph(Graph,Sources,SubGraph):-
	rooted_subgraph_(Sources,Graph,[],SubGraph).

rooted_subgraph_([],_Graph,SubGraph,SubGraph).
rooted_subgraph_([V|Vs],Graph,SubGraph0,SubGraph):-
	lookup(SubGraph0,V,Ss,SubGraph1),
	( SubGraph0 == SubGraph1 % visited
	-> SubGraph2 = SubGraph0
	 ; lookup(Graph,V,Ss,Graph1),
	   ( Graph == Graph1 % was there
	   -> rooted_subgraph_(Ss,Graph,SubGraph1,SubGraph2)
	    ; SubGraph2 = SubGraph0
	   )
	),
	rooted_subgraph_(Vs,Graph,SubGraph2,SubGraph).

lookup([], Element, Value, [Element-Value]).
lookup([Head-Val|Tail], Element, Value, Set) :-
	compare(Order, Head, Element),
	lookup_(Order, Head, Val, Tail, Element, Value, Set).

lookup_(<, Head, Val, Tail, Element, Value, [Head-Val|Set]) :-
	lookup(Tail, Element, Value, Set).
lookup_(=, Head, Value, Tail, _, Value, [Head-Value|Tail]).
lookup_(>, Head, Val, Tail, Element, Value, [Element-Value,Head-Val|Tail]).


:- true pred edges(+Graph, -Edges) 

# "Unifies @var{Edges} with the edges in @var{Graph}.".

edges([], []).
edges([Vertex-Neibs|G], Edges) :-
	edges_(Neibs, Vertex, Edges, MoreEdges),
	edges(G, MoreEdges).

edges_([], _, Edges, Edges).
edges_([Neib|Neibs], Vertex, [Vertex-Neib|Edges], MoreEdges) :-
	edges_(Neibs, Vertex, Edges, MoreEdges).

:- true pred vertices(+Graph, -Vertices) 

# "Unifies @var{Vertices} with the vertices in @var{Graph}.".

vertices([], []).
vertices([Vertex-_|Graph], [Vertex|Vertices]) :- vertices(Graph, Vertices).
