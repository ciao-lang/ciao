%   File   : dgraphs.yap
%   Author : Vitor Santos Costa
%   Updated: 2006
%   Purpose: Directed Graph Processing Utilities.

:- module( dgraphs,
	   [
	    dgraph_vertices/2,
	    dgraph_edge/3,
	    dgraph_edges/2,
	    dgraph_add_vertex/3,
	    dgraph_add_vertices/3,
	    dgraph_del_vertex/3,
	    dgraph_del_vertices/3,
	    dgraph_add_edge/4,
	    dgraph_add_edges/3,
	    dgraph_del_edge/4,
	    dgraph_del_edges/3,
	    dgraph_to_ugraph/2,
	    ugraph_to_dgraph/2,
	    dgraph_neighbors/3,
	    dgraph_neighbours/3,
	    dgraph_complement/2,
	    dgraph_transpose/2,
	    dgraph_compose/3,
	    dgraph_transitive_closure/2,
	    dgraph_symmetric_closure/2,
	    dgraph_top_sort/2,
	    dgraph_top_sort/3,
	    dgraph_min_path/5,
	    dgraph_max_path/5,
	    dgraph_min_paths/3,
	    dgraph_isomorphic/4,
	    dgraph_path/3,
	    dgraph_reachable/3
	]).
:- include(library(dialect/yap)).

:- reexport(.(rbtrees),
	[rb_new/1 as dgraph_new]).

:- use_module(.(rbtrees), 
	[rb_empty/1,
	 rb_lookup/3,
	 rb_apply/4,
	 rb_insert/4,
	 rb_visit/2,
	 rb_keys/2,
	 rb_delete/3,
	 rb_map/3,
	 rb_clone/3,
	 ord_list_to_rbtree/2]).

:- use_module(.(ordsets), 
	[ord_insert/3,
	 ord_union/3,
	 ord_subtract/3,
	 ord_del_element/3,
	 ord_memberchk/2]).

:- use_module(.(wdgraphs), 
	[dgraph_to_wdgraph/2,
	 wdgraph_min_path/5,
	 wdgraph_max_path/5,
	 wdgraph_min_paths/3]).

dgraph_add_edge(Vs0,V1,V2,Vs2) :-
	dgraph_new_edge(V1,V2,Vs0,Vs1),
	dgraph_add_vertex(Vs1,V2,Vs2).
	
dgraph_add_edges(V0, Edges, VF) :-
	rb_empty(V0), !,
	sort(Edges,SortedEdges),
	all_vertices_in_edges(SortedEdges,Vertices),
	sort(Vertices,SortedVertices),
	edges2graphl(SortedVertices, SortedEdges, GraphL),
	ord_list_to_rbtree(GraphL, VF).
dgraph_add_edges(G0, Edges, GF) :-
	sort(Edges,SortedEdges),
	all_vertices_in_edges(SortedEdges,Vertices),
	sort(Vertices,SortedVertices),
	dgraph_add_edges(SortedVertices,SortedEdges, G0, GF).

all_vertices_in_edges([],[]).
all_vertices_in_edges([V1-V2|Edges],[V1,V2|Vertices]) :-
	all_vertices_in_edges(Edges,Vertices).	 

edges2graphl([], [], []).
edges2graphl([V|Vertices], [VV-V1|SortedEdges], [V-[V1|Children]|GraphL]) :-
	V == VV, !,
	get_extra_children(SortedEdges,VV,Children,RemEdges),
	edges2graphl(Vertices, RemEdges, GraphL).
edges2graphl([V|Vertices], SortedEdges, [V-[]|GraphL]) :-
	edges2graphl(Vertices, SortedEdges, GraphL).


dgraph_add_edges([],[]) --> [].
dgraph_add_edges([V|Vs],[V0-V1|Es]) --> { V == V0 }, !,
	{ get_extra_children(Es,V,Children,REs) },
	dgraph_update_vertex(V,[V1|Children]),
	dgraph_add_edges(Vs,REs).
dgraph_add_edges([V|Vs],Es) --> !,
	dgraph_update_vertex(V,[]),
	dgraph_add_edges(Vs,Es).

get_extra_children([V-C|Es],VV,[C|Children],REs) :- V == VV, !,
	get_extra_children(Es,VV,Children,REs).
get_extra_children(Es,_,[],Es).

dgraph_update_vertex(V,Children, Vs0, Vs) :-
	rb_apply(Vs0, V, add_edges(Children), Vs), !.
dgraph_update_vertex(V,Children, Vs0, Vs) :-
	rb_insert(Vs0,V,Children,Vs).

add_edges(E0,E1,E) :-
	ord_union(E0,E1,E).

:- export(dgraph_new_edge/4).
dgraph_new_edge(V1,V2,Vs0,Vs) :-
	rb_apply(Vs0, V1, insert_edge(V2), Vs), !.
dgraph_new_edge(V1,V2,Vs0,Vs) :-
	rb_insert(Vs0,V1,[V2],Vs).

insert_edge(V2, Children0, Children) :-
	ord_insert(Children0,V2,Children).

dgraph_add_vertices(G, [], G).
dgraph_add_vertices(G0, [V|Vs], GF) :-
	dgraph_add_vertex(G0, V, G1),
	dgraph_add_vertices(G1, Vs, GF).


dgraph_add_vertex(Vs0, V, Vs0) :-
	rb_lookup(V,_,Vs0), !.
dgraph_add_vertex(Vs0, V, Vs) :-
	rb_insert(Vs0, V, [], Vs).

dgraph_edges(Vs,Edges) :-
	rb_visit(Vs,L0),
	cvt2edges(L0,Edges).

dgraph_vertices(Vs,Vertices) :-
	rb_keys(Vs,Vertices).

cvt2edges([],[]).
cvt2edges([V-Children|L0],Edges) :-
	children2edges(Children,V,Edges,Edges0),
	cvt2edges(L0,Edges0).

children2edges([],_,Edges,Edges).
children2edges([Child|L0],V,[V-Child|EdgesF],Edges0) :-
	children2edges(L0,V,EdgesF,Edges0).

dgraph_neighbours(V,Vertices,Children) :-
	rb_lookup(V,Children,Vertices).
dgraph_neighbors(V,Vertices,Children) :-
	rb_lookup(V,Children,Vertices).

/*
add_vertices(Graph, [], Graph).
add_vertices(Graph, [V|Vertices], NewGraph) :-
	rb_insert(Graph, V, [], IntGraph),
	add_vertices(IntGraph, Vertices, NewGraph).
*/

dgraph_complement(Vs0,VsF) :-
	dgraph_vertices(Vs0,Vertices),
	rb_map(Vs0,complement(Vertices),VsF).

complement(Vs,Children,NewChildren) :-
	ord_subtract(Vs,Children,NewChildren).

dgraph_del_edge(Vs0,V1,V2,Vs1) :-
	rb_apply(Vs0, V1, delete_edge(V2), Vs1).

dgraph_del_edges(G0, Edges, Gf) :-
	sort(Edges,SortedEdges),
	continue_del_edges(SortedEdges, G0, Gf).

continue_del_edges([]) --> [].
continue_del_edges([V-V1|Es]) --> !,
	{ get_extra_children(Es,V,Children,REs) },
	contract_vertex(V,[V1|Children]),
	continue_del_edges(REs).

contract_vertex(V,Children, Vs0, Vs) :-
	rb_apply(Vs0, V, del_edges(Children), Vs).

del_edges(ToRemove,E0,E) :-
	ord_subtract(E0,ToRemove,E).

dgraph_del_vertex(Vs0, V, Vsf) :-
	rb_delete(Vs0, V, Vs1),
	rb_map(Vs1, delete_edge(V), Vsf).

delete_edge(Edges0, V, Edges) :-
	ord_del_element(Edges0, V, Edges).

dgraph_del_vertices(G0, Vs, GF) :-
	sort(Vs,SortedVs),
	delete_all(SortedVs, G0, G1),
	delete_remaining_edges(SortedVs, G1, GF).

% it would be nice to be able to delete a set of elements from an RB tree
% but I don't how to do it yet.
delete_all([]) --> [].
delete_all([V|Vs],Vs0,Vsf) :-
	rb_delete(Vs0, V, Vsi),
	delete_all(Vs,Vsi,Vsf).

delete_remaining_edges(SortedVs,Vs0,Vsf) :-
	rb_map(Vs0, del_edges(SortedVs), Vsf).

dgraph_transpose(Graph, TGraph) :-
	rb_visit(Graph, Edges),
	rb_clone(Graph, TGraph, NewNodes),
	tedges(Edges,UnsortedTEdges),
	sort(UnsortedTEdges,TEdges),
	fill_nodes(NewNodes,TEdges).

tedges([],[]).
tedges([V-Vs|Edges],TEdges) :-
	fill_tedges(Vs, V, TEdges, TEdges0),
	tedges(Edges,TEdges0).

fill_tedges([], _, TEdges, TEdges).
fill_tedges([V1|Vs], V, [V1-V|TEdges], TEdges0) :-
	fill_tedges(Vs, V, TEdges, TEdges0).


fill_nodes([],[]).
fill_nodes([V-[Child|MoreChildren]|Nodes],[V-Child|Edges]) :- !,
	get_extra_children(Edges,V,MoreChildren,REdges),
	fill_nodes(Nodes,REdges).
fill_nodes([_-[]|Edges],TEdges) :-
	fill_nodes(Edges,TEdges).

dgraph_compose(T1,T2,CT) :-
	rb_visit(T1,Nodes),
	compose(Nodes,T2,NewNodes),
	dgraph_new(CT0),
	dgraph_add_edges(CT0,NewNodes,CT).

compose([],_,[]).
compose([V-Children|Nodes],T2,NewNodes) :-
	compose2(Children,V,T2,NewNodes,NewNodes0),
	compose(Nodes,T2,NewNodes0).

compose2([],_,_,NewNodes,NewNodes).
compose2([C|Children],V,T2,NewNodes,NewNodes0) :-
	rb_lookup(C, GrandChildren, T2),
	compose3(GrandChildren, V, NewNodes,NewNodesI),
	compose2(Children,V,T2,NewNodesI,NewNodes0).

compose3([], _, NewNodes, NewNodes).
compose3([GC|GrandChildren], V, [V-GC|NewNodes], NewNodes0) :-
	compose3(GrandChildren, V, NewNodes, NewNodes0).

dgraph_transitive_closure(G,Closure) :-
	dgraph_edges(G,Edges),
	continue_closure(Edges,G,Closure).

continue_closure([], Closure, Closure) :- !.
continue_closure(Edges, G, Closure) :-
	transit_graph(Edges,G,NewEdges),
	dgraph_add_edges(G, NewEdges, GN),
	continue_closure(NewEdges, GN, Closure).

transit_graph([],_,[]).
transit_graph([V-V1|Edges],G,NewEdges) :-
	rb_lookup(V1, GrandChildren, G),
	transit_graph2(GrandChildren, V, G, NewEdges, MoreEdges),
	transit_graph(Edges, G, MoreEdges).

transit_graph2([], _, _, NewEdges, NewEdges).
transit_graph2([GC|GrandChildren], V, G, NewEdges, MoreEdges) :-
	is_edge(V,GC,G), !,
	transit_graph2(GrandChildren, V, G, NewEdges, MoreEdges).
transit_graph2([GC|GrandChildren], V, G, [V-GC|NewEdges], MoreEdges) :-
	transit_graph2(GrandChildren, V, G, NewEdges, MoreEdges).

is_edge(V1,V2,G) :-
	rb_lookup(V1,Children,G),
	ord_memberchk(V2, Children).

dgraph_symmetric_closure(G,S) :-
	dgraph_edges(G, Edges),
	invert_edges(Edges, InvertedEdges),
	dgraph_add_edges(G, InvertedEdges, S).

invert_edges([], []).
invert_edges([V1-V2|Edges], [V2-V1|InvertedEdges]) :-
	invert_edges(Edges, InvertedEdges).

dgraph_top_sort(G, Q) :-
	dgraph_top_sort(G, Q, []).

dgraph_top_sort(G, Q, RQ0) :-
	% O(E)
	rb_visit(G, Vs),
	% O(E)
	invert_and_link(Vs, Links, UnsortedInvertedEdges, AllVs, Q),
	% O(V)
	rb_clone(G, LinkedG, Links),
	% O(Elog(E))
	sort(UnsortedInvertedEdges, InvertedEdges),
	% O(E)
	dgraph_vertices(G, AllVs),
	start_queue(AllVs, InvertedEdges, Q, RQ),
	continue_queue(Q, LinkedG, RQ, RQ0).

invert_and_link([], [], [], [], []).
invert_and_link([V-Vs|Edges], [V-NVs|ExtraEdges], UnsortedInvertedEdges, [V|AllVs],[_|Q]) :-
	inv_links(Vs, NVs, V, UnsortedInvertedEdges, UnsortedInvertedEdges0),
	invert_and_link(Edges, ExtraEdges, UnsortedInvertedEdges0, AllVs, Q).

inv_links([],[],_,UnsortedInvertedEdges,UnsortedInvertedEdges).
inv_links([V2|Vs],[l(V2,A,B,S,E)|VLnks],V1,[V2-e(A,B,S,E)|UnsortedInvertedEdges],UnsortedInvertedEdges0) :-
	inv_links(Vs,VLnks,V1,UnsortedInvertedEdges,UnsortedInvertedEdges0).

start_queue([], [], RQ, RQ).
start_queue([V|AllVs], [VV-e(S,B,S,E)|InvertedEdges], Q, RQ) :- V == VV, !,
	link_edges(InvertedEdges, VV, B, S, E, RemainingEdges),
	start_queue(AllVs, RemainingEdges, Q, RQ).
start_queue([V|AllVs], InvertedEdges, [V|Q], RQ) :-
	start_queue(AllVs, InvertedEdges, Q, RQ).

link_edges([V-e(A,B,S,E)|InvertedEdges], VV, A, S, E, RemEdges) :- V == VV, !,
	link_edges(InvertedEdges, VV, B, S, E, RemEdges).
link_edges(RemEdges, _, A, _, A, RemEdges).

continue_queue([], _, RQ0, RQ0).
continue_queue([V|Q], LinkedG, RQ, RQ0) :-
	rb_lookup(V, Links, LinkedG),
	close_links(Links, RQ, RQI),
	% not clear whether I should deleted V from LinkedG
	continue_queue(Q, LinkedG, RQI, RQ0).

close_links([], RQ, RQ).
close_links([l(V,A,A,S,E)|Links], RQ, RQ0) :-
	( S == E -> RQ = [V| RQ1] ; RQ = RQ1),
	close_links(Links, RQ1, RQ0).


ugraph_to_dgraph(UG, DG) :-
	ord_list_to_rbtree(UG, DG).

dgraph_to_ugraph(DG, UG) :-
	rb_visit(DG, UG).


dgraph_edge(N1, N2, G) :-
	rb_lookup(N1, Ns, G),
	ord_memberchk(N2, Ns).

dgraph_min_path(V1, V2, Graph, Path, Cost) :-
	dgraph_to_wdgraph(Graph, WGraph),
	wdgraph_min_path(V1, V2, WGraph, Path, Cost).

dgraph_max_path(V1, V2, Graph, Path, Cost) :-
	dgraph_to_wdgraph(Graph, WGraph),
	wdgraph_max_path(V1, V2, WGraph, Path, Cost).

dgraph_min_paths(V1, Graph, Paths) :-
	dgraph_to_wdgraph(Graph, WGraph),
	wdgraph_min_paths(V1, WGraph, Paths).

dgraph_path(V, G, [V|P]) :-
	rb_lookup(V, Children, G),
	ord_del_element(Children, V, Ch),
	do_path(Ch, G, [V], P).

do_path([], _, _, []).
do_path([C|Children], G, SoFar, Path) :-
	do_children([C|Children], G, SoFar, Path).

do_children([V|_], G, SoFar, [V|Path]) :-
	rb_lookup(V, Children, G),
	ord_subtract(Children, SoFar, Ch),
	ord_insert(SoFar, V, NextSoFar),
	do_path(Ch, G, NextSoFar, Path).
do_children([_|Children], G, SoFar, Path) :-
	do_children(Children, G, SoFar, Path).


dgraph_isomorphic(Vs, Vs2, G1, G2) :-
	rb_new(Map0),
	mapping(Vs,Vs2,Map0,Map),
	dgraph_edges(G1,Edges),
	translate_edges(Edges,Map,TEdges),
	dgraph_new(G20),
	dgraph_add_vertices(Vs2,G20,G21),
	dgraph_add_edges(G21,TEdges,G2).

mapping([],[],Map,Map).
mapping([V1|Vs],[V2|Vs2],Map0,Map) :-
	rb_insert(Map0,V1,V2,MapI),
	mapping(Vs,Vs2,MapI,Map).


	
translate_edges([],_,[]).
translate_edges([V1-V2|Edges],Map,[NV1-NV2|TEdges]) :-
	rb_lookup(V1,NV1,Map),
	rb_lookup(V2,NV2,Map),
	translate_edges(Edges,Map,TEdges).

dgraph_reachable(V, G, Edges) :-
	rb_lookup(V, Children, G),
	ord_list_to_rbtree([V-[]],Done0),
	reachable(Children, Done0, _, G, Edges, []).

reachable([], Done, Done, _, Edges, Edges).
reachable([V|Vertices], Done0, DoneF, G, EdgesF, Edges0) :-
	rb_lookup(V,_, Done0), !,
	reachable(Vertices, Done0, DoneF, G, EdgesF, Edges0).
reachable([V|Vertices], Done0, DoneF, G, [V|EdgesF], Edges0) :-
	rb_lookup(V, Kids, G),
	rb_insert(Done0, V, [], Done1),
	reachable(Kids, Done1, DoneI, G, EdgesF, EdgesI),
	reachable(Vertices, DoneI, DoneF, G, EdgesI, Edges0).
