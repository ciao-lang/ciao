:- module(graph_to_tex, [
		graph_to_tex/3,
% types:
		graph/1, node/1, edge/1, label/1],
	    [assertions, regtypes]).

:- use_module(library(hiordlib)).

:- doc(author, "Teresa Trigo").
:- doc(author, "Edison Mera").

:- doc(module, "
	We represent our call-graph in the following way
	  G = (N,E) -- A graph is a set of nodes and edges
	   Each node is a pair (Name, Time)
	    Name: the name of the node
	    Time: the cumulated time in the cost center
	   Each edge is a tuple (Origin, Destiny, Time)
	    Origin  : the name of the node origin
	    Destiny : the name of the node origin
	    Time    : the cumulated time in the cost center Destiny 
            when it is called from the cost center Origin").

%% Convert a prolog graph into its latex code

:- regtype graph/1 # "Represents a graph.".
graph(g(Nodes, Edges)) :-
	list(Nodes, node),
	list(Edges, edge).

:- regtype node/1 # "A node of a graph.".
node(n(A, T)) :-
	label(A),
	num(T).

:- regtype edge/1 # "An edge of a graph.".
edge(e(A, B, T)) :-
	label(A),
	label(B),
	num(T).

:- regtype label/1 # "A label of a node.".
label(A) :- atm(A).

:- pred graph_to_tex(NodeField, EdgeFields, Graph) :: atm * list(atm) *
	graph #
"Converts a graph defined in prolog to latex (in dot format) and writes
        a table in which the size of each node is explicit. The method is 
        parametric with respect to the metric of the size of the nodes".

graph_to_tex(_, _, g([], _)) :- !.
graph_to_tex(_, _, g(_, [])) :- !.
graph_to_tex(NodeField, EdgeFields, g(Nodes, Edges)) :-
	message('\\begin{dot2tex}[dot, straightedges, options=-tmath]'),
	message('digraph G {'),
	message('ranksep="0.0";'),
	message('mindist="0";'),
	message('node [shape=circle];'),
	transform(Nodes, NodesTrans),
	rename_nodes(NodesTrans, NodesRen, Equiv, 1),
	rename_edges(Equiv, Edges, EdgesRen),
	nodes_to_tex(NodesRen),
	edges_to_tex(EdgesRen, EdgeFields),
	message('}'),
	message('\\end{dot2tex}'),
	message('\\\\'),
	message('\\\\'),
	print_equivalences(NodeField, Equiv, Nodes).

:- pred print_equivalences(Field, Equiv, Nodes) #
"Prints a list of (cost_center,node_in_the_graph,size_of_node) in a 
        latex table.".
print_equivalences(Field, Equiv, Nodes) :-
	message('\\begin{tabular}{|c|c|c|}'),
	message('\\hline'),
	print_header(Field),
	message('\\hline'),
	print_equivalences_(Equiv, Nodes),
	message('\\end{tabular}').

print_equivalences_([],              []).
print_equivalences_([(CC, N)|Equiv], [n(CC, Size)|Nodes]) :-
	message(['cc', N, '&', ''(CC), '&', Size, '\\\\']),
	message('\\hline'),
	print_equivalences_(Equiv, Nodes).

print_header(counts) :-
	message('Node & Cost Center Name & Cost Center Size (Counts)\\\\').
print_header((counts, per)) :-
	message('Node & Cost Center Name & Cost Center Size (Counts \\%)\\\\').
print_header(ticks) :-
	message('Node & Cost Center Name & Cost Center Size (Ticks)\\\\').
print_header((ticks,per)) :-
	message('Node & Cost Center Name & Cost Center Size (Ticks \\%)\\\\').
print_header(time(_)) :-
	message(
	    'Node & Cost Center Name & Cost Center Size (Execution Time)\\\\').
print_header((time(_),per)) :-
	message(
	    'Node & Cost Center Name & Cost Center Size (Execution Time \\%)\\\\').
print_header(Values) :-
	message(
	    ['Node & Cost Center Name & Cost Center Size (', Values, ')\\\\']).
print_header((Values,per)) :-
	message(
	    ['Node & Cost Center Name & Cost Center Size (', Values, '\\%)\\\\']).


nodes_to_tex([]).
nodes_to_tex([n(N, T)|Nodes]) :-
	T0 is T/10,
	color(T, Color),
	message(['cc', N, '[margin="', T0, '", style="fill= ', Color, '!20"];']
	),
	nodes_to_tex(Nodes).

%% Color the nodes of the graph
%  Ranges can be modified if necessary
color(T, Color) :-
	T < 1.25,
	!,
	Color = green.
color(T, Color) :-
	T < 1.5,
	!,
	Color = yellow.
color(T, Color) :-
	T < 1.75,
	!,
	Color = orange.
color(_, red).

edges_to_tex([], _).
% Special cases of formatting:
edges_to_tex([e(O, D, Values)|Edges], EdgeFields) :-
	(
	    EdgeFields =
	    [call_exits_t, call_fails_t, redo_exits_t, redo_fails_t] ->
	    Values = [C_E, C_F, R_E, R_F],
	    (
		O = D ->
		message(['cc', O, '->', 'cc', O, ' [label="', C_E, ' ', C_F,
			',', R_E, R_F, '", topath="loop right"];'])
	    ;
		message(['cc', O, '->', 'cc', D, ' [label="', C_E, ' ', C_F,
			',', R_E, ' ', R_F, '"];'])
	    )
	;
	    (
		O = D ->
		message(['cc', O, '->', 'cc', O, ' [label="', Values,
			'", topath="loop right"];'])
	    ;
		message(['cc', O, '->', 'cc', D, ' [label="', Values, '"];'])
	    )
	),
	edges_to_tex(Edges, EdgeFields).

%% Transform a list of nodes (each time between 1 and 2)
transform(Nodes, NodesNorm) :-
	minimum(Nodes, min_node, n(_, Min)),
	minimum(Nodes, max_node, n(_, Max)),
	map(Nodes, transform_node(Min, Max), NodesNorm).

min_node(n(_, X), n(_, Y)) :- X < Y.
max_node(n(_, X), n(_, Y)) :- X > Y.

transform_node(n(N, T), Min, Max, n(N, T1)) :-
	T1 is 1 + (T - Min)/(Max - Min).

% We have seen that if the names of the nodes are not uniform (in the sense of 
% having the same length) we can not guarantee that the size of the nodes 
% reflects the cumulated time in the cost center because if the node label 
% is bigger than the node size value dot ignores our specification and adapts 
% automatically the size of the node. The following predicates rename nodes 
% (both in nodes and edges) to 1,2,3,...

%% Renames a list of nodes to 1, 2, 3, ...

rename_nodes([],              [],                  _,               _).
rename_nodes([n(N, T)|Nodes], [n(N1, T)|NodesRen], [(N, N1)|Equiv], N1) :-
	N2 is N1 + 1,
	rename_nodes(Nodes, NodesRen, Equiv, N2).

%%And the nodes in the edges

rename_edges([],              Edges, Edges).
rename_edges([(N, N1)|Equiv], Edges, EdgesRen) :-
	map(Edges, renamenode(N, N1), EdgesRen1),
	rename_edges(Equiv, EdgesRen1, EdgesRen).

renamenode(e(N, N, T), N, N1, e(N1, N1, T)) :- !.
renamenode(e(N, D, T), N, N1, e(N1, D,  T)) :- !.
renamenode(e(O, N, T), N, N1, e(O,  N1, T)) :- !.
renamenode(e(O, D, T), _, _,  e(O,  D,  T)).


%% Tests
%graph_to_tex(g([n(p1,1),n(p2,2),n(q,5)],[e(p1,q,2),e(p2,q,5)])).
%rename([n(p1,1),n(p2,2),n(q,5)],X,[e(p1,q,_),e(p2,q,_)],Y,Z).
