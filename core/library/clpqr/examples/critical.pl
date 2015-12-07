:- module(_,[go1/0, go2/0, go3/0, cpm/2, cpm/3, print_analysis/1], []).
:- use_package(clpq).

% Sample goals: go1/0, go2/0, go3/0

:- set_prolog_flag(multi_arity_warnings, off).
:- set_prolog_flag(single_var_warnings, off).

% *************************************
% CLP(R) Version 1.1 - Example Programs
% *************************************
%
% Cpm critical path routine:
% Network is an input project network of the form
%    [ [node1 , node2, time ] .... ]
%    Graph is the critical path graph produced
%    Latest is the latest possible completion time is specified
% cpm/3 is used if the latest time is specified
% otherwise use cpm/2
%
% Output:
%     Node Es Lc 
%         (Gives the Earliest Start time and Latest Completion
%                 time for the particular node)
%     Node1 Node2 T Ls Ec Tf Ff
%         (Details the times relating to the activity between Node1 & Node2
%          T is the time required for the activity
%          Ls the Latest Start time
%          Ec the Earliest Completion time
%          Tf the Total Float
%          Ff the Free Float)
% Activities on the critical path are marked with an asterix
% The start node and end node are computed automatically and distinguished
%
% Sample output
%
%        Node   Es   Lc
% Node1  Node2  T    Ls   Ec   Tf   Ff
% --------------------------------------------------
% START NODE    n1   0    0
% --------------------------------------------------
% n1    n2      3    2    3    2    0
% n1    n3      2    0    2    0    0 *
% --------------------------------------------------
%       n2      3    5
% --------------------------------------------------
% n2    n4      2    5    5    2    2
% --------------------------------------------------
%       n3      2    2
% --------------------------------------------------
% n3    n4      5    2    7    0    0 *
% --------------------------------------------------
% END NODE      n4   7    7

cpm(Network, Graph, Latest) :-
	build(Network, Graph),
	early_late(Graph, Graph, End, Latest),
	Latest .>=. End,
	analyse(Graph, Graph).
	
cpm(Network, Graph) :-
	build(Network, Graph),
	early_late(Graph, Graph, _End),
	analyse(Graph, Graph).

% build an adjacency graph out of the network
build([[I, J, C]|T], Graph) :-
	buildv_to(ed(I, J, C), Graph),
	buildv_from(ed(I, J, C), Graph),
	build(T, Graph).
build([], Graph) :-
	closegraph(Graph).

buildv_to(ed(I, J, C), [ad(I, Es, Lc, To, From)|T]) :-
	!, addedg_to(J, C, To).
buildv_to(Edge, [H|T]) :-
	buildv_to(Edge, T).

addedg_to(V, C, [ed(V, C, _, _, _, _)|T]) :- !.
addedg_to(V, C, [H|T]) :-
	addedg_to(V, C, T).

buildv_from(ed(I, J, C), [ad(J, Es, Lc, To, From)|T]) :-
	!, addedg_from(I, C, From).
buildv_from(Edge, [H|T]) :-
	buildv_from(Edge, T).

addedg_from(V, C, [ed(V, C, _, _, _, _)|T]) :- !.
addedg_from(V, C, [H|T]) :-
	addedg_from(V, C, T).

closegraph([]) :- !.
closegraph([ad(_, _, _, To, From)|T]) :-
	closelist(To),
	closelist(From),
	closegraph(T).

closelist([]) :- !.
closelist([H|T]) :-
	closelist(T).

% Get early start times and latest completion times
% early/4 is used when a ending time is given
% otherwise early/3 assumes that the early start time
% for the end node is equal to the latest completion time
early_late([], _, _, _).
early_late([ad(I, Es, Lc, To, From)|T], G, End, Latest) :-
	setearly(From, To, G, End, Es),
	setlate(To, G, Latest, Lc),
	early_late(T, G, End, Latest).

early_late([], _, _).
early_late([ad(I, Es, Lc, To, From)|T], G, End) :-
	setearly(From, To, G, End, Es),
	setlate(To, G, End, Lc),
	early_late(T, G, End).

setearly([], _, _, _, 0).
setearly([ed(V, C, _, _, _, _)|T], [], G, Es, Es) :-
	!,
	getnode(V, G, Es1, _),
        Es2 .=. Es1+C,
	setmax(T, G, Es2, Es).
setearly([ed(V, C, _, _, _, _)|T], _, G, End, Es) :-
	getnode(V, G, Es1, _),
        Es2 .=. Es1+C,
	setmax(T, G, Es2, Es).

setmax([], _, Max, Max).
setmax([ed(V, C, _, _, _, _)|T], G, Max0, Max) :-
	getnode(V, G, Es1, _),
        MAX .=. max(Max0,Es1+C),
	setmax(T, G, MAX, Max).

setlate([], _, Last, Last).
setlate([ed(V, C, _, _, _, _)|T], G, Last, Lc) :-
	getnode(V, G, _, Lc1),
        Lc2 .=. Lc1-C,
	setmin(T, G, Lc2, Lc).

setmin([], _, Min, Min).
setmin([ed(V, C, _, _, _, _)|T], G, Min0, Min) :-
	getnode(V, G, _, Lc1),
        MIN .=. min(Min0, Lc1-C),
	setmin(T, G, MIN, Min).

% Search graph for the early & late times for a node
getnode(I, [ad(I, Es, Lc, _, _)|T], Es, Lc).
getnode(I, [H|T], Es, Lc) :-
	getnode(I, T, Es, Lc).

% Compute the other times :
%		Ls - latest start time
%		Ec - earliest completion time
%		Tf - total float time
%		Ff - free float time
analyse([], G).
analyse([ad(I, Es, Lc, To, _)|T], G) :-
	analyse_times(To, Es, Lc, G),
	analyse(T, G).

analyse_times([], _, _, _).
analyse_times([ed(V, C, Ls, Ec, Tf, Ff)|T], Esi, Lci, G) :-
	getnode(V, G, Esj, Lcj), 
	compute(Ls, Ec, Tf, Ff, Esj, Lcj, Esi, Lci, C),
	analyse_times(T, Esi, Lci, G).

% Indirect way of doing the calculation just to speed things up
% can be removed and inserted directly in analyse_times
compute(Ls, Ec, Tf, Ff, Esj, Lcj, Esi, Lci, C) :-
	X .=. Esi+C, 
	Ls .=. Lcj-C, 
	Ec .=. Esi+C, 
	Tf .=. Lcj-X, 
	Ff .=. Esj-X.

% display routines
print_analysis(G) :-
	message('\t\tNode\tEs\tLc'), 
	message('Node1\tNode2\tT\tLs\tEc\tTf\tFf'), 
	print_analysis1(G).
print_analysis1([]).
print_analysis1([H|T]) :- 
	print_node(H), 
	print_analysis1(T).
print_node(ad(I, Es, Lc, [], From)) :-
	!,	
	message('--------------------------------------------------'), 
	message(['END NODE\t',I,'\t',Es,'\t',Lc]).
print_node(ad(I, Es, Lc, To, [])) :-
	!, 
	message('--------------------------------------------------'), 
	message(['START NODE\t',I,'\t',Es,'\t',Lc]), 
	message('--------------------------------------------------'), 
	print_times(To, I).
print_node(ad(I, Es, Lc, To, From)) :-
	message('--------------------------------------------------'), 
	message(['\t\t',I,'\t',Es,'\t',Lc]), 
	message('--------------------------------------------------'), 
	print_times(To, I).

print_times([], _).
print_times([ed(V, C, Ls, Ec, Tf, Ff)|T], I) :-
        critical_mark(Tf,Cr),
	message([I,'\t',V,'\t',C,'\t',Ls,'\t',Ec,'\t',Tf,'\t',Ff,'\t',Cr]), 
	print_times(T, I).

critical_mark(0,' *').
critical_mark(Tf, '') :-
	Tf .>. 0.

go1 :-  
	cpm([
		[n1, n2, 3], [n1, n3, 2], [n3, n4, 5], [n2, n4, 2]], G), 
	print_analysis(G).

% Answer:
%                  Node    Es      Lc
%  Node1   Node2   T       Ls      Ec      Tf      Ff
%  --------------------------------------------------
%  START NODE      n1      0       0
%  --------------------------------------------------
%  n1      n2      3       2       3       2       0
%  n1      n3      2       0       2       0       0 *
%  --------------------------------------------------
%                  n2      3       5
%  --------------------------------------------------
%  n2      n4      2       5       5       2       2
%  --------------------------------------------------
%                  n3      2       2
%  --------------------------------------------------
%  n3      n4      5       2       7       0       0 *
%  --------------------------------------------------
%  END NODE        n4      7       7

go2 :-    
	cpm([	
		[n5, n6, 9], [n5, n7, 5], [n6, n7, 0], [n4, n7, 4], 
		[n1, n2, 2], [n2, n3, 0], [n1, n3, 6], [n1, n4, 3], 
		[n2, n5, 1], [n2, n6, 4], [n3, n5, 2], [n4, n5, 0], [n4, n8, 3], 
		[n6, n8, 4], [n7, n8, 6]], G), 
	print_analysis(G).

% Answer:
%                  Node    Es      Lc
%  Node1   Node2   T       Ls      Ec      Tf      Ff
%  --------------------------------------------------
%                  n5      8       8
%  --------------------------------------------------
%  n5      n6      9       8       17      0       0 *
%  n5      n7      5       12      13      4       4
%  --------------------------------------------------
%                  n6      17      17
%  --------------------------------------------------
%  n6      n7      0       17      17      0       0 *
%  n6      n8      4       19      21      2       2
%  --------------------------------------------------
%                  n7      17      17
%  --------------------------------------------------
%  n7      n8      6       17      23      0       0 *
%  --------------------------------------------------
%                  n4      3       8
%  --------------------------------------------------
%  n4      n7      4       13      7       10      10
%  n4      n5      0       8       3       5       5
%  n4      n8      3       20      6       17      17
%  --------------------------------------------------
%  START NODE      n1      0       0
%  --------------------------------------------------
%  n1      n2      2       4       2       4       0
%  n1      n3      6       0       6       0       0 *
%  n1      n4      3       5       3       5       0
%  --------------------------------------------------
%                  n2      2       6
%  --------------------------------------------------
%  n2      n3      0       6       2       4       4
%  n2      n5      1       7       3       5       5
%  n2      n6      4       13      6       11      11
%  --------------------------------------------------
%                  n3      6       6
%  --------------------------------------------------
%  n3      n5      2       6       8       0       0 *
%  --------------------------------------------------
%  END NODE        n8      23      23

go3 :-
	cpm([
		[n1, n2, 4], [n1, n3, 3], [n1, n4, 4], 
		[n2, n5, 7], [n2, n3, 1], [n2, n7, 8], 
		[n3, n5, 4], [n4, n6, 2], [n5, n6, 1], 
		[n5, n7, 3], [n6, n7, 4]], G), 
	print_analysis(G).

/* Answer:
		Node	Es	Lc
Node1	Node2	T	Ls	Ec	Tf	Ff
--------------------------------------------------
START NODE	n1	0	0
--------------------------------------------------
n1	n2	4	0	4	0	0	 *
n1	n3	3	4	3	4	2	
n1	n4	4	6	4	6	0	
--------------------------------------------------
		n2	4	4
--------------------------------------------------
n2	n5	7	4	11	0	0	 *
n2	n3	1	6	5	2	0	
n2	n7	8	8	12	4	4	
--------------------------------------------------
		n3	5	7
--------------------------------------------------
n3	n5	4	7	9	2	2	
--------------------------------------------------
		n4	4	10
--------------------------------------------------
n4	n6	2	10	6	6	6	
--------------------------------------------------
		n5	11	11
--------------------------------------------------
n5	n6	1	11	12	0	0	 *
n5	n7	3	13	14	2	2	
--------------------------------------------------
END NODE	n7	16	16
--------------------------------------------------
		n6	12	12
--------------------------------------------------
n6	n7	4	12	16	0	0	 *

*/

% ?- style_check(all_reset).
