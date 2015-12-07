
:- module(xrefs2graph,[ xrefs2graph/2, xrefsnolabels/2 ],[assertions,pure]).

:- use_module(library(sort), [sort/2]).
:- use_module(engine(term_basic), [(=)/2]).
:- use_module(engine(basiccontrol)).

%-----------------------------------------------------------------------------
:- doc(xrefs2graph/2,"Converts a graph of xrefs to a dlgraph in the
	style of @tt{library(graphs)}.").

xrefs2graph(HGraph,MGraph):-
	hgraph2graph(HGraph,Graph,[]),
	sort(Graph,SGraph),
	graph2mgraph(SGraph,MGraph).

hgraph2graph([],Tail,Tail).
hgraph2graph([(Origin,Labels)|More],Graph,Tail):-
	labels2graph(Labels,Origin,Graph,Graph1),
	hgraph2graph(More,Graph1,Tail).

labels2graph([],_,Tail,Tail).
labels2graph([(Label,Targets)|More],Origin,Graph,Tail):-
	targets2graph(Targets,Origin,Label,Graph,Graph1),
	labels2graph(More,Origin,Graph1,Tail).

targets2graph([],Origin,Label,Graph,Tail):- !,
	Graph=[(Origin,0,Label)|Tail].          % 0 means no target!
targets2graph(Targets,Origin,Label,Graph,Tail):-
	targets2graph_(Targets,Origin,Label,Graph,Tail).

targets2graph_([],_,_,Tail,Tail).
targets2graph_([Target|Targets],Origin,Label,
	      [(Origin,Target,Label)|Graph],Tail):-
	targets2graph_(Targets,Origin,Label,Graph,Tail).

graph2mgraph([],[]).
graph2mgraph([(Origin,Target,Label)|Graph],
	     [(Origin,[Label|Labels],Target)|MGraph]):-
	graph2mgraph0(Graph,Origin,Target,Labels,MGraph).

graph2mgraph0([],_,_,[],[]).
graph2mgraph0([(Origin,Target,Label)|Graph],Origin,Target,[Label|Labels],
	      MGraph):-
	graph2mgraph0(Graph,Origin,Target,Labels,MGraph).
graph2mgraph0([(Origin1,Target1,Label)|Graph],Origin,Target,[],
	      [(Origin1,[Label|Labels],Target1)|MGraph]):-
	\+ ( Origin = Origin1, Target = Target1 ),
	graph2mgraph0(Graph,Origin1,Target1,Labels,MGraph).

:- doc(xrefsnolabels/2,"Strips out the labels in a graph of xrefs.").

xrefsnolabels(HGraph,MGraph):-
	hgraph2graph(HGraph,Graph,[]),
	strip_out_labels(Graph,NGraph),
	sort(NGraph,MGraph).

strip_out_labels([(Origin,Target,_Label)|Graph],[(Origin,Target)|NGraph]):-
	strip_out_labels(Graph,NGraph).
strip_out_labels([],[]).
