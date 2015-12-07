
:- module(mrefs,
        [ mrefs_lgraph/1,
          mrefs_ugraph/1
        ],
        [ assertions, regtypes
        ]).

:- use_module(library(graphs), [edges_to_lgraph/2, edges_to_ugraph/2]).
:- use_module(library(graphs/ugraphs), [ugraph/1]). % for documenting
:- use_module(library(graphs/lgraphs), [lgraph/2]). % for documenting
:- use_module(library(xrefs/xrefsread), [xrefs_modules/2]).
:- reexport(library(xrefs/xrefsread),[ set_files/1, set_flag/1 ]).
:- use_module(library(xrefs/xrefs2graph), [xrefs2graph/2]).

:- doc(title,"Graphs of crossed-references between modules").
:- doc(subtitle_extra,"@bf{The CIAO System Documentation Series}").
:- doc(subtitle_extra,"@em{Draft printed on:} @today{}").
:- doc(author,"Francisco Bueno").
:- doc(module,"This library provides support for obtaining a graph of
        crossed-references between modules. The graph has directed edges
        which go in the direction of the imports between the modules:
        edge source imports predicates (in edge label) from edge sink.

        The library works as follows. The files (modules) which are
        taken into account to construct the graph are set by the
        predicate @tt{set_files/1}. The kind of links between these files
        that will be included as graph edges are set by the predicate
        @tt{set_flag/1}. Then, predicates @tt{mrefs_lgraph/1} and 
        @tt{mrefs_ugraph/1} construct the labeled (@tt{lgraph}) or
        unlabeled (@tt{ugraph}) graph.").

:- doc(bug,"The modules @tt{multifile} and @tt{user} are not
	taken into account.").

%-----------------------------------------------------------------------------
% entry points

:- pred mrefs_lgraph(Graph) : var => lgraph(gnd)
        # "Binds @var{Graph} to a graph 
          of crossed-references for the current files.".

mrefs_lgraph(LGraph):-
        xrefs_modules(labels,HGraph),
        xrefs2graph(HGraph,Graph),
        edges_to_lgraph(Graph,LGraph).

:- pred mrefs_ugraph/1 : var => ugraph
        # "Like @tt{mrefs_lgraph/1} but edges have no labels.".

mrefs_ugraph(UGraph):-
	xrefs_modules(nolabels,Graph),
	edges_to_ugraph(Graph,UGraph).
