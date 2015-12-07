:- module(xrefs,
	[ xrefs_lgraph/2,
	  xrefs_ugraph/2
	],
        [ assertions, regtypes
        ]).

:- use_module(library(graphs), [edges_to_lgraph/2, edges_to_ugraph/2]).
:- use_module(library(graphs/ugraphs), [ugraph/1]). % for documenting
:- use_module(library(graphs/lgraphs), [lgraph/2]). % for documenting
:- use_module(library(xrefs/xrefsbuild), [xrefs/2]).
:- reexport(library(xrefs/xrefsbuild),[ set_flag/1, xref/1 ]).
:- reexport(library(xrefs/xrefsread),[ set_files/1 ]).
:- use_module(library(xrefs/xrefs2graph), [xrefs2graph/2, xrefsnolabels/2]).

:- doc(title,"Graphs of crossed-references between files").
:- doc(subtitle_extra,"@bf{The CIAO System Documentation Series}").
:- doc(subtitle_extra,"@em{Draft printed on:} @today{}").
:- doc(author,"Francisco Bueno").
:- doc(module,"This library provides support for obtaining a graph of
        crossed-references between the source code in different files.
        The graph identifies calls from clauses in one file which match
	the name of a predicate defined in another file.

        The graph has directed edges between the file names, the meaning 
        of which depends on the selected type of crossed-references. For
        crossed-references of type @tt{whouses} edges point to files
        that ""use"" predicates defined in the edge source, i.e., edge
        source defines predicates (in edge label) which are used (called)
        by edge sink. For crossed-references of type @tt{whodefs} edges
        point to files that define predicates used by the edge source,
        i.e., edge source uses (calls) predicates which are defined in 
        edge sink. An edge sink @tt{0} indicates that the edge has no real
        sink. 

        The library works as follows. The files which are
        taken into account to construct the graph are set by the
        predicate @tt{set_files/1}. The kind of links between these files
        that will be included as graph edges are set by the predicate
        @tt{set_flag/1}. Then, predicates @tt{xrefs_lgraph/2} and 
        @tt{xrefs_ugraph/2} construct the labeled (@tt{lgraph}) or
        unlabeled (@tt{ugraph}) graph.").

:- doc(bug,"The bodies of initialization/on_abort directives are not
	taken into account.").

%-----------------------------------------------------------------------------
% entry points

:- pred xrefs_lgraph(Xrefs,Graph) : xref * var => xref * lgraph(gnd)
        # "Binds @var{Graph} to a graph of crossed-references of type
           @var{Xrefs} for the current files.".

xrefs_lgraph(Xrefs,LGraph):-
        xrefs(Xrefs,HGraph),
        xrefs2graph(HGraph,Graph),
        edges_to_lgraph(Graph,LGraph).

:- pred xrefs_ugraph(Xrefs,Graph) : xref * var => xref * ugraph
        # "Like @tt{xrefs_lgraph/2} but edges have no labels.".

xrefs_ugraph(Xrefs,LGraph):-
        xrefs(Xrefs,HGraph),
        xrefsnolabels(HGraph,Graph),
        edges_to_ugraph(Graph,LGraph).
