
:- module(xmrefs,
        [ xmrefs/0,
          xmlinks/0,
          quit/0
        ],
        [ assertions, regtypes
        ]).

:- use_module(library(davinci)).
:- use_module(library(xrefs/mrefs), [mrefs_lgraph/1, mrefs_ugraph/1]).
:- reexport(library(xrefs/mrefs),[ set_files/1, set_flag/1 ]).

:- doc(title,"Crossed-references between modules").
:- doc(subtitle_extra,"@bf{The CIAO System Documentation Series}").
:- doc(subtitle_extra,"@em{Draft printed on:} @today{}").
:- doc(author,"Francisco Bueno").

:- doc(module,"This module allows displaying a graph of
        crossed-references between modules. The graph is obtained using
	@tt{library(xrefs/mrefs)} and is displayed using daVinci (a
        windows-based graphics graph displayer developed by U. of Bremen,
	Germany), via @tt{library(davinci)}.

        See the documentation of these libraries for details.").

:- doc(bug,"@tt{quit/0} does nothing. daVinci has to be exited
        from its own window.").

%-----------------------------------------------------------------------------
% entry points

:- pred xmrefs 
        # "Displays a graph of crossed-references for the current files.".

xmrefs:- 
        mrefs_lgraph(Graph),
        ( davinci -> true ; true ),
        davinci_lgraph(Graph).

:- pred xmlinks
        # "Like @tt{xmrefs/0} but edges have no labels.".

xmlinks:- 
        mrefs_ugraph(Graph),
        ( davinci -> true ; true ),
        davinci_ugraph(Graph).

:- doc(quit/0,"Quits daVinci.").

quit:- davinci_quit.

:- doc(doinclude,set_files/1).
:- doc(doinclude,sourcename/1).
:- doc(sourcename/1,"See @tt{engine(streams_basic)}.").
:- doc(doinclude,set_flag/1).
