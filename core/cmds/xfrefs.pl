
:- module(xfrefs,
        [ xfrefs/1,
          xflinks/1,
          quit/0
        ],
        [ assertions, regtypes
        ]).

:- use_module(library(davinci)).
:- use_module(library(xrefs), [xrefs_lgraph/2, xrefs_ugraph/2, xref/1]).
:- reexport(library(xrefs),[ set_files/1, set_flag/1 ]).

:- doc(title,"Crossed-references between files").
:- doc(subtitle_extra,"@bf{The CIAO System Documentation Series}").
:- doc(subtitle_extra,"@em{Draft printed on:} @today{}").
:- doc(author,"Francisco Bueno").

:- doc(module,"This module allows displaying a graph of
        crossed-references between source code in different files.
        The graph is obtained using
	@tt{library(xrefs)} and is displayed using daVinci (a
        windows-based graphics graph displayer developed by U. of Bremen,
	Germany), via @tt{library(davinci)}.

        See the documentation of these libraries for details.").

:- doc(bug,"@tt{quit/0} does nothing. daVinci has to be exited
        from its own window.").

%-----------------------------------------------------------------------------
% entry points

:- pred xfrefs(Xrefs) : xref
        # "Displays a graph of crossed-references of type @var{Xrefs}
           for the current files.".

xfrefs(Xrefs):- 
        xrefs_lgraph(Xrefs,Graph),
        ( davinci -> true ; true ),
        davinci_lgraph(Graph).

:- pred xflinks(Xrefs)
        # "Like @tt{xfrefs/1} but edges have no labels.".

xflinks(Xrefs):- 
        xrefs_ugraph(Xrefs,Graph),
        ( davinci -> true ; true ),
        davinci_ugraph(Graph).

:- doc(quit/0,"Quits daVinci.").

quit:- davinci_quit.

:- doc(doinclude,set_files/1).
:- doc(doinclude,sourcename/1).
:- doc(sourcename/1,"See @tt{engine(streams_basic)}.").
:- doc(doinclude,set_flag/1).
