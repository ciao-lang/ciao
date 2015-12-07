:- module(pxrefs,
	[ pxrefs/1
	],
        [ assertions, regtypes
        ]).

:- use_module(library(streams), [open_output/2, close_output/1]).
:- use_module(library(terms), [atom_concat/2]).  
:- use_module(library(xrefs/xrefsbuild), [xrefs/2, xref/1]).
:- reexport(library(xrefs/xrefsbuild),[ set_flag/1 ]).
:- reexport(library(xrefs/xrefsread),[ set_files/1 ]).
:- use_module(library(write), [write/1]).  

:- doc(title,"Lists of crossed-references between files").
:- doc(subtitle_extra,"@bf{The CIAO System Documentation Series}").
:- doc(subtitle_extra,"@em{Draft printed on:} @today{}").
:- doc(author, "Francisco Bueno").
:- doc(module,
	"This library provides support for identifying crossed-references
	between the source code in different files, i.e., calls from clauses
        in one file which match the name of a predicate defined in another
	file. This can be helpful in identifying which file(s) define(s) a
	predicate called in other files, or the other way around, which files
	call a predicate defined in other file(s).

        Crossed-references can be of two types. For type @tt{whouses} a
        list of xrefs is output that shows, for each file @tt{F}, the names
        of the predicates defined in @tt{F}, and for each of these, 
        the names of all other files which call the predicate. I.e.:
        @begin{verbatim}
        file
           predicate-defined-in-this-file
               file-using-this-predicate ...
           ...
        ...
        @end{verbatim}

        For type @tt{whodefs}, for each file @tt{F} and each predicate called
        in @tt{F}, the files that define the predicate are listed. I.e.:
        @begin{verbatim}
        file
           predicate-used-in-this-file
               file-defining-this-predicate ...
           ...
        ...
        @end{verbatim}

        The lists of xrefs are output to files
        @tt{xrefs.whodefs} and @tt{xrefs.whouses}, respectively.

        The files which are taken into account and the classes of predicates
        which are considered can be selected with flags. See below.").

%-----------------------------------------------------------------------------
% entry points

:- pred pxrefs(Xrefs) : xref
        # "Outputs a list of crossed-references of type
           @var{Xrefs} for the current files.".

pxrefs(Xrefs):-
	xrefs(Xrefs,HGraph),
	outxrefs(Xrefs,HGraph).

%-----------------------------------------------------------------------------
% output to file

outxrefs(Xref,HGraph):- outxrefs_(Xref,xrefs,HGraph).

% outgraph(Xref,Graph):- outxrefs_(Xref,xgraph,Graph).

outxrefs_(Xref,Prefix,HGraph):-
	atom_concat([Prefix,'.',Xref],File),
	open_output(File,Streams),
	outxrefs0(HGraph),
	close_output(Streams).

outxrefs0([]).
outxrefs0([(F,Vertices)|More]):-
	write(F), nl,
	outxvertices(Vertices),
	outxrefs0(More).

outxvertices([]).
outxvertices([(Label,Targets)|More]):-
	tab(5),
	write(Label), nl,
	tab(9),
	outxtargets(Targets), nl,
	outxvertices(More).

outxtargets([]).
outxtargets([U|Us]):-
	tab(1),
	write(U),
	outxtargets(Us).
