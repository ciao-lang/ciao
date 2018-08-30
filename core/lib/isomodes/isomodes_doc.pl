:- use_package([assertions,metaprops]).
:- doc(nodoc,assertions).
:- doc(nodoc,assertions_basic).
:- doc(nodoc,metaprops).
:- doc(hide,callme/2). % TODO: needed since nodoc above does not hide this predicate

:- doc(title,"ISO-Prolog modes").
 
:- doc(author,"Daniel Cabeza").
:- doc(author,"Manuel Hermenegildo").
 
:- doc(module,"This file defines the ``@concept{modes}''
   used in the documentation of the ISO-Prolog standard.
   See also @ref{Classical Prolog modes} for an alternative set of
   modes.").

:- use_package(library(isomodes)).

:- doc('?'/1,"Unspecified argument.").
:- doc('*'/1,"Unspecified argument.").
