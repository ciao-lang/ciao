
:- use_package([assertions]).
:- doc(nodoc,assertions).
% does not work:
%:- doc(nodoc,metaprops).
:- doc(hide,callme/2).

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
