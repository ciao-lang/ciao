
:- use_package([assertions,metaprops]).
:- doc(nodoc,assertions).
% does not work:
%:- doc(nodoc,metaprops).
:- doc(hide,callme/2).

:- doc(title,"Classical Prolog modes").
 
:- doc(author,"Manuel Hermenegildo").

:- doc(module,"This file defines a number of very simple
   ``@concept{modes}'' which are frequently useful in programs. These
   correspond to the modes used in classical Prolog texts with some
   simple addtions. Note that some of these modes use the same symbol
   as one of the ISO-modes (see @ref{ISO-Prolog modes}) but with subtly
   different meaning.").

:- use_package(library(basicmodes)).

:- doc('+'/1,"Input value in argument.").
:- doc('-'/1,"No input value in argument.").
:- doc('?'/1,"Unspecified argument.").
:- doc('@'/1,"No output value in argument.").
:- doc(in/1,"Input argument.").
:- doc(out/1,"Output argument.").
:- doc(go/1,"Ground output (input/output argument).").
