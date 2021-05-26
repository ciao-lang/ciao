:- use_package([assertions]).
:- doc(nodoc,assertions).
:- doc(nodoc,assertions_basic).

:- doc(title, "Web programming libraries (PiLLoW)").
:- doc(subtitle_extra,"REFERENCE MANUAL").
:- doc(subtitle_extra,"@em{Generated/Printed on:} @today{}").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Manuel Hermenegildo").
:- doc(author, "The Ciao Development Team").

:- include(core_docsrc(common/'ClipAddress')).

:- doc(copyright,"Copyright @copyright{} Daniel Cabeza and Manuel Hermenegildo

@include{FreeDocLicense.lpdoc}

").

:- doc(module,"@cindex{WWW, interfacing with}
   @cindex{XML}@cindex{HTML}@cindex{JSON}
   This package implements a collection of libraries for HTML/XML/JSON
   processing.

   See @lib{http_doc} libraries for the @cindex{HTTP} protocol and
   @cindex{CGI} programming.

   @section{Other PiLLoW releases}
   This code is is based on the original PiLLoW library
   @cite{pillow-ws-dist,pillow-tplp}), which can be downloaded from
   @href{http://clip.dia.fi.upm.es/Software/pillow/pillow.html}. There
   is also a @index{PiLLoW on-line tutorial} (slides) at
   @href{http://clip.dia.fi.upm.es/logalg/slides/C_pillow/C_pillow.pdf}
   which illustrates the basic features and provides a number of
   examples of PiLLoW use.
").

:- use_package(library(pillow)).
