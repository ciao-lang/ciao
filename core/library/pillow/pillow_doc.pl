:- use_package([assertions]).
:- doc(nodoc,assertions).

:- doc(title, "The PiLLoW Web programming library").
:- doc(subtitle_extra,"REFERENCE MANUAL").
:- doc(subtitle_extra,"@em{Generated/Printed on:} @today{}").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Manuel Hermenegildo").

:- include(ciao_docsrc(common/'ClipAddress')).

:- doc(copyright,"Copyright @copyright{} Daniel Cabeza and Manuel Hermenegildo

@include{DocCopyright.lpdoc}

").

:- doc(summary, "@include{pillow/pillow_summ.lpdoc}").

:- doc(module,"@cindex{WWW, interfacing with}
   @cindex{XML}@cindex{CGI}@cindex{HTML}@cindex{HTTP} This package
   implements the PiLLoW library @cite{pillow-ws-dist-short}.  The
   following three chapters document, respectively, the predicates for
   HTML/XML/CGI programming, the predicate for HTTP conectivity, and
   the types used in the definition of the predicates (key for fully
   understanding the other predicates).  You can find a paper and some
   additional information in the @file{library/pillow/doc} directory
   of the distribution, and in the WWW at
   @href{http://clip.dia.fi.upm.es/Software/pillow/pillow.html}. There
   is also a @index{PiLLoW on-line tutorial} (slides) at
   @href{http://clip.dia.fi.upm.es/logalg/slides/C_pillow/C_pillow.html} which
   illustrates the basic features and provides a number of examples of
   PiLLoW use.

@section{Installing PiLLoW}
@include{INSTALL_pillow.lpdoc}

").

:- use_package(library(pillow)).
