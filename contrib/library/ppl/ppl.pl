:- module(ppl, _, [assertions]).  
:- doc(nodoc, assertions). 


:- doc( title, "Parma Polyhedra Library Interface").
:- doc( author, "Roberto Bagnara").
:- doc( author, "R@'{e}my Haemmerl@'{e}").

:- doc( module, " This module provides an interface with Parma
Polyhedra Library (PPL). It is a patched version of the interface
available in the source code of PPL 0.9 (Warning : It should not work
with any other version of PPL). For more information about the
predicates provided by the interface, refer to the Parma Polyhedra
Library webpage @href{http://www.cs.unipr.it/ppl/Applications/}.

If PPL is not available during the compilation of ciao, a dummy
interface will be substituted. In this case a @tt{PPL library not
installed} error will be raised at loading of the module.").

:- include(library(ppl/ppl_auto)).

