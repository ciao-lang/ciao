:- use_package(assertions).
:- doc(nodoc,assertions).

:- doc(title,"Classic Prolog package").

:- doc(author, "The CLIP Group").

:- doc(module,"This library package allows the use of certain Prolog
   features which have become sort of 'classical' from many Prolog 
   implementations. These include definite clause grammars and some classical
   predicates like @tt{append/3}. The libraries listed below define these
   predicates, and the following chapters describe them.").

:- include(library(classic_common)).
:- use_package(runtime_ops).

