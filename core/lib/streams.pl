:- module(streams, [], [assertions]).

:- doc(title, "Stream handling and operations").

:- doc(author, "The Ciao Development Team").

:- doc(module, "This module reexports the predicates required for
   stream handling (@lib{stream_basic}) and input/output operations
   (@lib{io_basic}). See the documentation of the reexported modules
   for further details.").

:- reexport(engine(stream_basic)).
:- reexport(engine(io_basic)).
