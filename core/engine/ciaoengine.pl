:- module(ciaoengine, [], [assertions, library(compiler/emugen)]).

:- doc(title, "The Ciao Engine").
:- doc(author, "Ciao developers").

:- doc(module, "An efficient, high-performance, bytecode-based engine,
   with garbage collection, unbound precision integer arithmetic,
   built-in concurrency capabilities, and many other features.

   This is the program that loads and runs Ciao bytecode binaries.").

:- doc(usage, "The command @tt{ciao build core.engine} automatically
   builds the engine binary, including auto-generated engine C
   files. Do not compile this module directly.").

:- include(engine(ciaoengine_common)).
:- engine_stubmain('eng_main.c').

