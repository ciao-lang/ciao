:- module(ciaoenginejs, [], [assertions, library(compiler/emugen)]).

:- doc(title, "The Ciao Engine (ASMJS version)").
:- doc(author, "Ciao developers").

:- doc(module, "See @lib{ciaoengine} for more details about the Ciao
   engine. This variant is intended to be compiled with Emscripten (C
   to JavaScript compiler)").

:- include(engine(ciaoengine_common)).
:- engine_stubmain('main-ciaojs.c').

:- engine_opts([
  cross('LINUX', x86_JS) % Emscripten
]).
