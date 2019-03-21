:- module(_, [], [assertions, fsyntax, hiord, dcg]).

:- doc(title,  "Basic support for CMake").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module provides some very basic functionality to
   interface with the @href{https://cmake.org}{CMake} build automation
   system from the Ciao @app{builder}").

:- use_module(library(process), [process_call/3]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(library(system_extra), [mkpath/1]).

:- use_module(ciaobld(eng_defs), [bld_cmake_path/3]).

:- export(b_cmake/4).
% Build RelSrc as Name under build/ (under cmake/)
b_cmake(Bundle, RelSrc, Name, Env) :-
	SrcDir = ~bundle_path(Bundle, RelSrc),
	bld_cmake_path(Bundle, Name, BuildDir),
	mkpath(BuildDir),
	do_cmake(SrcDir, BuildDir, Env).

% Call cmake on SrcDir producing binaries on BuildDir.
% Passes Env as program environment while cmake is called.
do_cmake(SrcDir, BuildDir, Env) :-
	process_call(path(cmake), [SrcDir], [env(Env), cwd(BuildDir), status(0)]),
	process_call(path(cmake), ['--build', '.'], [cwd(BuildDir), status(0)]).

