:- module(foreign_compilation,
    [
        compiler_and_opts/2,
        linker_and_opts/2
    ],
    [assertions, isomodes]).

:- doc(title,"Utilities for on-demand compilation of foreign files").

:- doc(author,"Manuel Carro").
:- doc(author,"Jose F. Morales").

:- doc(module, "This module provides two predicates which give the user
   information regarding how to compile external (C) files in order
   to link them with the Ciao engine at runtime.

   These predicates are not intended to be called directly by the end-user.  
   Instead,  a tool or module whose aim is generating dynamically 
   loadable files from source files should use the predicates in this file 
   in order to find out what are the proper compiler and linker to use, 
   and which options must be passed to them in the current architecture.").

:- use_module(library(parse_shell_args), [parse_shell_args/2]).

% NOTE: Automatically generated in build_car.sh (eng_build_info.c)
:- impl_defined(foreign_opts_cc/1).
:- impl_defined(foreign_opts_ld/1).
:- impl_defined(foreign_opts_ccshared/1).
:- impl_defined(foreign_opts_ldshared/1).

:- pred compiler_and_opts(?Compiler, ?Opts) ::
    atm * list(atm)
 # "@var{CC} is the C compiler used to compile foreign code (including
   gluecode), using options @var{Opts}.".

compiler_and_opts(CC, Opts):-
    foreign_opts_cc(CC),
    foreign_opts_ccshared(Opts0),
    parse_shell_args(Opts0, Opts).

:- pred linker_and_opts(?LD, ?Opts) ::
    atm * list(atm)
 # "@var{LD} is the linker program used to link foreign code, using
   options @var{Opts}.".

linker_and_opts(LD, Opts):-
    foreign_opts_ld(LD),
    foreign_opts_ldshared(Opts0),
    parse_shell_args(Opts0, Opts).

