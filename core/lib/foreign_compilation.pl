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

% NOTE: Automatically generated in build_engine.sh (eng_build_info.c)
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
	split_blanks(Opts0, Opts).

:- pred linker_and_opts(?LD, ?Opts) ::
        atm * list(atm)
 # "@var{LD} is the linker program used to link foreign code, using
   options @var{Opts}.".

linker_and_opts(LD, Opts):-
        foreign_opts_ld(LD),
	foreign_opts_ldshared(Opts0),
	split_blanks(Opts0, Opts).

% TODO: Move somewhere else?

% Split blanks (e.g. 'a b c' -> ['a','b','c'])
split_blanks(X, Ys) :-
	atom_codes(X, Cs),
	split(Cs, Zs),
	atom_codes_list(Ys, Zs).

split(Cs, Ys) :-
	skip_blanks(Cs, Cs2),
	( Cs2 = [] -> Ys = []
	; split_(Cs2, As, As, Ys)
	).

split_([], As, [], [As]) :- !. % close As, end
split_([0' |Xs], As, [], [As|Zs]) :- !, split(Xs, Zs). % close As, continue
split_([X|Xs], As, [X|As0], Zs) :- split_(Xs, As, As0, Zs). % accum in As

skip_blanks([0' |Xs], Ys) :- !, skip_blanks(Xs, Ys).
skip_blanks(Xs, Xs).

atom_codes_list([], []).
atom_codes_list([X|Xs], [Cs|Css]) :-
	atom_codes(X, Cs),
	atom_codes_list(Xs, Css).
