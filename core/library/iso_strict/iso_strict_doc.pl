:- use_package([assertions]).
:- doc(nodoc,assertions).
:- doc(nodoc,assertions_basic).

:- doc(filetype,package).

:- doc(title,"Stricter ISO-Prolog package").

:- doc(author, "The Ciao Development Team").

:- doc(module,"This package enables stricter @concept{ISO-Prolog}
   conformance by default in the modules that include it. In particular,
   it includes the modules and packages listed in the @em{Usage and
   interface} box below. Note that this package is also included when
   loading the @ref{Classic Prolog} package (either explicitly or
   implicity by using a @decl{module/2} declaration).  On the other
   hand, using this package by itself, i.e.: @tt{:- module(..., ...,
   [iso_strict]).}  @em{approximates} a strictly conforming mode, i.e., a
   mode which rejects uses of non-ISO features (except for those that
   are built-in in Ciao, such as the module system).

   Also note that library predicates that correspond to those in the
   ISO-Prolog standard are marked accordingly throughout the manuals,
   and differences between the Ciao and the prescribed ISO-Prolog
   behaviours, if any, commented appropriately.

   Strict compliance with ISO is still not complete: currently there
   are some minor deviations in, e.g., the treatment of characters,
   the syntax, some of the arithmetic functions, and part of the error
   system.  In particular, the @decl{char_conversion/2} directive is
   not implemented, since Ciao does not (yet) have a character
   conversion table.

   However, as mentioned in @ref{ISO-Prolog compliance versus
   extensibility} in the introduction, the intention of the Ciao
   developers is to progressively complete ISO standard support, as
   well as adapt to the corrigenda and reasonable extensions of the
   standard as they appear.

").

:- use_package(iso_strict).

% TODO: 

% @comment{Given that the final version of the ISO standard has only
% been recently published,}

% @comment{On the other hand, Ciao has been reported by independent
% sources (members of the standarization body) to be one of the most
% conforming Prologs at the moment of this writing, and the first one
% to be able to compile all the standard-conforming test cases.}
   
