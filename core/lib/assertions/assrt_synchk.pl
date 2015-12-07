:- module(assrt_synchk,	[], [assertions]).

:- doc(title,"Simple syntax checking of code and assertions").

:- doc(module,"

@cindex{checking syntax of code}
@cindex{checking syntax of assertions}

This module defines some predicates which are useful for checking the
syntax of the code and assertions in a file, as well as imports and
exports.  Full (semantic) assertion checking must be done with the
preprocessor.

This module can be used in three ways:

@begin{itemize}

@item From the top level, by loading this module and calling the
      appropriate exported predicate.

@item From a shell, using the @tt{fileinfo} utility (see the @tt{etc}
      directory).

@item From the CIAO emacs mode, by selecting the appropriate menu
      option (or key binding).

@end{itemize}

").

%% ISO Compat
%:- use_module(library(format)).  
%:- use_module(library(aggregates)).  

%% CIAO libraries
%:- use_module(library(compiler/c_itf)).
%:- use_module(library(assertions/assrt_lib)).
