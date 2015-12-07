:- use_package([assertions, isomodes]).

:- doc(nodoc,assertions).

:- doc(filetype,package).

:- doc(title, "Call on determinate").

:- doc(author, "Jose F. Morales").
:- doc(author, "Manuel Carro").

:- doc(module, "Offers an enriched variant of call and cut
@tt{!!/0} which executes pending goals when the computation has no
more alternatives.  

This library is useful to, for example, get rid of external
connections once the necessary data has been obtained.").

:- use_package(library(det_hook)).

:- doc(appendix, "As an example, the program

@begin{verbatim}
:- module(_, _, [det_hook]).

enumerate(X):-
        display(enumerating), nl,
        OnCut = (display('goal cut'), nl),
        OnFail = (display('goal failed'), nl),
        det_try(enum(X), OnCut, OnFail).

enum(1).
enum(2).
enum(3).
@end{verbatim}

behaves as follows:

@begin{verbatim}
?- enumerate(X).
enumerating

X = 1 ? ;

X = 2 ? ;

X = 3 ? ;
goal failed
@end{verbatim}

(note the message inserted on failure).  The execution can be cut as follows:

@begin{verbatim}
?- use_package(det_hook).
@{Including /home/clip/lib/ciao/ciao-1.7/library/det_hook/det_hook.pl
@}

yes
?- enumerate(X), '!!'.
enumerating
goal cut

X = 1 ? ;

no
@end{verbatim}").

:- doc(bug, "If the started goals do not exhaust their solutions,
and '!!'/0 is not used, the database will populate with facts which
will be consulted the next time a '!!'/0 is used.  This could cause
incorrect executions.").

:- doc(usage, "@begin{verbatim}
:- use_module(library(det_hook_rt)).
@end{verbatim}
in which case, @tt{!!/0} is not available.

Typically, this library is used as a package:
@begin{verbatim}
:- use_package(det_hook).
@end{verbatim}").

/*
:- doc(doinclude, '!!'/0).
:- pred '!!' # "Performs a special cut which prunes alternatives away,
as the usual cut, but which also executes the goals specified as
@var{OnCut} for any call in the scope of the cut.".
*/
