:- use_package(assertions).

:- doc(nodoc, assertions).

:- doc(filetype, package).

:- doc(author,"Pablo Chico de Guzm@'{a}n Huerta").
:- doc(author,"The CLIP Group").

:- doc(title,"Difference Constraints").

:- doc(module, "This module supports difference constraint
evaluation.").

:- doc(bug, "This library is a beta version. The constraint store is a
$N\timesN$ matrix.").

:- doc('#='/2, "Meta-constraint \"equal\".").
:- doc('#<>'/2, "Meta-constraint \"not equal\".").
:- doc('#\<'/2, "Meta-constraint \"smaller than\".").
:- doc('#=<'/2, "Meta-constraint \"smaller or equal\".").
:- doc('#>'/2, "Meta-constraint \"greater than\".").
:- doc('#>='/2, "Meta-constraint \"greater or equal\".").
