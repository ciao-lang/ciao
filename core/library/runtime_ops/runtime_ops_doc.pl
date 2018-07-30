:- use_package([assertions]).
:- doc(nodoc, assertions).

:- doc(title, "Enabling operators at run-time").

:- doc(author, "Daniel Cabeza").

:- doc(module, "This library package allows the use of the statically
   defined operators of a module for the reading performed at run-time
   by the program that uses the module. Simply by using this package the 
   operator definitions appearing in the module are enabled during the 
   execution of the program.

   @begin{alert}
   @bf{Use with care:} this is a non-modular extension. It may alter
   the behaviour of other modules (performing read/write operations)
   not using the package. This may be improved in future versions of
   the package (see bugs entries below).
   @end{alert}
   ").

:- doc(bug, "Consider implementing local runtime operator tables and
   @tt{addmodule} versions of read/write predicates").

:- use_package(runtime_ops).
