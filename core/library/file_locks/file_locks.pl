:- module(file_locks, [lock_file/3, unlock_file/2], [assertions]).

:- doc(title,"File locks").
:- doc(author, "Jos@'{e} Manuel G@'{o}mez P@'{e}rez").
:- doc(author, "Daniel Cabeza").
:- doc(author, "Manuel Carro").

:- doc(module,"This module implements file locks: the ability to lock
   a file so that other processes cannot access it until the file is
   unlocked.

   @begin{alert}
   NOT IMPLEMENTED. Consider locking in @pred{open/3}.
   @end{alert}
   ").

:- pred lock_file(File, LockType, Result) :: atm * atm * atm # "Tries
        to lock @var{File} with @var{LockType} and returns the result
          (either @tt{true} or @tt{false}) in @var{Result}.".
lock_file(_, _, _).

:- pred unlock_file(File, Result) :: atm * atm # "Tries
        to unlock @var{File} the result
          (either @tt{true} or @tt{false}) in @var{Result}.".
unlock_file(_,_).

:- doc(bug, "file locks are not currently implemented.").
