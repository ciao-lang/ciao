:- module(string_type_rt, [], [noprelude, assertions]).

:- doc(title, "Runtime for Primitive String Type").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module defines operations to manipulate strings
   as a single data type (not as lists).

   To read @tt{\"...\"} strings as native strings in your program,
   please include:

   @begin{verbatim}
      :- use_package(string_type).
   @end{verbatim}

   @textbf{Note:} Ciao built-ins does not support string types yet.

   @textbf{Note:} Supporting strings as different types has been
     discussed several times in the past. See @emph{Supporting more types in
     the WAM: the hProlog tagging scheme}, Bart Demoen, Phuong-Lan Nguyen.

     Some systems that support native strings are LPA Prolog and SWI
     Prolog.
   ").

:- use_module(engine(basiccontrol)).
:- use_module(engine(term_typing)).
:- use_module(engine(term_basic)).
:- use_module(engine(atomic_basic)).

:- doc(bug, "Improve compatibility with other systems").
:- doc(bug, "Improve implementation (use blobs, etc.)").
:- doc(bug, "Add runtime support by default?").
:- doc(bug, "Use string_codes/2 in the reader -- do not use the
   '\\6\\string' functor directly").

:- export(is_string/1).
:- pred is_string(String) # "@var{String} is a string".
is_string(String) :-
	nonvar(String), functor(String, '\6\string', 1).

:- export(string_codes/2).
:- pred string_codes(String, Codes) # "@var{Codes} are the character
   codes (integer values) of @var{String}.".
string_codes(String, Codes) :-
        functor(String, '\6\string', 1),
        arg(1, String, Codes).

:- export(atom_string/2).
:- pred atom_string(Atom, String) # "@var{String} is the string that
   represents the name of atom @var{Atom}.".
atom_string(Atom, String) :-
        atom_codes(Atom, Codes),
        functor(String, '\6\string', 1),
        arg(1, String, Codes).

