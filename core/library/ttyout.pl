:- module(ttyout, [
        ttyget/1, ttyget1/1, ttynl/0, ttyput/1, ttyskip/1, ttytab/1,
        ttyflush/0, ttydisplay/1, ttydisplayq/1, ttyskipeol/0,
        ttydisplay_string/1
        ],[assertions]).

:- doc(title, "C-Prolog terminal I/O").
:- doc(author, "The Ciao Development Team").

:- doc(module, "This module provides emulation of C-Prolog terminal I/O.").

:- use_module(engine(stream_basic), [flush_output/1]).
:- use_module(engine(io_basic)).

:- pred ttyput(X) : int + (native, deprecated).

ttyput(X) :- put_code(user, X).

ttytab(X) :- tab(user, X).

ttyskip(X) :- skip_code(user, X).

:- true comp ttyflush + (native, deprecated).

ttyflush :- flush_output(user).


:- pred ttyget(X) => int + (native, deprecated).

ttyget(N) :- get_code(user, N).

ttyget1(N) :- get1_code(user, N).

:- true comp ttynl + (native, deprecated).

ttynl :- nl(user).

:- true comp ttyskipeol + deprecated.

ttyskipeol :- skip_code(user, 0'\n).

:- true comp ttydisplay(_) + deprecated.

ttydisplay(X) :- display(user, X).

:- true comp ttydisplayq(_) + deprecated.

ttydisplayq(X) :- displayq(user, X).

:- true comp ttydisplay_string(_) + deprecated.

ttydisplay_string([]).
ttydisplay_string([X|Xs]) :-
        put_code(user, X),
        ttydisplay_string(Xs).
