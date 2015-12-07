:- module(ttyout, [
        ttyget/1, ttyget1/1, ttynl/0, ttyput/1, ttyskip/1, ttytab/1,
        ttyflush/0, ttydisplay/1, ttydisplayq/1, ttyskipeol/0,
        ttydisplay_string/1
        ],[assertions]).


:- pred ttyput(X) : int + native.

ttyput(X) :- put_code(user, X).

ttytab(X) :- tab(user, X).

ttyskip(X) :- skip_code(user, X).

:- true comp ttyflush + native.

ttyflush :- flush_output(user).


:- pred ttyget(X) => int  + native.

ttyget(N) :- get_code(user, N).

ttyget1(N) :- get1_code(user, N).

:- true comp ttynl + native.

ttynl :- nl(user).

ttyskipeol :- skip_code(user, 0'
                           ).
ttydisplay(X) :- display(user, X).

ttydisplayq(X) :- displayq(user, X).

ttydisplay_string([]).
ttydisplay_string([X|Xs]) :-
        put_code(user, X),
        ttydisplay_string(Xs).
