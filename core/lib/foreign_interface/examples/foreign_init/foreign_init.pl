:- module(foreign_init, [print_time/0, init_p/0], [foreign_interface]).

:- trust pred init :: true + foreign(init).
:- trust pred print_time :: true + foreign(print_time).

:- use_foreign_source(foreign).

init_p :- init.

:- initialization(init_p).
