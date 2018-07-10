% Enable infix dot notation
:- op(40, yfx, [(.)]).
:- set_prolog_flag(read_infix_dot, on).

:- op(800, xfx, [(as)]). % between \+ and =, :, etc.

