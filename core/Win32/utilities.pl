:- module(utilities, [setup_mess/1, line/0], []).

:- use_module(engine(io_basic)).
:- use_module(engine(messages_basic), [display_list/1]).

% --------------------------------------------------------------------------
% Utilities
% --------------------------------------------------------------------------

setup_mess(M) :-
    line,
    display_list(['* '|M]),
    flush_output.

line :- 
    display(
'--------------------------------------------------------------------------\n'
           ).
