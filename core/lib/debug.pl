:- package(debug).
% Enable embedded debugger in 'debug' mode (for the module that uses this package)

?- error_in_lns(_,_,note, 'Using debug package').

:- use_package(library(debugger/embedded)).
% TODO: Not very nice, avoid this use of initialization!
:- initialization((this_module(M), debug_module(M), debug)).

