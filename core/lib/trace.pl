:- package(trace).
% Enable embedded debugger in 'trace' mode (for the module that uses this package)

?- error_in_lns(_,_,note, 'Using trace package').

:- use_package(library(debugger/embedded)).
% TODO: Not very nice, avoid this use of initialization!
:- initialization((this_module(M), debug_module(M), trace)).
