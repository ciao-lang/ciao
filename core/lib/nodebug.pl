:- package(nodebug).
% Enable embedded debugger in 'nodebug' mode (for all modules)

:- use_package(library(debugger/embedded)).
% TODO: Not very nice, avoid this use of initialization!
:- initialization(nodebug).
