:- package(initial_for_ciaosh).
% Version of initial.pl for the toplevel.

% It enables the 'SHELL' condcomp fact so that other packages can be
% written conditionally.

:- use_package(condcomp).
:- compilation_fact('SHELL'). % We are in a toplevel
