:- package(condcomp).
% (this package is built-in in Ciao)
:- if('$with_compiler_name'(c_optim_comp)).
:- compilation_fact(optim_comp).
:- endif.

