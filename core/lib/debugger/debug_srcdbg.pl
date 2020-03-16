:- package(debug_srcdbg).
% Internal package for srcdbg debugging (see toplevel_debugger.pl)

% TODO: requires defined(optim_comp), merge?

:- use_module(engine(debugger_support), [srcdbg_spy/7]).
% TODO: 'dynamic' is no longer needed
% :- '$default_preddef'(dynamic).

:- multifile '$mod_srcdbg'/1.
:- '$preddef'('$mod_srcdbg'/1, bytecode).

:- '$insert_debug_info'.
