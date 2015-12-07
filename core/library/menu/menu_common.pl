:- use_package(argnames).

:- argnames menu_opt(menu, flag, message, guard, pre, post).
:- multifile menu_default/3.
:- multifile menu_opt/6.
:- meta_predicate menu_opt(?, ?, ?, pred(1), pred(0), pred(2)).

% HOOKS

:- multifile hook_menu_flag_values/3.
:- multifile hook_menu_check_flag_value/3.
:- multifile hook_menu_flag_help/3.
:- multifile hook_menu_default_option/3.
