% (included file)

% Hooks for compact_assrt/2
% (e.g., used in ciaopp_cost)
% TODO: review, why not succ too?
:- multifile hook_compact_global_prop/2.
:- multifile hook_compact_calls_prop/2.

% Hooks for p_printer
:- multifile hook_pp_info_clause/3.
:- multifile hook_pp_info_lit/4.

% Hooks for regtypes
:- multifile hook_legal_regtype/1.
:- multifile hook_insert_regtype/2.
:- multifile hook_post_init_regtypes/0.
:- multifile hook_cleanup_lib_regtypes/0.
:- multifile hook_restore_lib_regtypes/1.
:- multifile hook_save_lib_regtypes/1.
