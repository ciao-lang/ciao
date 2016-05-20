:- package(opt_rt_client_safe).
% Flags for the 'client_safe' mode

:- use_package(pp).
:- pp_opt(rtchecks).
:- pp_opt(output).

:- pp_post([
   % Options for rtchecks
   (:- set_prolog_flag(rtchecks_asrloc,no)),
   (:- set_prolog_flag(rtchecks_predloc,no)),
   (:- set_prolog_flag(rtchecks_callloc,no)),
   %
   (:- set_prolog_flag(rtchecks_level,exports))
   %(:- set_prolog_flag(rtchecks_trust,no))
]).

:- compilation_fact(export_wrap).

