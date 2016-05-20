:- package(opt_rt_safe_rt).
% Flags for the 'safe_rt' mode

:- use_package(pp).
:- pp_opt(rtchecks).
:- pp_opt(output).

:- pp_post([
   % Options for rtchecks
   (:- set_prolog_flag(rtchecks_asrloc,no)),
   (:- set_prolog_flag(rtchecks_predloc,no)),
   (:- set_prolog_flag(rtchecks_callloc,no)),
   % since there are both 'entry' (needed for ct analysis) and 'pred'
   % (needed for rt checks) assertions for the exported predicate(s),
   % this flag should be disabled to avoid checks duplication (given
   % 'entry' and 'pred' are exactly the same)
   (:- set_prolog_flag(rtchecks_entry,no)),
   (:- set_prolog_flag(rtchecks_trust,no))
]).


