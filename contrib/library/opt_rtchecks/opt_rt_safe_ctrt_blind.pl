:- package(opt_rt_safe_ctrt_blind).
% Flags for the 'safe_ctrt_blind' mode
%
% (Does not use ctrt transformation)

:- use_package(pp).
:- pp_cmd(use_module(ciaopp(preprocess_flags),[set_pp_flag/2])).
%:- pp_cmd(use_module(ciaopp(tr_ctrt))). % enable ctrt transform
:- pp_cmd(set_pp_flag(client_safe_ctchecks, on)).
%:- pp_cmd(transform(ctrt)).
:- pp_cmd(analyze(shfr)).
:- pp_cmd(analyze(eterms)).
:- pp_cmd(acheck).
:- pp_opt(rtchecks).
:- pp_opt(output).

:- pp_post([
   % Options for rtchecks
   % TODO: loc are expensive at this moment (implement locator keys, etc.)
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

:- compilation_fact(export_wrap).

