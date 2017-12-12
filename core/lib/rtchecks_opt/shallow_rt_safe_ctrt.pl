:- package(shallow_rt_safe_ctrt).
% Flags for the 'safe_ctrt' mode and shallow run-time checking

:- use_package(library(assertions/pp)).
:- pp_cmd(use_module(ciaopp(preprocess_flags),[set_pp_flag/2])).
:- pp_cmd(use_module(ciaopp(tr_ctrt))). % enable ctrt transform
:- pp_cmd(set_pp_flag(client_safe_ctchecks, on)).
:- pp_cmd(transform(ctrt)).
:- pp_cmd(analyze(shfr)).
:- pp_cmd(analyze(eterms)).
:- pp_cmd(acheck).
:- pp_opt(rtchecks_shallow).
:- pp_opt(output).

:- pp_pre([
        (:- use_package(termhide)), % Do termhide expansion only on ciaopp side (the output will be expanded)
         (:- new_declaration(hide/1))
]).

% :- pp_post([
%    % Options for rtchecks
%    % (:- set_prolog_flag(rtchecks_level,inner)) % DEPRECATED
% ]).
